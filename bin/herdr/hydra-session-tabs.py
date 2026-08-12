#!/usr/bin/env python3
"""Keep herdr tabs in sync with hydra's warm sessions.

One tab per warm interactive session, plus a picker tab pinned to
position 1. Add-only: it never closes a tab, so a session going cold
leaves whatever you were reading alone.

WHY THIS CAN BE A DUMB SCRIPT

The join key is published. `hydra session list --json` gives warm
sessions; `herdr pane list` gives each pane's `session` metadata token,
which is the session that pane is attached to RIGHT NOW. Set difference
of those two is the whole algorithm -- no state file, no naming
convention, nothing to drift.

The picker identifies itself for free, too. A hydra pane sitting in the
picker releases its agent and withdraws its tokens, so "runs hydra but
has no session token" means "this is the picker". That is also why the
reconciler does not mistake a picker for a session tab and open a
duplicate.

THREE THINGS THAT LOOK LIKE IMPROVEMENTS AND ARE NOT

1. Do NOT pass --label to `tab create`. hydra renames the tab to the
   session title itself, but only while it owns the label: label-sync
   refuses to overwrite a name it did not write, on the assumption a
   human chose it. Labelling the tab here permanently freezes its title.
   Create it bare and let hydra name it.

2. Do NOT drop the `exec` in the launch command. Without it the pane
   falls back to a shell when the session exits, that pane then has no
   session token, and the next pass cheerfully opens the session again.

3. Do NOT reconcile per pane. Two panes may hold one session on purpose
   (that is a supported hydra thing), so the unit of comparison is the
   set of session ids on screen, not the panes showing them.

SIDE EFFECT WORTH WATCHING

Every tab this opens is a real attached client, so `attachedClients`
climbs on sessions you are not actually reading. If hydra's cold
demotion ever keys off attached clients, running this on a timer would
keep everything warm forever. Verify before trusting the loop.
"""

from __future__ import annotations

import argparse
import json
import os
import socket
import subprocess
import sys
import time

SOCKET_ENV = "HERDR_SOCKET_PATH"
DEFAULT_SOCKET = "~/.config/herdr/herdr.sock"
# herdr runs `type = "shell"` keybindings detached with stdin, stdout AND
# stderr pointed at /dev/null, so a pass triggered by a keypress is
# otherwise undiagnosable: it either worked or it did not, with no output
# and no exit status anywhere. Everything printed also lands here.
LOG_PATH = os.path.expanduser("~/.cache/hydra-session-tabs.log")
LOG_MAX_BYTES = 256 * 1024


def log(line: str) -> None:
    try:
        os.makedirs(os.path.dirname(LOG_PATH), exist_ok=True)
        if os.path.exists(LOG_PATH) and os.path.getsize(LOG_PATH) > LOG_MAX_BYTES:
            os.replace(LOG_PATH, LOG_PATH + ".1")
        stamp = time.strftime("%Y-%m-%d %H:%M:%S")
        with open(LOG_PATH, "a", encoding="utf-8") as fh:
            fh.write(f"{stamp} [{os.getpid()}] {line}\n")
    except Exception:
        pass


def say(line: str) -> None:
    """Print for a human at a terminal, and log for a keypress."""
    print(line)
    log(line)


def socket_path() -> str:
    return os.environ.get(SOCKET_ENV) or os.path.expanduser(DEFAULT_SOCKET)


def call(method: str, params: dict | None = None) -> dict:
    """One herdr socket request. The server closes after each response."""
    s = socket.socket(socket.AF_UNIX)
    s.settimeout(10)
    s.connect(socket_path())
    s.sendall(json.dumps({"id": "session-tabs", "method": method, "params": params or {}}).encode() + b"\n")
    buf = b""
    while b"\n" not in buf:
        chunk = s.recv(1 << 16)
        if not chunk:
            break
        buf += chunk
    s.close()
    resp = json.loads(buf.split(b"\n")[0])
    if "error" in resp:
        raise RuntimeError(f"{method}: {resp['error']}")
    return resp["result"]


def notify(title: str, body: str = "") -> None:
    """Surface a summary. Bound to a key, this script runs as a herdr
    `type = "shell"` command, which nulls stdout -- without a toast a
    keypress would look like it did nothing."""
    params: dict = {"title": title}
    if body:
        params["body"] = body
    try:
        call("notification.show", params)
    except Exception:
        pass


def hydra_sessions() -> list[dict]:
    # --all lifts the 20-most-recent cap on cold sessions. Without it a tab
    # whose session has aged out simply would not appear, and "not in the
    # list" is indistinguishable from "cold" -- which would make closing
    # decisions on missing evidence.
    out = subprocess.run(
        ["hydra", "session", "list", "--all", "--json"], capture_output=True, text=True, timeout=20
    )
    if out.returncode != 0:
        raise RuntimeError(f"hydra session list failed: {out.stderr.strip()}")
    return json.loads(out.stdout)


def foreground_process(pane_id: str) -> tuple[str, str]:
    """(name, cmdline) of the pane's foreground process, or ("", "")."""
    try:
        info = call("pane.process_info", {"pane_id": pane_id})
        procs = info["process_info"]["foreground_processes"]
        if not procs:
            return "", ""
        return procs[0].get("name") or "", procs[0].get("cmdline") or ""
    except Exception:
        return "", ""


def launching_session(cmdline: str) -> str | None:
    """The session id a pane was LAUNCHED for, from its argv.

    Deliberately argv, which is the wrong source for "which session is this
    pane showing" -- the TUI switches sessions in place and argv never
    changes -- but the right source for "have we already spawned a viewer
    for this session". hydra needs a second or two to attach and report its
    `session` token; without this a second pass inside that window sees the
    session as absent and opens a duplicate tab.
    """
    parts = cmdline.split()
    for i, part in enumerate(parts):
        if part == "--session" and i + 1 < len(parts):
            return parts[i + 1]
    return None


def active_workspace() -> str | None:
    workspaces = call("workspace.list")["workspaces"]
    for w in workspaces:
        if w.get("focused"):
            return w["workspace_id"]
    return workspaces[0]["workspace_id"] if workspaces else None


class Plan:
    def __init__(self) -> None:
        self.create: list[dict] = []
        self.close: list[dict] = []
        self.kept: list[str] = []           # cold, but a guard declined it
        self.picker: str | None = None      # tab id to pin at position 1
        self.make_picker = False
        self.skipped: list[str] = []
        self.sessions: dict[str, dict] = {}


def build_plan(workspace: str, args: argparse.Namespace) -> Plan:
    plan = Plan()

    sessions = hydra_sessions()
    by_id = {s["sessionId"]: s for s in sessions}
    warm = {
        sid: s
        for sid, s in by_id.items()
        if s.get("status") == "warm" and s.get("interactive")
    }

    all_panes = call("pane.list")["panes"]
    tabs = {t["tab_id"]: t for t in call("tab.list")["tabs"]}
    plan.sessions = by_id

    # WHICH PANES COUNT AS "already showing this session".
    #
    # With a managed workspace, only that workspace counts. The workspace
    # exists to be a complete index of warm sessions, so a session you
    # happen to be viewing in your own workspace must still get a tab here
    # -- otherwise the index silently omits exactly the sessions you are
    # working on. hydra supports several viewers on one session, so the
    # second one is cheap.
    #
    # Without a managed workspace the opposite is right: tabs land in
    # whatever workspace you are in, so a session visible ANYWHERE is
    # already handled and a second tab would just be clutter.
    panes = (
        [p for p in all_panes if p["workspace_id"] == workspace]
        if args.managed_workspace
        else all_panes
    )

    # On-screen detection is deliberately GLOBAL rather than scoped to the
    # target workspace: a session already open elsewhere should not get a
    # second tab here.
    onscreen: set[str] = set()
    tokenless: list[dict] = []
    for p in panes:
        token = (p.get("tokens") or {}).get("session")
        if token:
            onscreen.add(token)
        else:
            tokenless.append(p)

    # The picker is always looked for in the TARGET workspace only. It is a
    # tab we may pin to position 1 there, so a picker sitting in someone
    # else's workspace is not a candidate -- without this the plan proposes
    # pinning a tab that pin_first will correctly refuse to move, which reads
    # as a bug in the dry run.
    picker_ws = {p["pane_id"] for p in all_panes if p["workspace_id"] == workspace}

    # One process-info pass over the tokenless panes answers two questions:
    # which pane is the picker, and which sessions have a viewer still
    # starting up.
    #
    # A hydra pane with no session token is either a picker or a launch in
    # flight. Check the process rather than the agent field: a suspended pane
    # has released its agent, so `agent` is null for exactly these panes.
    for p in tokenless:
        name, cmdline = foreground_process(p["pane_id"])
        if name != "hydra":
            continue
        pending = launching_session(cmdline)
        if pending:
            onscreen.add(pending)
        elif plan.picker is None and p["pane_id"] in picker_ws:
            plan.picker = p["tab_id"]
    if plan.picker is None and args.ensure_picker:
        plan.make_picker = True

    if args.close_cold:
        plan_closes(plan, workspace, args, panes, tabs)

    missing = [(sid, s) for sid, s in warm.items() if sid not in onscreen]
    missing.sort(key=lambda kv: kv[1].get("updatedAt") or "")

    for sid, s in missing:
        if args.cwd and not (s.get("cwd") or "").startswith(args.cwd):
            plan.skipped.append(f"{sid[-16:]} (cwd)")
            continue
        if len(plan.create) >= args.limit:
            plan.skipped.append(f"{sid[-16:]} (limit)")
            continue
        plan.create.append({"sessionId": sid, "title": s.get("title") or "", "cwd": s.get("cwd")})

    _ = tabs, workspace
    return plan


def plan_closes(
    plan: Plan, workspace: str, args: argparse.Namespace, panes: list[dict], tabs: dict
) -> None:
    """Decide which tabs are safe to close.

    Closing destroys work if it is wrong, so every guard here fails CLOSED:
    anything unproven is kept. The reasons are recorded so --dry-run shows
    not just what would go but why the rest stayed.
    """
    ws_tabs = [t for t in tabs.values() if t["workspace_id"] == workspace]
    panes_by_tab: dict[str, list[dict]] = {}
    for p in panes:
        panes_by_tab.setdefault(p["tab_id"], []).append(p)

    now = time.time()

    for tab in ws_tabs:
        tab_id = tab["tab_id"]
        members = panes_by_tab.get(tab_id, [])
        label = (tab.get("label") or tab_id)[:32]

        # Identify the tab's session first, so that everything below can
        # report WHY a cold tab was spared. Structural guards checked before
        # this point would reject silently, and a dry run that prints
        # "converged" while quietly keeping a cold tab teaches the operator
        # nothing.
        session_panes = [p for p in members if (p.get("tokens") or {}).get("session")]
        if not session_panes:
            # A shell, the picker, or a TUI still starting: not ours to judge.
            continue
        pane = session_panes[0]
        sid = (pane.get("tokens") or {}).get("session")

        session = plan.sessions.get(sid)
        if session is None:
            # Should not happen with --all, but an unknown session is not
            # evidence of a cold one.
            plan.kept.append(f"{label} (session unknown)")
            continue
        if session.get("status") == "warm":
            continue

        # One pane only. A split tab may hold a shell or a second session
        # beside the cold one, and closing the tab takes those with it.
        if len(members) != 1:
            plan.kept.append(f"{label} ({len(members)} panes)")
            continue
        if pane.get("focused"):
            plan.kept.append(f"{label} (focused)")
            continue
        if len(ws_tabs) - len(plan.close) <= 1:
            # Closing a workspace's last tab closes the workspace.
            plan.kept.append(f"{label} (last tab)")
            continue

        # Grace period, derived from the session's own updatedAt rather than
        # state we would have to persist. A session that flips cold and warm
        # again would otherwise have its tab closed and reopened, losing
        # scroll position for nothing.
        age = now - iso_to_epoch(session.get("updatedAt"))
        if age < args.cold_grace:
            plan.kept.append(f"{label} (cold {int(age)}s < {int(args.cold_grace)}s)")
            continue

        plan.close.append({"tab_id": tab_id, "label": label, "sessionId": sid})


def iso_to_epoch(value: str | None) -> float:
    if not value:
        return 0.0
    try:
        from datetime import datetime

        return datetime.fromisoformat(value.replace("Z", "+00:00")).timestamp()
    except Exception:
        return 0.0


def resolve_managed_workspace(
    label: str, dry_run: bool
) -> tuple[str | None, bool, str | None]:
    """Find, or create, the workspace this script is allowed to manage.

    Identification is by LABEL, not by a metadata marker: workspace tokens
    are not written to herdr's session snapshot, so a token would vanish on
    the next server restart and we would build a second workspace beside the
    first. The tradeoff is that renaming the workspace orphans it -- this
    script would then create a fresh one and leave the old tabs alone.

    Creating a workspace also creates its first tab and root pane, so the
    picker goes straight into that pane. Leaving it as a shell instead would
    make the picker check below miss it and add a second tab.
    """
    for w in call("workspace.list")["workspaces"]:
        if (w.get("label") or "") == label:
            return w["workspace_id"], False, None
    if dry_run:
        return None, True, None
    result = call(
        "workspace.create",
        {"label": label, "focus": False, "cwd": os.path.expanduser("~")},
    )
    workspace = result["workspace"]["workspace_id"]
    pane = result["root_pane"]["pane_id"]
    call("pane.send_input", {"pane_id": pane, "text": "exec hydra", "keys": ["Enter"]})
    # Hand the tab id back. The picker detection in build_plan cannot find
    # this pane yet -- its shell has not exec'd hydra -- so without this the
    # pin and the focus have nothing to act on and quietly do nothing.
    return workspace, True, result["tab"]["tab_id"]


def await_session_tokens(pending: dict[str, str], timeout: float) -> list[str]:
    """Block until each freshly launched pane reports its session token.

    Without this the pass finishes while its panes are still starting, and
    the NEXT pass -- a second keypress, or the next tick of a loop -- sees
    those sessions as absent and opens duplicates.

    An argv check is not enough on its own. A launched pane goes through two
    blind windows, not one:

      1. the shell has not yet exec'd hydra   -> argv is "/bin/sh", nothing to read
      2. hydra is starting, before it reports -> argv has "--session X"

    The argv guard closes (2). Only waiting closes (1), and waiting also
    subsumes (2), so this is the load-bearing half. Returns the session ids
    that never showed up, which are worth surfacing rather than hiding: a
    launch that failed leaves a shell sitting in a tab.
    """
    deadline = time.time() + timeout
    outstanding = dict(pending)
    while outstanding and time.time() < deadline:
        time.sleep(0.2)
        for sid, pane_id in list(outstanding.items()):
            try:
                pane = call("pane.get", {"pane_id": pane_id})["pane"]
            except Exception:
                continue
            if (pane.get("tokens") or {}).get("session"):
                outstanding.pop(sid, None)
    return list(outstanding)


def open_session_tab(workspace: str, session_id: str, cwd: str | None) -> str:
    # No label: hydra renames the tab to the session title, but only while
    # it still owns the label. See the header.
    params: dict = {"workspace_id": workspace, "focus": False}
    if cwd:
        params["cwd"] = cwd
    result = call("tab.create", params)
    pane_id = result["root_pane"]["pane_id"]
    # exec so the pane dies with the session instead of falling back to a
    # shell that the next pass would read as "session not open".
    call(
        "pane.send_input",
        {"pane_id": pane_id, "text": f"exec hydra tui --session {session_id}", "keys": ["Enter"]},
    )
    return pane_id


def open_picker_tab(workspace: str) -> str:
    result = call("tab.create", {"workspace_id": workspace, "focus": False})
    pane_id = result["root_pane"]["pane_id"]
    call("pane.send_input", {"pane_id": pane_id, "text": "exec hydra", "keys": ["Enter"]})
    return result["tab"]["tab_id"]


def pin_first(workspace: str, tab_id: str) -> bool:
    """Move tab_id to position 1 if it is not already there."""
    tabs = [t for t in call("tab.list")["tabs"] if t["workspace_id"] == workspace]
    order = [t["tab_id"] for t in tabs]
    if not order or order[0] == tab_id or tab_id not in order:
        return False
    call("tab.move", {"tab_id": tab_id, "insert_index": 0})
    return True


def reconcile(args: argparse.Namespace) -> None:
    fresh = False
    fresh_picker_tab: str | None = None
    if args.managed_workspace:
        # Confine every tab we create to one workspace, so pressing the key
        # from your own project workspace cannot fill it with sessions.
        workspace, fresh, fresh_picker_tab = resolve_managed_workspace(
            args.managed_workspace, args.dry_run
        )
        if workspace is None:
            say(f"would create workspace {args.managed_workspace!r} with a picker tab")
            return
        if fresh:
            say(f"created workspace {args.managed_workspace!r} with a picker tab")
    else:
        workspace = (
            args.workspace or os.environ.get("HERDR_ACTIVE_WORKSPACE_ID") or active_workspace()
        )
    if not workspace:
        say("no workspace", file=sys.stderr)
        return

    plan = build_plan(workspace, args)
    if fresh:
        # The root pane is becoming the picker, but its `hydra` process has
        # not started yet, so the detection in build_plan cannot see it.
        plan.make_picker = False
        plan.picker = fresh_picker_tab
    tag = "would " if args.dry_run else ""

    if plan.make_picker:
        say(f"{tag}create picker tab")
        if not args.dry_run:
            plan.picker = open_picker_tab(workspace)

    launched: dict[str, str] = {}
    for item in plan.create:
        say(f"{tag}open {item['sessionId'][-16:]}  {item['title'][:44]}")
        if not args.dry_run:
            launched[item["sessionId"]] = open_session_tab(
                workspace, item["sessionId"], item["cwd"]
            )

    if launched:
        # Polled together rather than one at a time, so opening five tabs
        # costs one startup wait and not five.
        stalled = await_session_tokens(launched, args.launch_timeout)
        for sid in stalled:
            say(f"  {sid[-16:]} did not report a session within {args.launch_timeout}s")

    for item in plan.close:
        say(f"{tag}close {item['sessionId'][-16:]}  {item['label']}")
        if not args.dry_run:
            try:
                call("tab.close", {"tab_id": item["tab_id"]})
            except Exception as err:
                say(f"  close failed: {err}", file=sys.stderr)

    for k in plan.kept:
        say(f"keep  {k}")

    if args.focus_picker and plan.picker and not args.dry_run:
        # tab.focus crosses workspaces (switch_workspace_tab), so this is the
        # whole jump. Deliberately opt-in: a background loop stealing focus
        # mid-keystroke would be hostile.
        try:
            call("tab.focus", {"tab_id": plan.picker})
        except Exception as err:
            say(f"  focus failed: {err}", file=sys.stderr)
    elif args.focus_picker and args.dry_run and plan.picker:
        say(f"would focus picker tab {plan.picker}")

    if plan.picker:
        if args.dry_run:
            tabs = [t["tab_id"] for t in call("tab.list")["tabs"] if t["workspace_id"] == workspace]
            if tabs and tabs[0] != plan.picker:
                say(f"would pin picker tab {plan.picker} to position 1")
        elif pin_first(workspace, plan.picker):
            say(f"pinned picker tab {plan.picker} to position 1")

    for s in plan.skipped:
        say(f"skip {s}")

    if not (plan.create or plan.close or plan.make_picker or plan.skipped):
        say("converged")

    if args.dry_run or args.quiet:
        return
    # Only on change: a loop that toasted every quiet pass would be
    # unusable, and a keypress that changed nothing is self-evident from
    # the tabs not moving.
    if plan.create or plan.close or plan.make_picker:
        opened = len(plan.create) + (1 if plan.make_picker else 0)
        bits = []
        if opened:
            bits.append(f"opened {opened}")
        if plan.close:
            bits.append(f"closed {len(plan.close)}")
        titles = ", ".join(i["title"][:24] or i["sessionId"][-8:] for i in plan.create[:3])
        notify(
            "Session tabs: " + ", ".join(bits),
            titles + (f" (+{len(plan.skipped)} skipped)" if plan.skipped else ""),
        )


def focus_only(args: argparse.Namespace) -> int:
    """Jump to the picker without reconciling anything.

    Same detection as a full pass, minus every write: no workspace is
    created, no tab opened or closed. Bound to its own key so "show me the
    picker" costs nothing and cannot be surprising.
    """
    if args.managed_workspace:
        workspace, fresh, fresh_tab = resolve_managed_workspace(args.managed_workspace, True)
        if workspace is None:
            say(f"no {args.managed_workspace!r} workspace yet -- press the sync key first")
            return 1
        _ = fresh, fresh_tab
    else:
        workspace = (
            args.workspace or os.environ.get("HERDR_ACTIVE_WORKSPACE_ID") or active_workspace()
        )
    if not workspace:
        say("no workspace")
        return 1

    for p in call("pane.list")["panes"]:
        if p["workspace_id"] != workspace:
            continue
        if (p.get("tokens") or {}).get("session"):
            continue
        name, cmdline = foreground_process(p["pane_id"])
        if name == "hydra" and not launching_session(cmdline):
            call("tab.focus", {"tab_id": p["tab_id"]})
            say(f"focused picker {p['tab_id']}")
            return 0
    # No picker: the usual reason is that the last one consumed itself.
    # Picking a session attaches it IN the picker pane, so that tab becomes
    # the session's tab and the picker is gone. "Jump to the picker" should
    # still land you on one, so make it -- this is the only write here, and
    # it adds nothing you did not ask for.
    tab_id = open_picker_tab(workspace)
    # Pin it exactly as a full sync pass would. A picker that lands wherever
    # tab.create happened to put it means "the picker" is at position 1 some
    # days and position 4 on others, which defeats the muscle memory the
    # pinning exists to create.
    pin_first(workspace, tab_id)
    call("tab.focus", {"tab_id": tab_id})
    say(f"no picker existed; created at position 1 and focused {tab_id}")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description="Mirror hydra warm sessions into herdr tabs.")
    ap.add_argument("--workspace", help="target workspace id (default: focused)")
    ap.add_argument(
        "--managed-workspace",
        metavar="LABEL",
        help="confine session tabs to the workspace with this label, creating it if absent",
    )
    ap.add_argument("--interval", type=float, default=0, help="seconds between passes; 0 = run once")
    ap.add_argument("--dry-run", action="store_true", help="print decisions, change nothing")
    ap.add_argument("--ensure-picker", action="store_true", help="create a picker tab when none exists")
    ap.add_argument("--limit", type=int, default=8, help="max tabs to open per pass")
    ap.add_argument("--cwd", help="only sessions whose cwd starts with this path")
    ap.add_argument("--quiet", action="store_true", help="no toast on change")
    ap.add_argument(
        "--focus-picker",
        action="store_true",
        help="jump to the picker tab when the pass finishes",
    )
    ap.add_argument(
        "--focus-only",
        action="store_true",
        help="just jump to the picker; reconcile nothing",
    )
    ap.add_argument(
        "--launch-timeout",
        type=float,
        default=10.0,
        help="seconds to wait for a launched pane to report its session",
    )
    ap.add_argument(
        "--close-cold",
        action="store_true",
        help="also close tabs whose session has gone cold (see the guards in plan_closes)",
    )
    ap.add_argument(
        "--cold-grace",
        type=float,
        default=60.0,
        help="seconds a session must have been quiet before its tab may be closed",
    )
    args = ap.parse_args()
    log(f"invoked: {' '.join(sys.argv[1:])}")

    if args.focus_only:
        try:
            return focus_only(args)
        except Exception as err:
            log(f"focus-only failed: {err!r}")
            raise

    if args.interval <= 0:
        try:
            reconcile(args)
        except Exception as err:
            log(f"pass failed: {err!r}")
            raise
        return 0

    while True:
        try:
            reconcile(args)
        except Exception as err:  # keep the loop alive across daemon restarts
            print(f"pass failed: {err}", file=sys.stderr)
        time.sleep(args.interval)


if __name__ == "__main__":
    sys.exit(main())

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
    out = subprocess.run(
        ["hydra", "session", "list", "--json"], capture_output=True, text=True, timeout=20
    )
    if out.returncode != 0:
        raise RuntimeError(f"hydra session list failed: {out.stderr.strip()}")
    return json.loads(out.stdout)


def foreground_name(pane_id: str) -> str:
    try:
        info = call("pane.process_info", {"pane_id": pane_id})
        procs = info["process_info"]["foreground_processes"]
        return procs[0]["name"] if procs else ""
    except Exception:
        return ""


def active_workspace() -> str | None:
    workspaces = call("workspace.list")["workspaces"]
    for w in workspaces:
        if w.get("focused"):
            return w["workspace_id"]
    return workspaces[0]["workspace_id"] if workspaces else None


class Plan:
    def __init__(self) -> None:
        self.create: list[dict] = []
        self.picker: str | None = None      # tab id to pin at position 1
        self.make_picker = False
        self.skipped: list[str] = []


def build_plan(workspace: str, args: argparse.Namespace) -> Plan:
    plan = Plan()

    warm = {
        s["sessionId"]: s
        for s in hydra_sessions()
        if s.get("status") == "warm" and s.get("interactive")
    }

    panes = call("pane.list")["panes"]
    tabs = {t["tab_id"]: t for t in call("tab.list")["tabs"]}

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

    # A hydra pane with no session token is a picker. Check the process
    # rather than the agent field: a suspended pane has released its agent,
    # so `agent` is null for exactly the panes we care about here.
    for p in tokenless:
        if foreground_name(p["pane_id"]) == "hydra":
            plan.picker = p["tab_id"]
            break
    if plan.picker is None and args.ensure_picker:
        plan.make_picker = True

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
    return result["tab"]["tab_id"]


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
    workspace = args.workspace or os.environ.get("HERDR_ACTIVE_WORKSPACE_ID") or active_workspace()
    if not workspace:
        print("no workspace", file=sys.stderr)
        return

    plan = build_plan(workspace, args)
    tag = "would " if args.dry_run else ""

    if plan.make_picker:
        print(f"{tag}create picker tab")
        if not args.dry_run:
            plan.picker = open_picker_tab(workspace)

    for item in plan.create:
        print(f"{tag}open {item['sessionId'][-16:]}  {item['title'][:44]}")
        if not args.dry_run:
            open_session_tab(workspace, item["sessionId"], item["cwd"])

    if plan.picker:
        if args.dry_run:
            tabs = [t["tab_id"] for t in call("tab.list")["tabs"] if t["workspace_id"] == workspace]
            if tabs and tabs[0] != plan.picker:
                print(f"would pin picker tab {plan.picker} to position 1")
        elif pin_first(workspace, plan.picker):
            print(f"pinned picker tab {plan.picker} to position 1")

    for s in plan.skipped:
        print(f"skip {s}")

    if not (plan.create or plan.make_picker or plan.skipped):
        print("converged")

    if args.dry_run or args.quiet:
        return
    # Only on change: a loop that toasted every quiet pass would be
    # unusable, and a keypress that changed nothing is self-evident from
    # the tabs not moving.
    if plan.create or plan.make_picker:
        opened = len(plan.create) + (1 if plan.make_picker else 0)
        titles = ", ".join(i["title"][:24] or i["sessionId"][-8:] for i in plan.create[:3])
        notify(
            f"Opened {opened} session tab{'s' if opened != 1 else ''}",
            titles + (f" (+{len(plan.skipped)} skipped)" if plan.skipped else ""),
        )


def main() -> int:
    ap = argparse.ArgumentParser(description="Mirror hydra warm sessions into herdr tabs.")
    ap.add_argument("--workspace", help="target workspace id (default: focused)")
    ap.add_argument("--interval", type=float, default=0, help="seconds between passes; 0 = run once")
    ap.add_argument("--dry-run", action="store_true", help="print decisions, change nothing")
    ap.add_argument("--ensure-picker", action="store_true", help="create a picker tab when none exists")
    ap.add_argument("--limit", type=int, default=8, help="max tabs to open per pass")
    ap.add_argument("--cwd", help="only sessions whose cwd starts with this path")
    ap.add_argument("--quiet", action="store_true", help="no toast on change")
    args = ap.parse_args()

    if args.interval <= 0:
        reconcile(args)
        return 0

    while True:
        try:
            reconcile(args)
        except Exception as err:  # keep the loop alive across daemon restarts
            print(f"pass failed: {err}", file=sys.stderr)
        time.sleep(args.interval)


if __name__ == "__main__":
    sys.exit(main())

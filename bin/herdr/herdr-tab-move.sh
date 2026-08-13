#!/bin/bash
# Move the active herdr tab one position left or right.
#
# herdr port of `bind -r < swap-window -d -t -1` / `bind -r > ... +1`.
#
# There is no `herdr tab move` CLI verb, but the socket API has
# `tab.move` — one of several methods with no CLI wrapper. So this talks
# the wire protocol directly: newline-delimited JSON on the unix socket,
# one request per connection (the server closes after responding).
#
# insert_index is a GAP index with insert-before semantics, and
# Workspace::move_tab subtracts one when the tab travels rightward:
#
#     target = source < insert ? insert - 1 : insert
#
# so moving left one slot is insert_index = idx - 1, and moving right one
# slot is idx + 2, not idx + 1. Getting this wrong silently no-ops
# (move_tab returns false when source == target) while the API still
# answers with a success payload.
#
# BOTH INDICES ARE WORKSPACE-RELATIVE, and `tab.list` is not: it returns
# every tab on the server, across all workspaces, in one flat array. So the
# position of a tab within that array is NOT the index tab.move wants, and
# using it is wrong by however many tabs sit in the workspaces listed
# before ours.
#
# This was the original bug here. With a tab at global index 3 but
# workspace index 2, "left" computed insert_index = 2, which move_tab reads
# as source == target and refuses; "right" computed 5, out of bounds for a
# 4-tab workspace. Both failed invisibly -- one as a success payload that
# did nothing, one as an error printed to a stdout the keybinding discards.
# It only worked at all while the hydra workspace happened to be the first
# one listed. Filter by workspace_id before computing anything.
#
# Neither direction wraps, matching tmux: at an end, this does nothing.

set -u

case "${1:-}" in
    left|right) direction="$1" ;;
    *)
        echo "usage: herdr-tab-move.sh left|right" >&2
        exit 2
        ;;
esac

sock="${HERDR_SOCKET_PATH:-$HOME/.config/herdr/herdr.sock}"
tab="${HERDR_ACTIVE_TAB_ID:-}"

if [ ! -S "$sock" ]; then
    echo "herdr-tab-move: no socket at $sock" >&2
    exit 1
fi

python3 - "$sock" "$tab" "$direction" <<'PY'
import json, os, socket, sys

sock_path, tab_id, direction = sys.argv[1], sys.argv[2], sys.argv[3]


def call(method, params):
    s = socket.socket(socket.AF_UNIX)
    s.settimeout(5)
    s.connect(sock_path)
    s.sendall(json.dumps({"id": "tab-move", "method": method, "params": params}).encode() + b"\n")
    buf = b""
    while b"\n" not in buf:
        chunk = s.recv(65536)
        if not chunk:
            break
        buf += chunk
    s.close()
    return json.loads(buf.split(b"\n")[0])


try:
    tabs = call("tab.list", {})["result"]["tabs"]
except Exception as err:
    print(f"herdr-tab-move: tab.list failed: {err}", file=sys.stderr)
    sys.exit(1)

# HERDR_ACTIVE_TAB_ID is authoritative when present; the focused flag is
# the fallback for invocations outside a keybinding.
by_id = {t["tab_id"]: t for t in tabs}
active = by_id.get(tab_id) or next((t for t in tabs if t.get("focused")), None)
if active is None:
    print("herdr-tab-move: no active tab", file=sys.stderr)
    sys.exit(1)
tab_id = active["tab_id"]

# Only the tabs sharing this tab's workspace, in order. See the header:
# tab.move indexes within a workspace, tab.list does not.
order = [t["tab_id"] for t in tabs if t["workspace_id"] == active["workspace_id"]]
idx = order.index(tab_id)

if direction == "left":
    if idx == 0:
        sys.exit(0)
    insert_index = idx - 1
else:
    if idx >= len(order) - 1:
        sys.exit(0)
    insert_index = idx + 2

resp = call("tab.move", {"tab_id": tab_id, "insert_index": insert_index})
if "error" in resp:
    print(f"herdr-tab-move: {resp['error']}", file=sys.stderr)
    sys.exit(1)
PY

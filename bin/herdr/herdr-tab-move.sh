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
order = [t["tab_id"] for t in tabs]
if tab_id in order:
    idx = order.index(tab_id)
else:
    idx = next((i for i, t in enumerate(tabs) if t.get("focused")), -1)
    if idx < 0:
        print("herdr-tab-move: no active tab", file=sys.stderr)
        sys.exit(1)
    tab_id = order[idx]

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

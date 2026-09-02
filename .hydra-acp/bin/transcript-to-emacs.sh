#!/bin/sh
# Wired to a hydra-acp TUI hotkey via ~/.hydra-acp/config.json:
#   tui.hotkeys."ctrl-x" = { "command": "/home/smagnuson/bin/hydra-transcript-to-emacs.sh" }
#
# Env available (set by hydra-acp):
#   HYDRA_SESSION_ID, HYDRA_CWD, HYDRA_AGENT, HYDRA_BASE_URL, HYDRA_TOKEN_FILE

set -e

if [ -z "$HYDRA_SESSION_ID" ]; then
  echo "HYDRA_SESSION_ID not set" >&2
  exit 1
fi

out="$HOME/.hydra.transcript.md"
hydra-acp sessions transcript "$HYDRA_SESSION_ID" --out "$out"
emacsclient -n "$out"

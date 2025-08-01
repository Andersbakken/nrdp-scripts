#aider
if [ -e "$HOME/.local/bin/env" ]; then
    if which gh >/dev/null 2>&1; then
        export OPENAI_API_BASE="https://github.com"
        export OPENAI_API_KEY=$(gh auth token)
    else
         COPILOT_CONFIG="$HOME/.config/github-copilot/hosts.json"
         if [ -e "$COPILOT_CONFIG" ]; then
             for host in $(jq -r 'keys[]' "$COPILOT_CONFIG"); do
                 export OPENAI_API_BASE="https://${host}"
                 export OPENAI_API_KEY=$(jq -r ".[\"$host\"].oauth_token" "$COPILOT_CONFIG")
             done
         fi
    fi
    source "$HOME/.local/bin/env"
fi

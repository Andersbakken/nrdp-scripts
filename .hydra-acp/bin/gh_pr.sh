#!/bin/sh
# List my open PRs for the current repo (used by a sidebar process gadget
# in ~/.hydra-acp/config.json). Netflix-internal repos clone from
# git.netflix.net, which gh doesn't know as a host -- it authenticates via
# Metatron client certs, not gh's OAuth token, so gh only ever knows
# github.com and github.netflix.net. Only THAT case gets routed through
# github.netflix.net explicitly; every other repo (github.com included)
# runs plain `gh` untouched, so this never redirects a non-Netflix host.
me=$(whoami)
# Auto-generated PR titles look like "<repo>: <branch>-><target>"; strip
# the repo-name prefix, and when the branch is one of our own auto
# dev/pr branches ((dev|pr)/$USER/auto/...) strip that prefix (and the
# auto/ segment after it) too, so the row fits the sidebar's narrow
# column instead of just getting truncated.
# The number is wrapped as a markdown link so the sidebar gadget renders
# it as a real OSC 8 hyperlink (ctrl/cmd-click opens the PR).
jq_filter='.[] | "[#\(.number)](\(.url)) \(.title | sub("^[^:]+:\\s*"; "") | sub("^(dev|pr)/" + $me + "/(auto/)?"; ""))"'
remote=$(git remote get-url origin 2>/dev/null)
case "$remote" in
  *git.netflix.net*)
    repo=$(echo "$remote" | sed -E 's#\.git$##' | grep -oE '[^/]+/[^/]+$')
    GH_HOST=github.netflix.net gh pr list -R "$repo" --author "@me" --json number,title,url 2>/dev/null | jq -r --arg me "$me" "$jq_filter"
    ;;
  *)
    gh pr list --author "@me" --json number,title,url 2>/dev/null | jq -r --arg me "$me" "$jq_filter"
    ;;
esac

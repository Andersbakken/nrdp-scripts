#!/usr/bin/env bash
# List or cancel queued PR builds on nrdp-jenkins-agents.
#
# Usage:
#   cancel-pr-builds.sh              -- list PR queue items grouped by PR
#   cancel-pr-builds.sh --pr 36701   -- print IDs for one PR (no cancel)
#   cancel-pr-builds.sh --cancel-pr 36701
#   cancel-pr-builds.sh --cancel-all-prs
#
set -eo pipefail

BASE="https://nrdp.builds.test.netflix.net:7004"

fetch_queue() {
    metatron curl_proxy -a jenkins -- -sS -f --globoff \
        "$BASE/queue/api/json?tree=items[id,task[name],params,why]"
}

pr_ids_for() {
    local pr="$1"
    fetch_queue | jq -r --arg pr "$pr" '
        .items[]
        | select(.why | test("nrdp-jenkins-agents"; "i"))
        | select(.params | test("NF_PULLREQUEST=" + $pr + "\\b"))
        | .id'
}

cancel_id() {
    local id="$1"
    local code
    code=$(metatron curl_proxy -a jenkins -- -sS -o /dev/null -w '%{http_code}' \
        -X POST "$BASE/queue/cancelItem?id=$id")
    printf '  id=%s http=%s\n' "$id" "$code"
}

case "${1:-}" in
    --pr)
        pr_ids_for "$2"
        ;;

    --cancel-pr)
        pr="$2"
        ids=$(pr_ids_for "$pr")
        [ -z "$ids" ] && { echo "no queued items for PR $pr"; exit 0; }
        echo "$ids" | wc -l | awk '{printf "cancelling %s queue items for PR '"$pr"'\n",$1}'
        for id in $ids; do cancel_id "$id"; done
        ;;

    --cancel-all-prs)
        ids=$(fetch_queue | jq -r '
            .items[]
            | select(.why | test("nrdp-jenkins-agents"; "i"))
            | select(.params | test("NF_PULLREQUEST=[0-9]"))
            | .id')
        [ -z "$ids" ] && { echo "no PR queue items"; exit 0; }
        echo "$ids" | wc -l | awk '{printf "cancelling %s PR queue items\n",$1}'
        for id in $ids; do cancel_id "$id"; done
        ;;

    ""|--list)
        fetch_queue | jq -r '[.items[]
                              | select(.why | test("nrdp-jenkins-agents"; "i"))
                              | select(.params | test("NF_PULLREQUEST=[0-9]"))
                              | (.params | capture("NF_PULLREQUEST=(?<pr>[0-9]+)").pr)]
                             | group_by(.) | map({pr: .[0], count: length})
                             | sort_by(-.count)
                             | (["PR","COUNT"] | (., map(length*"-"))),
                               (.[] | [.pr, (.count|tostring)])
                             | @tsv' \
            | column -t -s $'\t'
        ;;

    -h|--help)
        sed -n '2,10p' "$0"
        ;;

    *)
        echo "unknown: $1" >&2
        exit 2
        ;;
esac

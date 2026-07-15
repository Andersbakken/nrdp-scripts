#!/usr/bin/env bash
set -eo pipefail

mode=table
for arg in "$@"; do
    case "$arg" in
        --json|-j) mode=json ;;
        --table|-t) mode=table ;;
        -h|--help)
            cat <<EOF
Usage: $(basename "$0") [--table | --json]
  --table  (default) print top 20 queued jobs waiting for nrdp-jenkins-agents
  --json   print the raw JSON grouping
EOF
            exit 0
            ;;
    esac
done

data=$(metatron curl_proxy -a jenkins -- -sS -f --globoff \
    "https://nrdp.builds.test.netflix.net:7004/queue/api/json?tree=items[task[name],why]" \
    | jq '[.items[] | select(.why | test("nrdp-jenkins-agents"; "i"))]
          | group_by(.task.name)
          | map({job: .[0].task.name, count: length})
          | sort_by(-.count)
          | .[0:20]')

if [ "$mode" = "json" ]; then
    printf '%s\n' "$data"
    exit 0
fi

printf '%s\n' "$data" \
    | jq -r '(["JOB","COUNT"] | (., map(length*"-"))),
             (.[] | [.job, (.count|tostring)])
             | @tsv' \
    | column -t -s $'\t'

if ! which jira >/dev/null 2>&1; then
    echo "Cannot find jira commandline!"
    exit 2
fi

JIRA_PASSWORD=
exec_jira() {
    JIRA_ARGS=
    if [ -z "$JIRA_PASSWORD" ]; then
        if [ -z "$JIRA_PASSWORD_COMMAND" ] && [ -f "$JIRA_PASSWORD_FILE" ]; then
            DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
            JIRA_PASSWORD_COMMAND="$DIR/git-jira-password-command.sh $JIRA_PASSWORD_FILE"
        fi
        if [ -n "$JIRA_PASSWORD_COMMAND" ]; then
            JIRA_PASSWORD="`$JIRA_PASSWORD_COMMAND`"
        fi
    fi
    if [ -z "$JIRA_PASSWORD" ]; then
        read -p "JIRA Password: " -s JIRA_PASSWORD
    fi
    [ -n "$JIRA_USER" ] && JIRA_ARGS="$JIRA_ARGS --user $JIRA_USER"
    [ -n "$JIRA_PASSWORD" ] && JIRA_ARGS="$JIRA_ARGS --password $JIRA_PASSWORD"
    $TEST jira $JIRA_ARGS "$@"
}

#!/usr/bin/env node

const fs = require("fs");
const WebSocket = require("ws");
const argv = require("minimist")(process.argv.slice(2));
const Jira = require("jira-client");

const port = argv.port || 58910;

const opts = {};

function log(...args) {
    for (let a of args) {
        let m;
        try {
            m = JSON.stringify(a, null, 4);
        } catch (e) {
            m = Object.keys(a);
        }
        fs.appendFileSync("/tmp/git-jira-server.log", m + " ");
    }
    fs.appendFileSync("/tmp/git-jira-server.log", "\n");
}

console.log = log;

const wss = new WebSocket.Server({ port: port });
wss.on("connection", ws => {
    let pending = [];

    const processPending = () => {
        for (let i = 0; i < pending.length; ++i) {
            let msg = pending[i];

            const ok = (msg, data) => {
                try {
                    ws.send(JSON.stringify({ success: true,
                                             msg: msg,
                                             data: data }));
                } catch (err) {
                    log("Failed to ok message to client", JSON.stringify(msg));
                }
            };
            const error = (msg, err) => {
                try {
                    ws.send(JSON.stringify({ success: false,
                                             msg: msg,
                                             error: err }));
                } catch (err) {
                    log("Failed to error message to client", JSON.stringify(msg));
                }
                if (err.statusCode == 500)
                    opts.jira = undefined;
            };

            if (msg.issue) {
                if (msg.resolve) {
                    let cmd = { transition: { id: 5 } };
                    if (msg.comment)
                        cmd.update = { comment: [ { add: { body: msg.comment } } ] };
                    opts.jira.transitionIssue(msg.issue, cmd).then(transition => {
                        ok(msg, transition);
                    }).catch(err => {
                        error(msg, err);
                    });
                } else if (msg.comment) {
                    console.log("about to comment", msg.issue, msg.comment);
                    opts.jira.addComment(msg.issue, msg.comment).then(comment => {
                        ok(msg, comment);
                    }).catch(err => {
                        error(msg, err);
                    });
                }
                /*
                opts.jira.findIssue(msg.issue).then(issue => {
                    ok(msg, issue);
                }).catch(err => {
                    error(msg, err);
                });
                */
            }
        }
        pending = [];
    };

    ws.on("message", json => {
        let msg;
        try {
            msg = JSON.parse(json);
        } catch (err) {
        }

        if (msg.username) {
            opts.username = msg.username;
            opts.password = msg.password;

            const jiraopts = {
                protocol: "https",
                host: "jira.netflix.com",
                username: opts.username,
                password: opts.password,
                apiVersion: "2",
                strictSSL: true
            };
            //log("welp", jiraopts);

            opts.jira = new Jira(jiraopts);
            processPending();

            //log("got foo", opts.username, opts.password);
            return;
        }

        pending.push(msg);
        if (opts.jira)
            processPending();
    });
    if (!opts.jira) {
        ws.send(JSON.stringify({ needsPassword: true }));
        return;
    }
});

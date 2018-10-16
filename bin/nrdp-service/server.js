#!/usr/bin/env node

const fs = require("fs");
const WebSocket = require("ws");
const argv = require("minimist")(process.argv.slice(2));
const Jira = require("jira-client");
const request = require("request");
const querystring = require('querystring');

const port = argv.port || 58910;

const opts = {};

const original_console_log = console.log;
function log(...args) {
    for (let a of args) {
        let m;
        try {
            m = JSON.stringify(a, null, 4);
        } catch (e) {
            m = Object.keys(a);
        }
        original_console_log("Console: ", m);
        fs.appendFileSync("/tmp/nrdp-service.log", m + " ");
    }
    fs.appendFileSync("/tmp/nrdp-service.log", "\n");
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
                if(err.statusCode == 500)
                    opts.password = undefined;
            };

            if (msg.mode == "jira") {
                if (!opts.jira) {
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
                }
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
            } else if(msg.mode == "stash") {
                var project = msg.project || "NRDP", repo = msg.repo || "nrdp"
                if(msg.from && msg.to) {
                    var form = {
                        title: project + '(' + repo + '): auto pull-request ' + msg.from + '->' + msg.to,
                        fromRef: {
                            id: "refs/heads/" + msg.from,
                            repository: {
                                slug: repo,
                                project: { key: project }
                            }
                        },
                        toRef: {
                            id: "refs/heads/" + msg.to,
                            repository: {
                                slug: repo,
                                project: { key: project }
                            }
                        }
                    };
                    request.post('https://stash.corp.netflix.com/rest/api/1.0/projects/' + project + '/repos/' + repo + '/pull-requests', {
                        body: JSON.stringify(form),
                        headers: {
                            'Content-Type': 'application/json'
                        },
                        auth: {
                            user: opts.username,
                            pass: opts.password,
                            sendImmediately: true
                        }
                    }, function(err, response, body) {
                        var data = { statusCode: response.statusCode, body: body };
                        if(err || data.statusCode != 200) {
                            var error_msg = data.body;
                            try {
                                var e = JSON.parse(data.body);
                                error_msg = e.errors[0].message;
                            } catch(v) {
                                error_msg = data.body;
                            }
                            error(msg, { message: error_msg });
                        } else {
                            ok(msg, data);
                        }
                    });
                }
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
        } else {
            pending.push(msg);
        }
        if (opts.password) {
            try {
                processPending();
            } catch(v) {
                log("Exception: " + v.toString());
            }
        }
    });
    if (!opts.password) {
        ws.send(JSON.stringify({ needsPassword: true }));
        return;
    }
});

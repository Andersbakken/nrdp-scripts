#!/usr/bin/env node

const fs = require("fs");
const WebSocket = require("ws");
const crypto = require('crypto');
const argv = require("minimist")(process.argv.slice(2));
const Jira = require("jira-client");
const url_request = require("request");
const querystring = require('querystring');

const port = argv.port || 58910;

function hash()
{
    const hash = crypto.createHash('md5');
    const data = fs.readFileSync(__filename);
    hash.update(data, 'utf8');
    return hash.digest('hex');
}

const originalHash = hash();
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
            let request = pending[i];

            const ok = (request, data, message) => {
                try {
                    ws.send(JSON.stringify({ success: true,
                                             request: request,
                                             data: data || {} }));
                } catch (err) {
                    log("Failed to ok message to client", JSON.stringify(request));
                }
            };
            const error = (request, err) => {
                try {
                    ws.send(JSON.stringify({ success: false,
                                             request: request,
                                             error: err }));
                } catch (e) {
                    log("Failed to error message to client", JSON.stringify(request));
                }
            };

            if (request.mode.startsWith("jira.")) {
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
                if (request.issue) {
                    if (request.resolve) {
                        let cmd = { transition: { id: 5 } };
                        if (request.comment)
                            cmd.update = { comment: [ { add: { body: request.comment } } ] };
                        opts.jira.transitionIssue(request.issue, cmd).then(transition => {
                            ok(request, transition);
                        }).catch(err => {
                            error(request, err);
                            if(err.statusCode == 500)
                                opts.password = undefined;
                        });
                    } else if (request.comment) {
                        console.log("about to comment", request.issue, request.comment);
                        opts.jira.addComment(request.issue, request.comment).then(comment => {
                            ok(request, comment);
                        }).catch(err => {
                            error(request, err);
                            if(err.statusCode == 500)
                                opts.password = undefined;
                        });
                    }
                    /*
                      opts.jira.findIssue(request.issue).then(issue => {
                      ok(request, issue);
                      }).catch(err => {
                      error(request, err);
                      });
                    */
                }
            } else if(request.mode.startsWith("stash.pr.")) {
                var project = request.project, repo = request.repo;
                if(request.mode == "stash.pr.create" && request.from && request.to) {
                    var form = {
                        title: project + '(' + repo + '): ' + request.from + '->' + request.to,
                        fromRef: {
                            id: "refs/heads/" + request.from,
                            repository: {
                                slug: repo,
                                project: { key: project }
                            }
                        },
                        toRef: {
                            id: "refs/heads/" + request.to,
                            repository: {
                                slug: repo,
                                project: { key: project }
                            }
                        }
                    };
                    url_request.post('https://stash.corp.netflix.com/rest/api/1.0/projects/' + project + '/repos/' + repo + '/pull-requests', {
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
                        try {
                            var pr;
                            var obj = JSON.parse(data.body);
                            if(obj.errors) {
                                var e = obj.errors[0];
                                data.message = e.message;
                                pr = e.existingPullRequest;
                            } else {
                                pr = obj;
                            }
                            if(pr.links) {
                                const href = pr.links.self[0].href;
                                if(href)
                                    data.message = (data.message ? (data.message + ": ") : "Status: ") + href;
                            }
                        } catch(v) {
                        }
                        if(err || data.statusCode >= 300) {
                            if(!data.message)
                                data.message = data.body;
                            var error_msg = data.body;
                            error(request, { message: data.message });
                            if(data.statusCode == 401)
                                opts.password = undefined;
                        } else {
                            ok(request, data);
                        }
                    });
                } else if(request.mode == "stash.pr.issues") {
                    console.log('https://stash.corp.netflix.com/rest/jira/1.0/projects/' + project + '/repos/' + repo + '/pull-requests/' + request.pullRequest + '/issues');
                    url_request.get('https://stash.corp.netflix.com/rest/jira/1.0/projects/' + project + '/repos/' + repo + '/pull-requests/' + request.pullRequest + '/issues', {
                        auth: {
                            user: opts.username,
                            pass: opts.password,
                            sendImmediately: true
                        }
                    }, function(err, response, body) {
                        var data = { statusCode: response.statusCode, body: body };
                        data.message = body;
                        if(err || data.statusCode >= 300) {
                            if(!data.message)
                                data.message = data.body;
                            var error_msg = data.body;
                            error(request, { message: data.message });
                            if(data.statusCode == 401)
                                opts.password = undefined;
                        } else {
                            ok(request, data);
                        }
                    });
                } else if(request.mode == "stash.pr.list") {
                    var state = "ALL";
                    if(request.state && request.state.length !== "")
                        state = request.state;
                    url_request.get('https://stash.corp.netflix.com/rest/api/1.0/projects/' + project + '/repos/' + repo + '/pull-requests?state=' + state + '&order=NEWEST', {
                        auth: {
                            user: opts.username,
                            pass: opts.password,
                            sendImmediately: true
                        }
                    }, function(err, response, body) {
                        var data = { statusCode: response.statusCode, body: body };
                        data.message = body;
                        if(err || data.statusCode >= 300) {
                            if(!data.message)
                                data.message = data.body;
                            var error_msg = data.body;
                            error(request, { message: data.message });
                            if(data.statusCode == 401)
                                opts.password = undefined;
                        } else {
                            ok(request, data);
                        }
                    });
                }
            }
        }
        pending = [];
    };

    ws.on("message", json => {
        let request;
        try {
            request = JSON.parse(json);
        } catch (err) {
        }

        if (request.username) {
            opts.username = request.username;
            opts.password = request.password;
        } else {
            pending.push(request);
        }
        if (opts.password) {
            try {
                processPending();
            } catch(v) {
                log("Exception: " + v.toString());
            }
        }
    });
    if (hash() != originalHash) {
        ws.send(JSON.stringify({ relaunch: true }));
        process.exit();
    }

    if (!opts.password) {
        ws.send(JSON.stringify({ needsPassword: true }));
        return;
    }
});


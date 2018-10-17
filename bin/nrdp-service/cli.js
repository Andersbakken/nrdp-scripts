#!/usr/bin/env node

const WebSocket = require("ws");
const child_process = require("child_process");
const argv = require("minimist")(process.argv.slice(2));
const gpg = require("gpg");
const readline = require("readline");
const Writable = require("stream").Writable;

let muted = false;
const mutedStdout = new Writable({
    write: function(chunk, encoding, callback) {
        if (!muted)
            process.stdout.write(chunk, encoding);
        callback();
    }
});

function readPassword()
{
    return new Promise((resolve, reject) => {
        const rl = readline.createInterface({
            input: process.stdin,
            output: mutedStdout,
            terminal: true
        });

        muted = false;
        rl.question("Password: ", pwd => {
            // terminate the muted output with a newline
            console.log("");

            rl.close();
            resolve(pwd);
        });
        muted = true;
    });
}

const port = argv.port || 58910;

let spawned = false;
function connect()
{
    let username = argv.username;
    if (!username)
        username = process.env.USER;

    let action = { mode: argv.mode }
    if(action.mode == "stash") {
        if(!argv.from || !argv.to) {
            console.error("need an from/to");
            return;
        }
        action.project = argv.project;
        action.repo = argv.repo;
        action.from = argv.from;
        action.to = argv.to;
    } else if(action.mode == "jira") {
        if (!argv.issue) {
            console.error("need an issue");
            return;
        }

        action.comment = argv.comment;
        action.resolve = argv.resolve;
        if (!action.resolve || !action.comment) {
            console.error("need comment or resolve");
            return;
        }
        action.issue = argv.issue;
    }

    const ws = new WebSocket(`ws://localhost:${port}`);

    ws.on("open", () => {
        try {
            ws.send(JSON.stringify(action));
        } catch (err) {
            console.error("failed to send action to server", err);
            process.exit();
        }
    });

    ws.on("message", json => {
        let response;
        try {
            response = JSON.parse(json);
        } catch (err) {
        }

        if (response.needsPassword) {
            //console.log("gotta send stuff");
            if (process.env["JIRA_PASSWORD_FILE"]) {
                //console.log("hehhh");
                gpg.decryptFile(process.env["JIRA_PASSWORD_FILE"], (err, contents) => {
                    if (!contents) {
                        console.error("gpg error", err);
                        process.exit(1);
                        return;
                    }

                    //console.log("hehhh2");
                    ws.send(JSON.stringify({
                        username: username,
                        password: contents.toString("utf8").trim()
                    }));
                });
                //console.log("hehhh3");
            } else {
                readPassword().then(pwd => {
                    //console.log("hohhh2");
                    ws.send(JSON.stringify({
                        username: username,
                        password: pwd
                    }));
                });
            }
        }
        if ("success" in response) {
            const message = response.success ? response.data.message : response.error.message;
            if(message)
                console.error(message);
            process.exit(!response.success);
        }
    });

    ws.on("error", error => {
        if (error.code == 'ECONNREFUSED') {
            if (!spawned) {
                console.log("Starting daemon");
                const daemon = child_process.spawn("node", [`${__dirname}/server.js`, "--port", port], { detached: true });
                daemon.unref();

                spawned = true;

                //console.log("Waiting for daemon");
            }
            setTimeout(connect, 100);
        } else {
            console.error("websocket error", error);
        }
    });
}

connect();

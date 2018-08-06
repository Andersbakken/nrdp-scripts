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
    let action;
    if (argv.comment) {
        action = { comment: argv.comment };
    }
    if (argv.resolve) {
        if (!action)
            action = {};
        action.resolve = argv.resolve;
    }

    if (!action) {
        console.error("need comment or resolve");
        return;
    }
    if (!argv.issue) {
        console.error("need an issue");
        return;
    }

    action.issue = argv.issue;

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
        let msg;
        try {
            msg = JSON.parse(json);
        } catch (err) {
        }

        if (msg.needsPassword) {
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
                        username: process.env.USER,
                        password: contents.toString("utf8").trim()
                    }));
                });
                //console.log("hehhh3");
            } else {
                readPassword().then(pwd => {
                    //console.log("hohhh2");
                    ws.send(JSON.stringify({
                        username: process.env.USER,
                        password: pwd
                    }));
                });
            }
        }
        if ("success" in msg) {
            if(!msg.success)
                console.error(msg.error.message);
            process.exit(msg.success ? 0 : 1);
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

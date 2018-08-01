#!/usr/bin/env node

const http = require('http');
const child_process = require('child_process');

let spawned = false;
function connect()
{
    var req = http.get("http://localhost:58910", resp => {
        console.log("we're connected resp");
    });

    req.on('error', error => {
        // console.log("Got error", error);
        if (error.code == 'ECONNREFUSED') {
            if (spawned) {
                console.log("ECONNREFUSED starting daemon");
                const daemon = child_process.spawn('node', `$(__dirname)/server.js`);
            } else {
                console.log("Waiting for daemon");
                setTimeout(connect, 1000);
            }
        } else {
            console.log("Unknown error", error);
        }
    });
}



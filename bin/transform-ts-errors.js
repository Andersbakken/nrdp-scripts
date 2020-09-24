#!/usr/bin/env node

const process = require("process");
const path = require("path");
const fs = require("fs");

const child = require("child_process").execFile(process.argv[2], process.argv.slice(3));

const lastWasEmpty = [ false, false ];
let lastFile;

function filter(idx, line) {
    if (!line) {
        if (lastWasEmpty[idx])
            return;
        lastWasEmpty[idx] = true;
    } else {
        // removing colors I think
        line = line.replace(/[\u001b\u009b][[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]/g, "");
        lastWasEmpty[idx] = false;
    }

    if (/^ *at [^ ]* \(.*:[0-9]+:[0-9]+\)/.exec(line)) {
        // console.log("not doing this", line);
        return;
    }
    if (/^ *at \/.*:[0-9]+:[0-9]+/.exec(line)) {
        // console.log("not doing this either", line);
        return;
    }

    let match = /Error: (.*)/.exec(line);
    if (match) {
        line = match[1];
        const index = line.indexOf(" ");
        if (index !== -1) {
            let str = " error:";
            if (line[index - 1] !== ":")
                str = ":" + str;
            line = line.substr(0, index) + str + line.substr(index);
        }
    } else if (/^\/[A-Za-z0-9_\/\.-]*\.ts$/.exec(line)) {
        lastFile = line;
        return;
    } else if (lastFile) {
        match = /^ *([0-9]+):([0-9]+) *([a-z]*) *(.*)/.exec(line);
        if (match) {
            line = `${match[3].toUpperCase()}: ${lastFile}:${match[1]}:${match[2]} - ${match[4]}`;
        }
    }

    if (idx === 0) {
        // console.log("out", `[${line}]`);
        console.log(line);
    } else {
        // console.error("err", `[${line}]`);
        console.error(line);
    }
}


child.stdout.on("data", data => {
    data.toString().split("\n").forEach(filter.bind(undefined, 0));
});

child.stderr.on("data", data => {
    data.toString().split("\n").forEach(filter.bind(undefined, 1));
});

child.on("exit", (code, signal) => {
    process.exit(code);
});

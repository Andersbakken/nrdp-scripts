#!/usr/bin/env node

const process = require("process");
const path = require("path");
const fs = require("fs");

const child = require("child_process").execFile(process.argv[2], process.argv.slice(3));

const lastWasEmpty = [ false, false ];
let lastFile;
let lastError;

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

    if (line.startsWith("[!] (plugin rpt2) ")) {
        line = line.substring(18);
    }

    let match = /^([A-Za-z0-9_./]*.ts:[0-9]+:[0-9]+:? *)?error: (.*)/.exec(line);
    // console.log("line", line, match);
    if (match) {
        line = match[1] ? match[1] + match[2] : match[2];
        const index = line.indexOf(" ");
        if (index !== -1) {
            let str = " error:";
            if (line[index - 1] !== ":")
                str = ":" + str;
            line = line.substr(0, index) + str + line.substr(index);
        }
    } else if (line.includes("[^0-9]: error:")) {
        // error without line/col it messes up emacs, AFAICT these get
        // printed later in a way that has more context
        // example is something like this:
        // ')': error: expected. (104:42)
        // later on we get this:
        // src/SSLManager.ts:104:42: error: - error TS1005: ')' expected.
        return;
    } else if (/^\/[A-Za-z0-9_\/\.-]*\.ts$/.exec(line)) {
        lastFile = line;
        return;
    } else if (lastFile) {
        match = /^ *([0-9]+):([0-9]+) *([a-z]*) *(.*)/.exec(line);
        if (match) {
            line = `${match[3].toUpperCase()}: ${lastFile}:${match[1]}:${match[2]} - ${match[4].replace(/\s+/g, " ")}`;
        }
    } else if (line.startsWith("@rollup/plugin-typescript: error: ")) {
        lastError = line.substring(34);
        return;
    } else if (line.startsWith("[!] (plugin typescript) Error: @rollup/plugin-typescript ")) {
        lastError = line.substring(57);
        return;
    } else if (lastError) {
        match = /^([A-Za-z0-9_\/\.-]*\.ts) \(([0-9]+):([0-9]+)\)$/.exec(line);
        if (match) {
            // console.log(lastError, match);
            line = `error: ${match[1]}:${match[2]}:${match[3]} - ${lastError.replace(/\s+/g, " ")}`;
            lastError = undefined;
        }
    }

    if (line.startsWith("Error: ") && line.includes("error")) {
        line = line.substring(7);
    }

    if (idx === 0) {
        // console.log("out", `[${line}]`);
        console.log(line);
    } else {
        // console.error("err", `[${line}]`);
        console.error(line);
    }
}

let stdoutPending = "";
child.stdout.on("data", data => {
    stdoutPending += data;
    const lastNewLine = stdoutPending.lastIndexOf("\n");
    if (lastNewLine !== -1) {
        const lines = stdoutPending.substr(0, lastNewLine + 1).split("\n");
        stdoutPending = stdoutPending.substr(lastNewLine + 1);
        lines.forEach(filter.bind(undefined, 0));
    }
});

let stderrPending = "";
child.stderr.on("data", data => {
    stderrPending += data;
    const lastNewLine = stderrPending.lastIndexOf("\n");
    if (lastNewLine !== -1) {
        const lines = stderrPending.substr(0, lastNewLine + 1).split("\n");
        stderrPending = stderrPending.substr(lastNewLine + 1);
        lines.forEach(filter.bind(undefined, 1));
    }
});

child.on("exit", (code, signal) => {
    process.exit(code);
});

#!/usr/bin/env node

const util = require("util");
const fs = require("fs");
const path = require("path");
const child_process = require("child_process");
const exec = util.promisify(child_process.exec);

let executable;
let daemonize = false;

let args = process.argv.slice(2).filter(arg => {
    if (arg === "-d" || arg === "--daemonize") {
        daemonize = true;
        return false;
    }
    return true;
});

let log = () => {};
let error = (...args) => {
    console.error(`${Date.now() - start}ms `.padEnd(10, ' '), ...args);
};

const start = Date.now();

// OBJCOPY=objcopy_in_toolchain gdb-add-index netflix (edited)

function usage(log)
{
    log(`gdb-add-index.js [--help|-h] [--daemonize|-d] [--silent] [--verbose|-v] [--objcopy=<arg>][--objcopy|-O <arg>] [--log-file=<arg>] [--log-file|-l <arg>] <executable>`);
}

function logFile(arg)
{
    let logFile = arg;
    try {
        fs.unlinkSync(logFile);
    } catch (err) {
    }
    log = error = (...args) => {
        try {
            fs.appendFileSync(logFile, `${Date.now() - start}ms `.padEnd(10, ' '), ...args);
        } catch (err) {
        }
    };
}

for (let idx=0; idx<args.length; ++idx) {
    const arg = args[idx];
    if (arg.startsWith("--objcopy=")) {
        process.env.OBJCOPY = arg.substr(11);
    } else if (arg === "--objcopy" || arg === "-O") {
        process.env.OBJCOPY = process.argv[++idx];
    } else if (arg === "-v" || arg === "--verbose") {
        log = (...args) => {
            console.log(`${Date.now() - start}ms `.padEnd(10, ' '), ...args);
        };
    } else if (arg === "--silent") {
        log = error = () => {};
    } else if (arg === "--help" || arg === "-h") {
        usage(console.log.bind(console));
        process.exit(0);
    } else if (arg.startsWith("--log-file=")) {
        if (!daemonize)
            logFile(arg);
    } else if (arg === "--log-file" || arg === "-l") {
        if (!daemonize)
            logFile(process.args[++idx]);
    } else if (!executable) {
        executable = arg;
    } else {
        usage(console.error.bind(console));
        console.error(`Unknown argument "${arg}"`);
        process.exit(1);
    }
}

if (!executable) {
    console.error("No executable");
    process.exit(1);
}

if (daemonize) {
    const proc = child_process.fork(path.join(__filename), args, { detached: true, stdio: (daemonize ? "ignore" : "inherit") });
    proc.unref();
    process.exit();
}

let temp = `${path.dirname(executable)}/.${path.basename(executable)}.gdb-add-index.js.${process.pid}`;

function cleanup()
{
    try {
        fs.unlinkSync(temp);
    } catch (err) {
    }
}
process.on("exit", cleanup);
process.on("SIGINT", () => {
    cleanup();
    process.exit();
});

function shit()
{
    return new Promise(res => {
        setTimeout(res, 2000);
    });
}

function validate(prev)
{
    try {
        let stat = fs.statSync(executable);
        if (prev && (!stat.isFile() || stat.mtimeMs !== prev.mtimeMs || stat.size !== prev.size)) {
            return undefined;
        }
        return stat;
    } catch (err) {
        return undefined;
    }
}

async function run()
{
    try {
        let pre = validate();
        if (!pre) {
            error("Couldn't stat");
            process.exit();
        }
        log("Running cp", executable, temp);
        await exec(`cp "${executable}" "${temp}"`);
        if (!validate(pre)) {
            error("Things changed");
            process.exit();
        }

        log("Adding fs watcher for", executable);
        const watcher = fs.watch(executable, (stat) => {
            if (stat.mtimeMs !== pre.mtimeMs || stat.size !== pre.size) {
                error("Got a change in modtime while adding index");
                process.exit();
            }
        });

        log("Running gdb-add-index", temp);
        await exec(`gdb-add-index "${temp}"`);
        log("Removing fs watcher for", executable);
        watcher.close();
        log("Running rename", temp, executable);
        await exec(`mv ${temp} ${executable}`);
        // fs.renameSync(temp, executable);
        log("Finished");
        process.exit();
    } catch (err) {
        error(err);
    }
}

run();


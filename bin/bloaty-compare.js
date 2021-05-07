#!/usr/bin/env node

const child_process = require("child_process");
const fs = require("fs");

function usage()
{
    return `bloaty-compare.js [--help|-h] [--human|-H] file1 file2`;
}
let humanReadable = false;
let sortByDiff = false;
let a, b;
for (let idx=2; idx<process.argv.length; ++idx) {
    const arg = process.argv[idx];
    switch (arg) {
    case "-h":
    case "--help":
        console.log(usage());
        process.exit(0);
    case "--human":
    case "-H":
        humanReadable = true;
        break;
    default:
        if (!a) {
            a = fs.realpathSync(arg);
        } else if (!b) {
            b = fs.realpathSync(arg);
        } else {
            console.error(usage());
            console.error("Unknown argument", arg);
            process.exit(1);
        }
        break;
    }
}

class Bloaty
{
    constructor(file, headers, vm, filesize)
    {
        this.file = file;
        this.headers = headers;
        this.vm = vm;
        this.filesize = filesize;
    }
}

function bloaty(file)
{
    return new Promise((resolve, reject) => {
        child_process.exec(`bloaty --csv ${file}`, (err, stdout, stderr) => {
            if (err) {
                reject(err);
                return;
            }
            if (stderr) {
                reject(stderr);
                return;
            }

            let headers = [];
            let vm = [];
            let filesize = [];
            let totVM = 0;
            let totFilesize = 0;
            stdout.split("\n").filter((x, idx) => {
                return x && idx;
            }).forEach(line => {
                const split = line.split(",");
                headers.push(split[0]);
                const v = parseInt(split[1]);
                const f = parseInt(split[2]);
                totVM += v;
                totFilesize += f;
                vm.push(v);
                filesize.push(f);
            });
            headers.push("total");
            vm.push(totVM);
            filesize.push(totFilesize);
            resolve(new Bloaty(file, headers, vm, filesize));
        });
    });
}

function formatNum(num)
{
    if (humanReadable) {
        const units = [ "B", "kB", "mB", "gB" ];
        let idx = 0;
        while (idx<units.length) {
            if (Math.abs(num) > 1000) {
                num /= 1000;
                ++idx;
            } else {
                break;
            }
        }
        if (num != Math.floor(num)) {
            return `${num.toFixed(1)}${units[idx]}`;
        }
        return `${num}${units[idx]}`;
    }
    return "" + num;
}

function diff(a, b, percentage)
{
    let diff = b - a;
    if (percentage) {
        return diff ? ((diff / a) * 100).toFixed(1) + "%" : "0%";
    }
    return formatNum(diff);
}

function pad(val, len)
{
    if (typeof val === "number") {
        val = formatNum(val);
    }
    return val.padEnd(len, ' ');
}

function compare(a, b)
{
    console.log("a", a.file);
    console.log("b", b.file);
    console.log();
    [ "vm", "filesize"].forEach(type => {
        let maxHeaderLength = 0;
        let maxNumberLength = 0;
        a.headers.forEach((header, idx) => {
            maxHeaderLength = Math.max(header.length, maxHeaderLength);
            maxNumberLength = Math.max(maxNumberLength,
                                       formatNum(a.vm[idx]).length,
                                       formatNum(b.vm[idx]).length,
                                       formatNum(a.filesize[idx]).length,
                                       formatNum(b.filesize[idx]).length);
        });
        maxHeaderLength += 4;
        maxNumberLength += 4;
        const separator = "".padStart(maxHeaderLength + (maxNumberLength * 4), "#");

        console.log(type);
        console.log(separator);
        console.log(`${pad("type", maxHeaderLength)}${pad("a", maxNumberLength)}${pad("b", maxNumberLength)}${pad("diff", maxNumberLength)}${pad("%diff", maxNumberLength)}`);
        console.log(separator);

        a.headers.forEach((header, idx) => {
            let str = pad(header, maxHeaderLength);
            str += pad(formatNum(a[type][idx]), maxNumberLength);
            str += pad(formatNum(b[type][idx]), maxNumberLength);
            str += pad(diff(a[type][idx], b[type][idx], false), maxNumberLength);
            str += pad(diff(a[type][idx], b[type][idx], true), maxNumberLength);
            if (idx === a.headers.length - 1) {
                console.log(separator);
                console.log(str);
                console.log(separator);
            } else {
                console.log(str);
            }
        });

        console.log();
    });
}

Promise.all([bloaty(a), bloaty(b)]).then(results => {
    compare(results[0], results[1]);
    // console.log("Got shit", results);
}).catch(err => {
    console.error(err);
    process.exit(1);
});

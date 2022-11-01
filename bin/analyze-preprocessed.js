#!/usr/bin/env node

const fs = require("fs");

process.argv.slice(2).forEach(file => {
    let cur;
    let files = new Map();
    console.log(file);
    fs.readFileSync(file, "utf8").split("\n").forEach(x => {
        // console.log("line", x);
        const match = /^# [0-9]+ "([^ "]+)/.exec(x);
        if (match) {
            const key = match[1];
            if (!files.has(key)) {
                cur = { value: 0 };
                files.set(key, cur);
            } else {
                cur = files.get(key);
            }
        } else if (cur) {
            ++cur.value;
        }
    });
    Array.from(files.entries()).map(x => [ x[0], x[1].value]).sort((l, r) => {
        // console.log(r[1], l[1]);
        return r[1] - l[1];
    }).forEach(x => {
        console.log(`${x[0]}: ${x[1]}`);
    });
});

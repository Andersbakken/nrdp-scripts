#!/usr/bin/env node

const fs = require("fs");

function prettyBytes(val)
{
    var units = [ "B", "KB", "MB", "GB" ];
    var idx = 0;
    while (idx<units.length && val>=1024) {
        ++idx;
        val /= 1024;
    }

    var num = Math.floor(val) === val ? val : val.toFixed(1);
    return num + units[idx];
}

let pretty = false;
let items = [];
for (let idx=2; idx<process.argv.length; ++idx) {
    if (process.argv[idx] === "--pretty-bytes") {
        pretty = true;
    } else {
        items.push(process.argv[idx]);
    }
}

if (!items.length) {
    const f = fs.readFileSync(0).toString();
    items = f.split("\n").filter(x => x);
    // console.log("items", items.length, items[0].includes(" "));
    if (items.length === 1 && items[0].includes(" ")) {
        items = items[0].split(/[\t ]+/);
    }
    // console.log("items", items, f);
}
let tot = 0;
const sorted = items.map(x => {
    const val = parseFloat(x);
    tot += val;
    return val;
}).sort((a, b) => b - a);

// sorted.forEach(x => console.log(x));
// console.log(sorted);

function formatNum(val)
{
    val = val || 0;
    if (val == Math.floor(val) || pretty) {
        return pretty ? prettyBytes(val) : val;
    }
    return val.toFixed(2);
}

let median;
if (sorted.length >= 2) {
    if (sorted.length % 2 === 0) {
        median = (sorted[sorted.length / 2] + sorted[(sorted.length / 2) - 1]) / 2;
    } else {
        median = sorted[(sorted.length - 1) / 2];
        // console.log(median, (sorted.length - 1) / 2);
    }
} else {
    median = sorted[0] || 0;
}

console.log("Total", formatNum(tot),
            "Count", items.length,
            "Min", formatNum(sorted[sorted.length - 1]),
            "Max", formatNum(sorted[0]),
            "Average", formatNum((tot / items.length)),
            "Median", formatNum(median));

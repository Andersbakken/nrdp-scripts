#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const load_1 = require("./load");
const parseArgs_js_1 = require("./parseArgs.js");
const assert_1 = __importDefault(require("assert"));
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
async function exec() {
    const options = await (0, parseArgs_js_1.parseArgs)();
    const urls = options.builds.map((x) => `https://build.dta.netflix.com/nrdp/${options.project}/${x}`);
    let infoResponse = await (0, load_1.load)(urls);
    if (infoResponse.status !== 200) {
        console.error(`Failed to fetch url(s) ${urls.join(", ")} (${infoResponse.status})`);
        process.exit(1);
    }
    let info = (await infoResponse.json());
    if (options.parentCount) {
        (0, assert_1.default)(options.builds.length === 1 && options.builds[0] === "master");
        const url = `https://build.dta.netflix.com/nrdp/${options.project}/${parseInt(String(info.variantBuildNumber)) - options.parentCount}`;
        infoResponse = await (0, load_1.load)(url);
        if (infoResponse.status !== 200) {
            console.error(`Failed to fetch ${url} (${infoResponse.status})`);
            process.exit(1);
        }
        info = (await infoResponse.json());
    }
    info.url = `https://build.dta.netflix.com/nrdp/${options.project}/${info.variantBuildNumber}/dist/${options.project}.${options.env}.js`;
    if (options.showInfo) {
        let longestKey = 0;
        console.error(options.infos
            .map((key) => {
            if (key === "list") {
                console.error(`Keys:\n    ${Object.keys(info).sort().join("\n    ")}`);
                return undefined;
            }
            if (key === "all") {
                console.error(JSON.stringify(info, null, 4));
                return undefined;
            }
            let val = info[key];
            if (typeof val === "object") {
                val = JSON.stringify(val);
            }
            else if (typeof val === "string") {
                const date = new Date(val);
                if (!isNaN(date.valueOf())) {
                    val = date.toString();
                }
            }
            longestKey = Math.max(key.length, longestKey);
            return [key, val];
        })
            .filter((x) => x)
            .map((x) => {
            return `${(x[0] + ":").padEnd(longestKey + 1)} ${x[1]}`;
        })
            .join("\n"));
    }
    if (options.output) {
        const response = await (0, load_1.load)(String(info.url));
        const contents = await response.text();
        fs_1.default.writeFileSync(options.output, contents);
        if (!options.output.startsWith("/dev/std")) {
            if (!options.output.startsWith("/")) {
                options.output = path_1.default.join(process.cwd(), options.output);
            }
            console.error(`Wrote ${info.url} to ${options.output}`);
        }
    }
}
exec();

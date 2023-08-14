#!/usr/bin/env node

import { Options } from "./Options.js";
import { load } from "./load";
import { parseArgs } from "./parseArgs.js";
import assert from "assert";
import fs from "fs";
import path from "path";

async function exec() {
    const options: Options = await parseArgs();
    const urls = options.builds.map((x) => `https://build.dta.netflix.com/nrdp/${options.project}/${x}`);
    let infoResponse;
    try {
        infoResponse = await load(urls);
        if (infoResponse.status !== 200) {
            console.error(`Failed to fetch url(s) ${urls.join(", ")} (${infoResponse.status})`);
            process.exit(1);
        }
    } catch (err: unknown) {
        console.error(`Failed to fetch url(s) ${urls.join(", ")} (${(err as Error).message})`);
        process.exit(1);
    }

    let info: Record<string, unknown> = (await infoResponse.json()) as Record<string, unknown>;
    if (options.parentCount) {
        assert(options.builds.length === 1 && options.builds[0] === "master");
        const url = `https://build.dta.netflix.com/nrdp/${options.project}/${
            parseInt(String(info.variantBuildNumber)) - options.parentCount
        }`;
        try {
            infoResponse = await load(url);
            if (infoResponse.status !== 200) {
                console.error(`Failed to fetch ${url} (${infoResponse.status})`);
                process.exit(1);
            }
        } catch (err: unknown) {
            console.error(`Failed to fetch ${url} (${(err as Error).message})`);
            process.exit(1);
        }

        info = (await infoResponse.json()) as Record<string, unknown>;
    }
    info.url = `https://build.dta.netflix.com/nrdp/${options.project}/${info.variantBuildNumber}/dist/${options.project}.${options.env}.js`;
    if (options.showInfo) {
        let longestKey = 0;
        console.error(
            options.infos
                .map((key: string) => {
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
                    } else if (typeof val === "string") {
                        const date = new Date(val);
                        if (!isNaN(date.valueOf())) {
                            val = date.toString();
                        }
                    }
                    longestKey = Math.max(key.length, longestKey);
                    return [key, val];
                })
                .filter((x) => x)
                .map((x: [string, string]) => {
                    return `${(x[0] + ":").padEnd(longestKey + 1)} ${x[1]}`;
                })
                .join("\n")
        );
    }

    if (options.output) {
        let response;
        let contents;
        try {
            response = await load(String(info.url));
            contents = await response.text();
        } catch (err: unknown) {
            console.error("Failed to download", info.url, (err as Error).message);
            process.exit(1);
        }
        fs.writeFileSync(options.output, contents);
        if (!options.output.startsWith("/dev/std")) {
            if (!options.output.startsWith("/")) {
                options.output = path.join(process.cwd(), options.output);
            }
            console.error(`Wrote ${info.url} to ${options.output}`);
        }
    }
}

exec();

#!/usr/bin/env node

import { LoadResponse } from "./LoadResponse";
import { Options } from "./Options.js";
import { gatherResponses } from "./gatherResponses";
import { parseArgs } from "./parseArgs.js";
import fs from "fs";
import path from "path";

async function exec() {
    const options: Options = await parseArgs();
    const responses = await gatherResponses(options);
    responses.forEach(async (response: LoadResponse, idx: number) => {
        const info: Record<string, unknown> = (await response.json()) as Record<string, unknown>;
        info.url = `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${info.repoBuildNumber})/dist/${options.project}.${options.env}.js`;
        if (options.showInfo) {
            let longestKey = 0;
            if (idx > 0) {
                console.error();
            }
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
            const contents = await response.text();
            fs.writeFileSync(options.output, contents);
            if (!options.output.startsWith("/dev/std")) {
                if (!options.output.startsWith("/")) {
                    options.output = path.join(process.cwd(), options.output);
                }
                console.error(`Wrote ${info.url} to ${options.output}`);
            }
        }
    });
}

exec();

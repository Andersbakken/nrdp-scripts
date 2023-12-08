#!/usr/bin/env node

import { BuildResult } from "./BuildResult";
import { Options } from "./Options.js";
import { gatherResponses } from "./gatherResponses";
import { parseArgs } from "./parseArgs.js";
import fs from "fs";
import path from "path";

async function exec() {
    const options: Options = await parseArgs();
    const responses: BuildResult[] = await gatherResponses(options);
    const commits: Array<[string, Date]> = [];
    responses.forEach(async (result: BuildResult, idx: number) => {
        result.info.url = `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${result.info.repoBuildNumber})/dist/${options.project}.${options.env}.js`;
        if (typeof result.info.commit === "string" && typeof result.info.commitTime === "string") {
            commits.push([result.info.commit, new Date(result.info.commitTime)]);
        }
        if (options.showInfo) {
            let longestKey = 0;
            if (idx > 0) {
                console.error();
            }

            console.error(
                options.infos
                    .map((key: string) => {
                        if (key === "list") {
                            console.error(`Keys:\n    ${Object.keys(result.info).sort().join("\n    ")}`);
                            return undefined;
                        }
                        if (key === "all") {
                            console.error(JSON.stringify(result.info, null, 4));
                            return undefined;
                        }
                        let val = result.info[key];
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
            const contents = await result.response.text();
            fs.writeFileSync(options.output, contents);
            if (!options.output.startsWith("/dev/std")) {
                if (!options.output.startsWith("/")) {
                    options.output = path.join(process.cwd(), options.output);
                }
                console.error(`Wrote ${result.info.url} to ${options.output}`);
            }
        }
    });

    if (options.showInfo && responses.length === 2 && commits.length === 2) {
        commits.sort((l, r) => l[1].valueOf() - r[1].valueOf()); // oldest first
        const older = commits[0]![0];
        const newer = commits[1]![0];
        console.error(`
git diff ${older}..${newer}
git cherry -v ${older} ${newer}
git log --ancestry-path ${older}..${newer}`);
    }
}

exec();

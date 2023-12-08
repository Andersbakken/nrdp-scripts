import { Build } from "./Build";
import { BuildType } from "./BuildType";
import { Options } from "./Options";
import { findProject } from "./findProject";
import { fromSha } from "./fromSha";
import { isBuild } from "./isBuild";
import { loadFile } from "./loadFile";
import { loadFromUrl } from "./loadFromUrl";
import { parseParent } from "./parseParent";
import { usage } from "./usage";
import { verbose } from "./verbose";
import assert from "assert";
import fs from "fs";

function parseValueArg(long: string, arg: string): string | undefined {
    assert(long.length > 1);
    const short = long[0];
    for (const a of [long, short]) {
        assert(!a.startsWith("-"));
        const prefix = `--${a}=`;
        if (arg.length > prefix.length && arg.startsWith(prefix)) {
            return arg.substring(prefix.length);
        }
    }

    if (arg.length > 2 && arg.startsWith(`-${short}`)) {
        return arg.substring(2);
    }
    return undefined;
}

function createBuild(val: string): Build {
    const tildeIndex = val.indexOf("~");
    let value: string;
    let parent: undefined | number;
    if (tildeIndex !== -1) {
        value = val.substring(0, tildeIndex);
        parent = parseParent(val.substring(tildeIndex));
    } else {
        value = val;
    }

    if (/^[0-9]+$/.exec(value)) {
        return { type: BuildType.BuildNumber, value: parseInt(value), parent };
    }

    if (!value || value.startsWith("master") || value.startsWith("release")) {
        return { type: BuildType.Branch, value, parent };
    }

    return { type: BuildType.Sha, value, parent };
}

export async function parseArgs(): Promise<Options> {
    let env: string = "prod";
    const builds: Build[] = [];
    let project: string | undefined;
    let output: string | undefined;
    let showInfo: boolean = false;
    let infos: string[] = [];
    let fileOrUrl: string | undefined;
    verbose("Parsing args", process.argv);

    for (let i = 2; i < process.argv.length; ++i) {
        const arg = process.argv[i];
        assert(arg !== undefined);
        switch (arg) {
            case "--verbose":
            case "-v":
                break;
            case "--build":
            case "-b":
                builds.push(createBuild(process.argv[++i] ?? ""));
                break;
            case "--project":
            case "-p":
                project = String(process.argv[++i]);
                break;
            case "--file":
            case "-f":
                fileOrUrl = String(process.argv[++i]);
                builds.push(loadFile(fileOrUrl));
                break;
            case "--url":
            case "-u":
                fileOrUrl = String(process.argv[++i]);
                builds.push(await loadFromUrl(fileOrUrl));
                break;
            case "--commit":
            case "-c":
                builds.push(fromSha(String(process.argv[++i])));
                break;
            case "--info":
            case "-i": {
                showInfo = true;
                const val = String(process.argv[i + 1]);
                if (!isBuild(val)) {
                    infos.push(...val.split(","));
                    ++i;
                }
                break;
            }
            case "--stdout":
            case "-1":
                output = "/dev/stdout";
                break;
            case "--stderr":
            case "-2":
                output = "/dev/stderr";
                break;
            case "--output":
            case "-o":
                output = process.argv[++i];
                break;
            case "--help":
            case "-h":
                console.log(usage);
                process.exit(0);
                break;
            case "--download":
            case "-D":
                if (!output) {
                    output = "";
                }
                break;
            case "--env":
            case "-e":
                env = String(process.argv[++i]);
                break;
            case "--prod":
            case "-P":
                env = "prod";
                break;
            case "--prod-assertions":
            case "-a":
                env = "prod.assertions";
                break;
            case "--debug":
            case "-d":
                env = "debug";
                break;
            default: {
                let tmp = parseValueArg("build", arg);
                if (tmp) {
                    builds.push(createBuild(tmp));
                    break;
                }

                tmp = parseValueArg("env", arg);
                if (tmp) {
                    env = tmp;
                    break;
                }

                tmp = parseValueArg("project", arg);
                if (tmp) {
                    project = tmp;
                    break;
                }

                tmp = parseValueArg("output", arg);
                if (tmp) {
                    output = tmp;
                    break;
                }

                tmp = parseValueArg("commit", arg);
                if (tmp) {
                    builds.push(fromSha(tmp));
                    break;
                }

                tmp = parseValueArg("file", arg);
                if (tmp) {
                    fileOrUrl = tmp;
                    builds.push(loadFile(fileOrUrl));
                    break;
                }

                tmp = parseValueArg("url", arg);
                if (tmp) {
                    fileOrUrl = tmp;
                    builds.push(await loadFromUrl(fileOrUrl));
                    break;
                }

                tmp = parseValueArg("info", arg);
                if (tmp) {
                    showInfo = true;
                    infos.push(...tmp.split(","));
                    break;
                }

                if (isBuild(arg)) {
                    builds.push(createBuild(arg));
                    break;
                }
                console.error(`${usage}\nUnknown argument "${arg}"`);
                process.exit(1);
                break;
            }
        }
    }
    if (!builds.length) {
        builds.push({ type: BuildType.Branch, value: "master" });
    }

    if (!showInfo && output === undefined) {
        showInfo = true;
    }
    if (showInfo) {
        let addStandard = infos.length === 0;
        infos = infos.map((x) => {
            if (x.startsWith("+")) {
                addStandard = true;
                return x.substring(1);
            }
            return x;
        });
        if (addStandard) {
            [
                "insertTime",
                "commitTime",
                "buildId",
                "submitter",
                "pointingRefs",
                "rev",
                "commit",
                "repoBuildNumber",
                "url"
            ].forEach((x) => {
                if (!infos.includes(x)) {
                    infos.unshift(x);
                }
            });
        }
    }

    if (!project) {
        if (fileOrUrl) {
            if (fileOrUrl.includes("milo")) {
                project = "milo";
            } else if (fileOrUrl.includes("poby")) {
                project = "poby";
            } else if (fileOrUrl.includes("bogart")) {
                project = "bogart";
            }
        }
        if (!project) {
            project = findProject();
        }
    }

    if (output === "") {
        output = `${project}.${env}.js`;
        if (fs.existsSync(output)) {
            let idx = 0;
            do {
                output = `${project}.${env}.js.${++idx}`;
            } while (fs.existsSync(output));
        }
    }

    return {
        builds,
        env,
        infos,
        output,
        project,
        showInfo,
        verbose
    };
}

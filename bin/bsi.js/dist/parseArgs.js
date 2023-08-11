"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseArgs = void 0;
const findProject_1 = require("./findProject");
const isBuild_1 = require("./isBuild");
const loadFile_1 = require("./loadFile");
const loadFromUrl_1 = require("./loadFromUrl");
const usage_1 = require("./usage");
const verbose_1 = require("./verbose");
const assert_1 = __importDefault(require("assert"));
function parseValueArg(long, arg) {
    (0, assert_1.default)(long.length > 1);
    const short = long[0];
    for (const a of [long, short]) {
        (0, assert_1.default)(!a.startsWith("-"));
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
async function parseArgs() {
    let env = "prod";
    let builds = ["master"];
    let project;
    let output;
    let showInfo = false;
    let infos = [];
    let parentCount = 0;
    let fileOrUrl;
    (0, verbose_1.verbose)("Parsing args", process.argv);
    for (let i = 2; i < process.argv.length; ++i) {
        const arg = process.argv[i];
        switch (arg) {
            case "--verbose":
            case "-v":
                break;
            case "--build":
            case "-b":
                builds = [process.argv[++i]];
                break;
            case "--project":
            case "-p":
                project = process.argv[++i];
                break;
            case "--file":
            case "-f":
                fileOrUrl = process.argv[++i];
                builds = (0, loadFile_1.loadFile)(fileOrUrl);
                break;
            case "--url":
            case "-u":
                fileOrUrl = process.argv[++i];
                builds = await (0, loadFromUrl_1.loadFromUrl)(fileOrUrl);
                break;
            case "--commit":
            case "-c": {
                const commit = process.argv[++i];
                builds = [`(commit="${commit}")`, `(rev="${commit}")`];
                break;
            }
            case "--info":
            case "-i": {
                showInfo = true;
                const val = process.argv[i + 1];
                if (!(0, isBuild_1.isBuild)(val)) {
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
                console.log(usage_1.usage);
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
                env = process.argv[++i];
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
                    builds = [tmp];
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
                    builds = [`(commit="${tmp}")`, `(rev="${tmp}")`];
                    break;
                }
                tmp = parseValueArg("file", arg);
                if (tmp) {
                    fileOrUrl = tmp;
                    builds = (0, loadFile_1.loadFile)(fileOrUrl);
                    break;
                }
                tmp = parseValueArg("url", arg);
                if (tmp) {
                    fileOrUrl = tmp;
                    builds = await (0, loadFromUrl_1.loadFromUrl)(fileOrUrl);
                    break;
                }
                tmp = parseValueArg("info", arg);
                if (tmp) {
                    showInfo = true;
                    infos.push(...tmp.split(","));
                    break;
                }
                if ((0, isBuild_1.isBuild)(arg)) {
                    builds = [arg];
                    break;
                }
                console.error(`${usage_1.usage}\nUnknown argument "${arg}"`);
                process.exit(1);
                break;
            }
        }
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
                "variantBuildNumber",
                "url"
            ].forEach((x) => {
                if (!infos.includes(x)) {
                    infos.unshift(x);
                }
            });
        }
    }
    const parsed = builds.length === 1 && (0, isBuild_1.isBuild)(String(builds[0]));
    if (parsed && parsed[1]) {
        while (parentCount < parsed[1].length) {
            if (parsed[1][parentCount] !== "~") {
                break;
            }
            ++parentCount;
        }
        if (parentCount !== parsed[1].length) {
            parentCount += parseInt(parsed[1].substring(parentCount)) - 1;
        }
        if (/^[0-9]+$/.exec(parsed[0])) {
            builds = [String(parseInt(parsed[0]) - parentCount)];
            parentCount = 0;
        }
        else {
            builds = [parsed[0]];
        }
    }
    if (!project) {
        if (fileOrUrl) {
            if (fileOrUrl.includes("milo")) {
                project = "milo";
            }
            else if (fileOrUrl.includes("poby")) {
                project = "poby";
            }
            else if (fileOrUrl.includes("bogart")) {
                project = "bogart";
            }
        }
        if (!project) {
            project = (0, findProject_1.findProject)();
        }
    }
    if (output === "") {
        output = `${project}.${env}.js`;
    }
    return {
        builds,
        env,
        infos,
        output,
        parentCount,
        project,
        showInfo,
        verbose: verbose_1.verbose
    };
}
exports.parseArgs = parseArgs;

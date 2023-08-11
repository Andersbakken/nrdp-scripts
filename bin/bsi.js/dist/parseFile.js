"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseFile = void 0;
const verbose_1 = require("./verbose");
function parseFile(url, contents) {
    try {
        let start = contents.indexOf('build_sha="');
        if (start === -1) {
            start = contents.indexOf('build_sha = "');
            if (start === -1) {
                throw new Error(`Couldn't find build_sha in ${url}`);
            }
            start += 13;
        }
        else {
            start += 11;
        }
        const end = contents.indexOf('"', start);
        const substr = contents.substring(start, end);
        (0, verbose_1.verbose)("Found build_sha in file", substr);
        return [`(commit="${substr}")`, `(rev="${substr}")`];
    }
    catch (err) {
        console.error(err.message);
        process.exit();
        return [];
    }
}
exports.parseFile = parseFile;

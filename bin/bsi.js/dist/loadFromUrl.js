"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.loadFromUrl = void 0;
const load_1 = require("./load");
const parseFile_1 = require("./parseFile");
const verbose_1 = require("./verbose");
async function loadFromUrl(url) {
    (0, verbose_1.verbose)("loadFromUrl", url);
    const response = await (0, load_1.load)(url);
    if (response.status !== 200) {
        throw new Error("Failed to load url " + url);
    }
    const text = await response.text();
    return (0, parseFile_1.parseFile)(url, text);
}
exports.loadFromUrl = loadFromUrl;

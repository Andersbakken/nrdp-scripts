"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.verbose = void 0;
let enabled;
function verbose(...args) {
    if (enabled === undefined) {
        enabled = process.argv.find((x) => x === "-v" || x === "--verbose") !== undefined;
    }
    if (enabled) {
        console.error(...args);
    }
}
exports.verbose = verbose;

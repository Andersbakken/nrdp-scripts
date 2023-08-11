"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.load = void 0;
const GotToResponse_1 = require("./GotToResponse");
const verbose_1 = require("./verbose");
const got_1 = __importDefault(require("got"));
async function load(url) {
    (0, verbose_1.verbose)("load", url);
    if (Array.isArray(url)) {
        let lastError;
        for (let idx = 0; idx < url.length; ++idx) {
            const u = url[idx];
            try {
                const response = await load(u);
                if (response.status === 200) {
                    return response;
                }
            }
            catch (err) {
                lastError = err;
            }
        }
        if (!lastError) {
            lastError = new Error(`Failed to fetch url(s) ${url.join(", ")}`);
        }
        throw lastError;
    }
    const response = await got_1.default.get(url);
    return new GotToResponse_1.GotToResponse(url, response);
}
exports.load = load;

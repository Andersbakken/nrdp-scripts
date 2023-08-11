"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.GotToResponse = void 0;
const verbose_1 = require("./verbose");
class GotToResponse {
    constructor(url, response) {
        this.url = url;
        this.response = response;
        this.status = response.statusCode;
        (0, verbose_1.verbose)("Got response", url, response.statusCode);
    }
    text() {
        return Promise.resolve(this.response.body);
    }
    json() {
        if (this.parsedJson === undefined) {
            this.parsedJson = JSON.parse(this.response.body);
        }
        return Promise.resolve(this.parsedJson);
    }
}
exports.GotToResponse = GotToResponse;

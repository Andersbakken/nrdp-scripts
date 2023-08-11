"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.isBuild = void 0;
const assert_1 = __importDefault(require("assert"));
function isBuild(arg) {
    const match = /^([0-9]+)(~+[0-9]*)?$/.exec(arg) ||
        /^(master)(~+[0-9]*)?$/.exec(arg) ||
        /^([0-9a-z]{32})(~+[0-9]*)?$/.exec(arg);
    if (match) {
        (0, assert_1.default)(match[1]);
        return [match[1], match[2]];
    }
    return undefined;
}
exports.isBuild = isBuild;

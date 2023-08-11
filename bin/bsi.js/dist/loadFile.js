"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.loadFile = void 0;
const parseFile_1 = require("./parseFile");
const fs_1 = __importDefault(require("fs"));
function loadFile(file) {
    let contents;
    try {
        contents = fs_1.default.readFileSync(file, "utf8");
    }
    catch (err) {
        console.error("Failed to read file", file, err.message);
        process.exit(1);
        return [];
    }
    return (0, parseFile_1.parseFile)(file, contents);
}
exports.loadFile = loadFile;

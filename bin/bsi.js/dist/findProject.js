"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.findProject = void 0;
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
function findProject() {
    let root = process.cwd();
    let best;
    while (root !== "/") {
        const candidate = path_1.default.join(root, "package.json");
        if (fs_1.default.existsSync(candidate)) {
            best = candidate;
        }
        root = path_1.default.join(root, "..");
    }
    if (best) {
        const contents = JSON.parse(fs_1.default.readFileSync(best, "utf8"));
        if (typeof contents.name === "string") {
            return contents.name;
        }
    }
    return "milo";
}
exports.findProject = findProject;

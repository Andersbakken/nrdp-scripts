import fs from "fs";
import path from "path";

export function findProject(): string {
    let root: string = process.cwd();
    let best: string | undefined;
    while (root !== "/") {
        const candidate: string = path.join(root, "package.json");
        if (fs.existsSync(candidate)) {
            best = candidate;
        }
        root = path.join(root, "..");
    }

    if (best) {
        const contents = JSON.parse(fs.readFileSync(best, "utf8"));
        if (typeof contents.name === "string") {
            return contents.name;
        }
    }
    return "milo";
}

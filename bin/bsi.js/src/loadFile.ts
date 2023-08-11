import { parseFile } from "./parseFile";
import fs from "fs";

export function loadFile(file: string): string[] {
    let contents: string;
    try {
        contents = fs.readFileSync(file, "utf8");
    } catch (err: unknown) {
        console.error("Failed to read file", file, (err as Error).message);
        process.exit(1);
        return [];
    }

    return parseFile(file, contents);
}

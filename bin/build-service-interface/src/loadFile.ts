import { Build } from "./Build";
import { parseFile } from "./parseFile";
import assert from "assert";
import fs from "fs";

export function loadFile(file: string): Build {
    let contents: string;
    try {
        contents = fs.readFileSync(file, "utf8");
    } catch (err: unknown) {
        assert(err instanceof Error);
        throw new Error(`Failed to read file ${file} ${err.message}`);
    }

    return parseFile(file, contents);
}

import { Build } from "./Build";
import { fromSha } from "./fromSha";
import { verbose } from "./verbose";
import assert from "assert";

export function parseFile(url: string, contents: string): Build {
    try {
        let start = contents.indexOf('build_sha="');
        if (start === -1) {
            let offset = 13;
            start = contents.indexOf('build_sha = "');
            if (start === -1) {
                start = contents.indexOf('sha = "');
                offset = 7;
            }
            if (start === -1) {
                throw new Error(`Couldn't find build_sha in ${url}`);
            }
            start += offset;
        } else {
            start += 11;
        }
        const end = contents.indexOf('"', start);
        const substr = contents.substring(start, end);
        verbose("Found build_sha in file", substr);
        return fromSha(substr);
    } catch (err: unknown) {
        assert(err instanceof Error);
        throw new Error(`Failed to parse file ${url} ${err.message}`);
    }
}

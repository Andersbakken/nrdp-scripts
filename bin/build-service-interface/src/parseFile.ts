import { fromSha } from "./fromSha";
import { verbose } from "./verbose";

export function parseFile(url: string, contents: string): string[] {
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
        console.error((err as Error).message);
        process.exit();
        return [];
    }
}

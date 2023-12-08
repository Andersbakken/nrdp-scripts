import { Build } from "./Build";
import { load } from "./load";
import { parseFile } from "./parseFile";
import { verbose } from "./verbose";
import assert from "assert";

export async function loadFromUrl(url: string): Promise<Build> {
    try {
        verbose("loadFromUrl", url);
        const response = await load(url);
        if (response.status !== 200) {
            throw new Error("Failed to load url " + url);
        }

        const text = await response.text();
        return parseFile(url, text);
    } catch (err: unknown) {
        assert(err instanceof Error);
        throw new Error(`Failed to loadFromUrl ${url} ${err.message}`);
    }
}

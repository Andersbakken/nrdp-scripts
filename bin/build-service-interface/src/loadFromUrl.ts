import { load } from "./load";
import { parseFile } from "./parseFile";
import { verbose } from "./verbose";

export async function loadFromUrl(url: string): Promise<string[]> {
    try {
        verbose("loadFromUrl", url);
        const response = await load(url);
        if (response.status !== 200) {
            throw new Error("Failed to load url " + url);
        }

        const text = await response.text();
        return parseFile(url, text);
    } catch (err: unknown) {
        console.error("Failed to loadFromUrl", (err as Error).message);
        process.exit(1);
        return Promise.resolve([]);
    }
}

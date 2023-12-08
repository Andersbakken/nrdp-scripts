import { GotToResponse } from "./GotToResponse";
import { LoadResponse } from "./LoadResponse";
import { verbose } from "./verbose";
import assert from "assert";
import got from "got";

export async function load(url: string): Promise<LoadResponse> {
    verbose("load", url);
    try {
        const response = await got.get(url);
        if (response.statusCode !== 200) {
            throw new Error(`statusCode != 200 (${response.statusCode})`);
        }
        return new GotToResponse(url, response);
    } catch (err: unknown) {
        assert(err instanceof Error);
        throw new Error(`Failed to fetch ${url} ${err.message}`);
    }
}

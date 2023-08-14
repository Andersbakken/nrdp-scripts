import { GotToResponse } from "./GotToResponse";
import { LoadResponse } from "./LoadResponse";
import { verbose } from "./verbose";
import got from "got";

export async function load(url: string | string[]): Promise<LoadResponse> {
    verbose("load", url);
    if (Array.isArray(url)) {
        let lastError: Error | undefined;
        for (let idx = 0; idx < url.length; ++idx) {
            const u = url[idx];
            try {
                const response = await load(u);
                if (response.status === 200) {
                    return response;
                }
            } catch (err: unknown) {
                lastError = err as Error;
            }
        }
        if (!lastError) {
            lastError = new Error(`Failed to fetch url(s) ${url.join(", ")}`);
        }
        throw lastError;
    }

    try {
        const response = await got.get(url);
        return new GotToResponse(url, response);
    } catch (err: unknown) {
        throw new Error(`Failed to fetch ${url} ${(err as Error).message}`);
    }
}

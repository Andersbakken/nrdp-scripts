import { Build } from "./Build";
import { LoadResponse } from "./LoadResponse";
import { Options } from "./Options";
import { load } from "./load";

export async function loadBySha(options: Options, build: Build): Promise<LoadResponse> {
    let response: LoadResponse;
    try {
        response = await load(`https://build.dta.netflix.com/nrdp/${options.project}/(commit=${build.value})`);
    } catch (err) {
        response = await load(`https://build.dta.netflix.com/nrdp/${options.project}/(rev=${build.value})`);
    }

    if (build.parent) {
        const info: Record<string, unknown> = (await response.json()) as Record<string, unknown>;
        const url = `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${
            parseInt(String(info.repoBuildNumber)) - build.parent
        })`;
        response = await load(url);
    }
    return Promise.resolve(response);
}

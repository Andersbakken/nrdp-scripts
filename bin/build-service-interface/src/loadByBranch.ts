import { Build } from "./Build";
import { LoadResponse } from "./LoadResponse";
import { Options } from "./Options";
import { load } from "./load";

export async function loadByBranch(options: Options, build: Build): Promise<LoadResponse> {
    if (!build.value || build.value === "master") {
        const response = await load(`https://build.dta.netflix.com/nrdp/${options.project}/`);
        if (build.parent) {
            const info: Record<string, unknown> = (await response.json()) as Record<string, unknown>;
            const url = `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${
                parseInt(String(info.repoBuildNumber)) - build.parent
            })`;
            return load(url);
        }
        return Promise.resolve(response);
    }
    throw new Error("WTF to do with this? " + JSON.stringify(build));
}

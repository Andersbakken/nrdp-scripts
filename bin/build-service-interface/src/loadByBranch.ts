import { Build } from "./Build";
import { BuildResult } from "./BuildResult";
import { Options } from "./Options";
import { load } from "./load";

export async function loadByBranch(options: Options, build: Build): Promise<BuildResult> {
    if (!build.value || build.value === "master") {
        let response = await load(`https://build.dta.netflix.com/nrdp/${options.project}/`);
        if (build.parent) {
            const info: Record<string, unknown> = (await response.json()) as Record<string, unknown>;
            const url = `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${
                parseInt(String(info.repoBuildNumber)) - build.parent
            })`;
            response = await load(url);
        }
        const info = (await response.json()) as Record<string, unknown>;
        return { response, info };
    }
    throw new Error("WTF to do with this? " + JSON.stringify(build));
}

import { Build } from "./Build";
import { BuildResult } from "./BuildResult";
import { Options } from "./Options";
import { load } from "./load";
import assert from "assert";

export async function loadByBuildNumber(options: Options, build: Build): Promise<BuildResult> {
    assert(typeof build.value === "number");
    const response = await load(
        `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${build.value - (build.parent ?? 0)})`
    );
    const info = (await response.json()) as Record<string, unknown>;
    return { response, info };
}

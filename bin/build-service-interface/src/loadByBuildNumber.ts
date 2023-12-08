import { Build } from "./Build";
import { LoadResponse } from "./LoadResponse";
import { Options } from "./Options";
import { load } from "./load";
import assert from "assert";

export function loadByBuildNumber(options: Options, build: Build): Promise<LoadResponse> {
    assert(typeof build.value === "number");
    return load(
        `https://build.dta.netflix.com/nrdp/${options.project}/(repoBuildNumber=${build.value - (build.parent ?? 0)})`
    );
}

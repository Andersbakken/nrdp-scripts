import { Build } from "./Build";
import { BuildType } from "./BuildType";
import { LoadResponse } from "./LoadResponse";
import { Options } from "./Options";
import { loadByBranch } from "./loadByBranch";
import { loadByBuildNumber } from "./loadByBuildNumber";
import { loadBySha } from "./loadBySha";

export async function gatherResponses(options: Options): Promise<LoadResponse[]> {
    const promises: Promise<LoadResponse>[] = options.builds.map((x: Build) => {
        switch (x.type) {
            case BuildType.BuildNumber:
                return loadByBuildNumber(options, x);
            case BuildType.Branch:
                return loadByBranch(options, x);
            case BuildType.Sha:
                break;
        }
        return loadBySha(options, x);
    });
    return Promise.all(promises);
}

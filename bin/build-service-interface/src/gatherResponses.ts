import { Build } from "./Build";
import { BuildResult } from "./BuildResult";
import { BuildType } from "./BuildType";
import { Options } from "./Options";
import { loadByBranch } from "./loadByBranch";
import { loadByBuildNumber } from "./loadByBuildNumber";
import { loadBySha } from "./loadBySha";

export function gatherResponses(options: Options): Promise<BuildResult[]> {
    const promises: Promise<BuildResult>[] = options.builds.map((x: Build) => {
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

import { Build } from "./Build";
import { BuildType } from "./BuildType";

export function fromSha(sha: string): Build {
    return { type: BuildType.Sha, value: sha };
}

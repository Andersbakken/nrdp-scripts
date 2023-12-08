import { BuildType } from "./BuildType";

export interface Build {
    type: BuildType;
    value: number | string;
    parent?: number;
}

import { Build } from "./Build";

export interface Options {
    builds: Build[];
    env: string;
    infos: string[];
    output: string | undefined;
    project: string;
    showInfo: boolean;
    verbose: (...args: unknown[]) => void;
}

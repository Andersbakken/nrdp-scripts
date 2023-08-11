export interface Options {
    builds: string[];
    env: string;
    infos: string[];
    output: string | undefined;
    parentCount: number;
    project: string;
    showInfo: boolean;
    verbose: (...args: unknown[]) => void;
}

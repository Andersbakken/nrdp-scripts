import assert from "assert";

export function isBuild(arg: string): [string, string | undefined] | undefined {
    const match =
        /^([0-9]+)(~+[0-9]*)?$/.exec(arg) ||
        /^(master)(~+[0-9]*)?$/.exec(arg) ||
        /^([0-9a-z]{32})(~+[0-9]*)?$/.exec(arg);
    if (match) {
        assert(match[1]);
        return [match[1], match[2]];
    }
    return undefined;
}

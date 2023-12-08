export function parseParent(val: string): number {
    const split = val.split("~");
    if (!split[split.length - 1]) {
        --split.length;
    }
    let ret = 0;
    split.forEach((v: string) => {
        if (v === "") {
            ++ret;
        } else {
            ret += parseInt(v) - 1;
        }
    });

    return ret;
}

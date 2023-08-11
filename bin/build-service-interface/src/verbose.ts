let enabled: boolean | undefined;
export function verbose(...args: unknown[]): void {
    if (enabled === undefined) {
        enabled = process.argv.find((x: string) => x === "-v" || x === "--verbose") !== undefined;
    }
    if (enabled) {
        console.error(...args);
    }
}

export function fromSha(sha: string): [string, string] {
    return [`(commit="${sha}",buildTags="master")`, `(rev="${sha}",buildTags="master")`];
}

---
name: lsdev
description: |
  Use when navigating between C/C++ source and build trees with lsdev.pl.
  Activates whenever the user asks about, or you need to discover, the source tree corresponding to a build tree (or vice versa), list the shadow / out-of-tree build trees of a project, or identify the project name of the current directory.
  Keywords:
  - lsdev, lsdev.pl
  - build tree, source tree, shadow build, out-of-tree build
  - CMakeCache.txt, config.status, configure, .lsdev_config, .lsdev_shadows, .lsdev_default
  - ~/.dev_directories
  - "where is the source for this build", "which builds exist for this source"
user-invocable: true
allowed_tools: Bash
---

# lsdev — find related source / build trees

`lsdev.pl` is a personal tool that, given any directory, identifies the project it belongs to and enumerates the *related* source trees and shadow/out-of-tree build trees. Use it instead of guessing from path conventions.

## Mental model

A directory is one of:
- **source tree** — has `configure`, `CMakeLists.txt`, `.lsdev_shadows`, `.lsdev_config`, or `.git` at its root.
- **build tree** — has `CMakeCache.txt`, `config.status`, or a build-side `.lsdev_config`. The corresponding source tree is recorded inside (`CMAKE_HOME_DIRECTORY` in `CMakeCache.txt`, the path to `configure` in `config.status`, or `source=` in `.lsdev_config`).

Named roots also live in `~/.dev_directories` (sections like `[path]` plus a top-level `builds=` listing build-root parents). `lsdev.pl` reads that file plus per-tree `.lsdev_shadows` files to discover related trees.

Output naming: source trees are emitted as `src_<name>`, build trees as `build_<name>` (often `build_<src>_<build>`).

## Invocation rules

- **Always pass `-l`** when scripting. Without it, `lsdev.pl` will prompt interactively when there is more than one match.
- **Capture stdout only.** Selections go to stdout; the interactive menu and verbose progress go to stderr.
- **Use `-c <path>`** to query about a directory without `cd`-ing into it.
- **Filtering tokens** (positional args after the flags) all must match. Useful tokens:
  - `src` / `source` — only source trees
  - `build` — only build trees
  - `path:<regex>` — match against the path
  - `<name>` — substring/word match against the generated `src_…` / `build_…` name
  - `-<token>` — invert (exclude)
- Match modes: default tries word → regexp → ido in order. Force exact match with `-me`.

## The recipes you'll actually use

| Goal | Command | Notes |
|---|---|---|
| Project name of cwd (and is-source-or-build) | `lsdev.pl -p` | Prints e.g. `src_jamba-dpi` or `build_jamba-dpi_jamba_release_gl_26.1` |
| From a build tree → its source tree path | `lsdev.pl -tS -r` | `-r` returns the root, not a sub-path |
| From cwd → the matching root path | `lsdev.pl -tp -r` | `-tp` (path) is default but explicit is fine |
| List build trees related to current source | `lsdev.pl -l build` |  |
| List source trees related to current build | `lsdev.pl -l src` |  |
| List everything related to cwd | `lsdev.pl -l` |  |
| List every known src + build (system-wide) | `lsdev.pl -l -a` | Includes unrelated projects |
| Filter by name fragment | `lsdev.pl -l build 26.1` | All filter tokens must match |
| Resolve a different directory | `lsdev.pl -c <path> -p` | Or any other recipe with `-c` prepended |

## Output shape for `-l`

```
<generated-name> [<path>]              # source tree
<generated-name> [<path>] [<src-name>] # build tree (third bracket = its source)
```

Parse by splitting on `] [` after the first `[`. The leading `src_` / `build_` on the name is a reliable type tag.

## Recovering the source from a build tree without `lsdev.pl`

If `lsdev.pl` is unavailable, the same data lives in:
- `CMakeCache.txt` line `CMAKE_HOME_DIRECTORY:INTERNAL=<path>`
- `config.status` — search for a `…/configure` path
- `.lsdev_config` — `source=<path>` line

Prefer `lsdev.pl -tS -r` when it's on PATH; only fall back to parsing these files directly if it isn't.

## Things that will burn you

- `--help` does nothing useful; `-h` prints the help.
- Without `-l`, a multi-match query blocks on stdin.
- `-r` / "root" matters: without it, `lsdev.pl` tries to translate the cwd's *sub-path* into the answer tree (handy in shells, confusing in scripts).
- Names are not unique across `-a`; rely on the path in the second `[…]` to disambiguate.
- Stderr can be noisy with `-v`; never pipe stderr into your parser.

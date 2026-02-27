#!/usr/bin/env python3
"""Scan ~/.claude/projects/ for orphaned .jsonl session files and add them to sessions-index.json.

This makes sessions created via claude-code-acp (agent-shell) visible to 'claude --resume'.
"""

import argparse
import json
import os
import sys
from pathlib import Path


CLAUDE_PROJECTS_DIR = Path.home() / ".claude" / "projects"


def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument(
        "--project-dir",
        type=Path,
        help="Repair a single project directory instead of all projects",
    )
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be added without writing",
    )
    return p.parse_args()


def extract_first_user_prompt(jsonl_path: Path) -> dict:
    """Read a .jsonl session file and extract metadata for the index entry."""
    session_id = jsonl_path.stem
    first_prompt = "No prompt"
    created = None
    modified = None
    cwd = None
    git_branch = ""
    message_count = 0
    is_sidechain = False

    lines = []
    try:
        with open(jsonl_path, "r") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                lines.append(line)
    except (OSError, UnicodeDecodeError):
        return None

    if not lines:
        return None

    for raw in lines:
        try:
            obj = json.loads(raw)
        except json.JSONDecodeError:
            continue

        obj_type = obj.get("type", "")
        if obj_type in ("queue-operation", "file-history-snapshot", "progress"):
            continue

        message_count += 1
        ts = obj.get("timestamp")

        if created is None and ts:
            created = ts
        if ts:
            modified = ts

        if cwd is None:
            cwd = obj.get("cwd")
        if not git_branch:
            git_branch = obj.get("gitBranch", "")

        if obj.get("sessionId"):
            session_id = obj["sessionId"]

        if obj.get("isSidechain"):
            is_sidechain = True

        # Extract first user prompt text
        if first_prompt == "No prompt" and obj_type == "user":
            if obj.get("isMeta"):
                continue
            msg = obj.get("message", {})
            if msg.get("role") != "user":
                continue
            content = msg.get("content", "")
            text = _extract_text(content)
            if text and not text.startswith("<"):
                first_prompt = text[:200]

    if not created:
        # Fallback to file mtime
        stat = jsonl_path.stat()
        from datetime import datetime, timezone
        created = datetime.fromtimestamp(stat.st_mtime, tz=timezone.utc).isoformat()
        modified = created

    file_mtime = int(jsonl_path.stat().st_mtime * 1000)

    return {
        "sessionId": session_id,
        "fullPath": str(jsonl_path),
        "fileMtime": file_mtime,
        "firstPrompt": first_prompt,
        "summary": first_prompt[:60] if first_prompt != "No prompt" else "No prompt",
        "messageCount": message_count,
        "created": created,
        "modified": modified,
        "gitBranch": git_branch or "",
        "projectPath": cwd or "",
        "isSidechain": is_sidechain,
    }


def _extract_text(content) -> str:
    """Extract plain text from message content (string or content blocks array)."""
    if isinstance(content, str):
        return content.strip()
    if isinstance(content, list):
        for block in content:
            if isinstance(block, dict):
                if block.get("type") == "tool_result":
                    return ""  # Skip tool result messages
                if block.get("type") == "text":
                    return block.get("text", "").strip()
            elif isinstance(block, str):
                return block.strip()
    return ""


def repair_project_dir(project_dir: Path, dry_run: bool) -> int:
    """Repair sessions-index.json for a single project directory. Returns count of added entries."""
    index_path = project_dir / "sessions-index.json"

    # Load existing index
    if index_path.exists():
        try:
            with open(index_path) as f:
                index_data = json.load(f)
        except (json.JSONDecodeError, OSError):
            print(f"  Warning: corrupt {index_path}, creating fresh", file=sys.stderr)
            index_data = {"version": 1, "entries": []}
    else:
        index_data = {"version": 1, "entries": []}

    indexed_ids = {e["sessionId"] for e in index_data.get("entries", [])}

    # Find all .jsonl files
    jsonl_files = sorted(project_dir.glob("*.jsonl"))
    added = 0

    for jsonl_path in jsonl_files:
        session_id = jsonl_path.stem
        if session_id in indexed_ids:
            continue

        entry = extract_first_user_prompt(jsonl_path)
        if entry is None:
            continue

        if dry_run:
            print(f"  Would add: {entry['sessionId']}")
            print(f"    Prompt: {entry['firstPrompt'][:80]}")
            print(f"    Created: {entry['created']}")
        else:
            index_data.setdefault("entries", []).append(entry)

        added += 1

    if added > 0 and not dry_run:
        # Derive originalPath from the directory name if not set
        if "originalPath" not in index_data:
            # -home-smagnuson-foo-bar -> /home/smagnuson/foo/bar
            dir_name = project_dir.name
            if dir_name.startswith("-"):
                original_path = dir_name.replace("-", "/")
            else:
                original_path = "/" + dir_name.replace("-", "/")
            index_data["originalPath"] = original_path

        with open(index_path, "w") as f:
            json.dump(index_data, f, indent=2)
            f.write("\n")

    return added


def main():
    args = parse_args()

    if args.project_dir:
        project_dirs = [args.project_dir]
    else:
        if not CLAUDE_PROJECTS_DIR.exists():
            print(f"No projects directory found at {CLAUDE_PROJECTS_DIR}", file=sys.stderr)
            sys.exit(1)
        project_dirs = sorted(
            p for p in CLAUDE_PROJECTS_DIR.iterdir() if p.is_dir()
        )

    total_added = 0
    for project_dir in project_dirs:
        jsonl_count = len(list(project_dir.glob("*.jsonl")))
        if jsonl_count == 0:
            continue

        index_path = project_dir / "sessions-index.json"
        indexed_count = 0
        if index_path.exists():
            try:
                with open(index_path) as f:
                    indexed_count = len(json.load(f).get("entries", []))
            except (json.JSONDecodeError, OSError):
                pass

        if jsonl_count == indexed_count:
            continue

        print(f"{project_dir.name}: {jsonl_count} sessions, {indexed_count} indexed")
        added = repair_project_dir(project_dir, args.dry_run)
        if added:
            action = "would add" if args.dry_run else "added"
            print(f"  {action} {added} entries")
        total_added += added

    if total_added == 0:
        print("All sessions already indexed.")
    elif not args.dry_run:
        print(f"\nTotal: added {total_added} entries across all projects.")


if __name__ == "__main__":
    main()

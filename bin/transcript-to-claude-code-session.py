#!/usr/bin/env python3
"""Convert agent-shell markdown transcripts into Claude Code .jsonl session files.

Parses .md transcripts from .agent-shell/transcripts/ and generates session files
compatible with 'claude --resume'.
"""

import argparse
import json
import os
import re
import sys
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path


CLAUDE_PROJECTS_DIR = Path.home() / ".claude" / "projects"

# Patterns for parsing transcript markdown
HEADER_STARTED = re.compile(r"\*\*Started:\*\*\s+(.+)")
HEADER_CWD = re.compile(r"\*\*Working Directory:\*\*\s+(.+)")
HEADER_AGENT = re.compile(r"\*\*Agent:\*\*\s+(.+)")
SECTION_USER = re.compile(r"^## User \((\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\)")
SECTION_AGENT = re.compile(r"^## Agent \((\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\)")
TOOL_CALL = re.compile(
    r"^### Tool Call \[(\w+)\]: (\w+)\s+(.*?)(?:\s+\((\d+)\s*-\s*(\d+)\))?$"
)
TOOL_META_TOOL = re.compile(r"^\*\*Tool:\*\*\s+(.+)")
TOOL_META_TS = re.compile(r"^\*\*Timestamp:\*\*\s+(.+)")


@dataclass
class ToolCall:
    status: str
    tool_name: str
    file_path: str
    line_start: int | None
    line_end: int | None
    tool_type: str
    timestamp: str
    output: str


@dataclass
class Turn:
    role: str  # "user" or "assistant"
    timestamp: str
    text: str
    tool_calls: list[ToolCall] = field(default_factory=list)


@dataclass
class Transcript:
    agent: str
    started: str
    cwd: str
    turns: list[Turn] = field(default_factory=list)


def parse_timestamp(ts_str: str) -> str:
    """Convert 'YYYY-MM-DD HH:MM:SS' to ISO-8601 UTC string."""
    try:
        dt = datetime.strptime(ts_str.strip(), "%Y-%m-%d %H:%M:%S")
        dt = dt.replace(tzinfo=timezone.utc)
        return dt.isoformat(timespec="milliseconds").replace("+00:00", "Z")
    except ValueError:
        return ts_str


def parse_transcript(path: Path) -> Transcript:
    """Parse an agent-shell markdown transcript file."""
    lines = path.read_text().splitlines()
    transcript = Transcript(agent="Claude Code", started="", cwd="")

    i = 0
    # Parse header
    while i < len(lines):
        line = lines[i]
        m = HEADER_STARTED.search(line)
        if m:
            transcript.started = parse_timestamp(m.group(1))
        m = HEADER_CWD.search(line)
        if m:
            transcript.cwd = m.group(1).strip().rstrip("/")
        m = HEADER_AGENT.search(line)
        if m:
            transcript.agent = m.group(1).strip()

        if line.startswith("## User") or line.startswith("## Agent"):
            break
        i += 1

    # Parse turns
    while i < len(lines):
        line = lines[i]

        m = SECTION_USER.match(line)
        if m:
            turn = Turn(role="user", timestamp=parse_timestamp(m.group(1)), text="")
            i += 1
            text_lines = []
            while i < len(lines) and not lines[i].startswith("## "):
                text_lines.append(lines[i])
                i += 1
            turn.text = "\n".join(text_lines).strip()
            transcript.turns.append(turn)
            continue

        m = SECTION_AGENT.match(line)
        if m:
            turn = Turn(role="assistant", timestamp=parse_timestamp(m.group(1)), text="")
            i += 1
            text_lines = []
            while i < len(lines):
                if lines[i].startswith("## User") or lines[i].startswith("## Agent"):
                    break
                # Check for tool call
                tm = TOOL_CALL.match(lines[i])
                if tm:
                    turn.text = "\n".join(text_lines).strip()
                    text_lines = []
                    tool = _parse_tool_call(lines, i, tm)
                    turn.tool_calls.append(tool.call)
                    i = tool.next_i
                    # Collect any remaining text after tool calls
                    continue
                text_lines.append(lines[i])
                i += 1
            if text_lines:
                extra = "\n".join(text_lines).strip()
                if turn.text and extra:
                    turn.text += "\n\n" + extra
                elif extra:
                    turn.text = extra
            transcript.turns.append(turn)
            continue

        i += 1

    return transcript


@dataclass
class ToolParseResult:
    call: ToolCall
    next_i: int


def _parse_tool_call(lines: list[str], start: int, match) -> ToolParseResult:
    """Parse a tool call section starting at the ### Tool Call line."""
    status = match.group(1)
    tool_name = match.group(2)
    file_path = match.group(3).strip()
    line_start = int(match.group(4)) if match.group(4) else None
    line_end = int(match.group(5)) if match.group(5) else None

    i = start + 1
    tool_type = ""
    timestamp = ""
    output_lines = []
    in_code_block = False

    while i < len(lines):
        line = lines[i]
        # Stop at next section
        if line.startswith("## User") or line.startswith("## Agent"):
            break
        if line.startswith("### Tool Call"):
            break

        m = TOOL_META_TOOL.match(line)
        if m and not in_code_block:
            tool_type = m.group(1).strip()
            i += 1
            continue
        m = TOOL_META_TS.match(line)
        if m and not in_code_block:
            timestamp = parse_timestamp(m.group(1))
            i += 1
            continue

        if line.startswith("```") and not in_code_block:
            in_code_block = True
            i += 1
            continue
        if line.startswith("```") and in_code_block:
            in_code_block = False
            i += 1
            continue
        if in_code_block:
            output_lines.append(line)
            i += 1
            continue

        i += 1

    return ToolParseResult(
        call=ToolCall(
            status=status,
            tool_name=tool_name,
            file_path=file_path,
            line_start=line_start,
            line_end=line_end,
            tool_type=tool_type,
            timestamp=timestamp,
            output="\n".join(output_lines),
        ),
        next_i=i,
    )


def tool_call_to_acp_name(tool_type: str, tool_name: str) -> str:
    """Map transcript tool name/type to ACP tool name."""
    name_lower = tool_name.lower()
    type_lower = tool_type.lower()
    if name_lower == "read" or type_lower == "read":
        return "mcp__acp__Read"
    if name_lower == "write" or type_lower == "write":
        return "mcp__acp__Write"
    if name_lower == "edit" or type_lower == "edit":
        return "mcp__acp__Edit"
    if name_lower == "bash" or type_lower == "bash":
        return "mcp__acp__Bash"
    if name_lower == "glob" or type_lower == "glob":
        return "mcp__acp__Glob"
    if name_lower == "grep" or type_lower == "grep":
        return "mcp__acp__Grep"
    if name_lower == "task" or type_lower == "task":
        return "mcp__acp__Task"
    # Fallback
    return f"mcp__acp__{tool_name}"


def tool_call_to_input(tc: ToolCall) -> dict:
    """Build the input dict for a tool call based on its type."""
    name_lower = tc.tool_name.lower()
    if name_lower == "read":
        inp = {"file_path": tc.file_path}
        if tc.line_start is not None:
            inp["offset"] = tc.line_start
        if tc.line_start is not None and tc.line_end is not None:
            inp["limit"] = tc.line_end - tc.line_start + 1
        return inp
    if name_lower in ("write", "edit"):
        return {"file_path": tc.file_path}
    if name_lower == "bash":
        return {"command": tc.file_path}
    if name_lower == "glob":
        return {"pattern": tc.file_path}
    if name_lower == "grep":
        return {"pattern": tc.file_path}
    return {"file_path": tc.file_path}


def cwd_to_project_dir_name(cwd: str) -> str:
    """Convert a working directory path to Claude Code project directory name.

    /home/smagnuson/foo/bar -> -home-smagnuson-foo-bar
    /home/smagnuson/nrdp/25.2 -> -home-smagnuson-nrdp-25-2

    Claude Code replaces both / and . with -
    """
    import re
    return re.sub(r"[/.]", "-", cwd)


def transcript_to_jsonl(transcript: Transcript) -> tuple[str, list[str]]:
    """Convert a parsed transcript to JSONL lines. Returns (session_id, lines)."""
    session_id = str(uuid.uuid4())
    jsonl_lines = []
    parent_uuid = None

    for turn in transcript.turns:
        if turn.role == "user":
            msg_uuid = str(uuid.uuid4())
            entry = {
                "parentUuid": parent_uuid,
                "isSidechain": False,
                "userType": "external",
                "cwd": transcript.cwd,
                "sessionId": session_id,
                "version": "2.1.38",
                "gitBranch": "",
                "type": "user",
                "message": {
                    "role": "user",
                    "content": [{"type": "text", "text": turn.text}],
                },
                "uuid": msg_uuid,
                "timestamp": turn.timestamp,
            }
            jsonl_lines.append(json.dumps(entry, separators=(",", ":")))
            parent_uuid = msg_uuid

        elif turn.role == "assistant":
            if turn.tool_calls:
                # Assistant message with text before tool calls + tool_use blocks
                content = []
                if turn.text:
                    content.append({"type": "text", "text": turn.text})

                # Add all tool_use blocks
                tool_ids = []
                for tc in turn.tool_calls:
                    tool_id = f"toolu_{uuid.uuid4().hex[:24]}"
                    tool_ids.append((tool_id, tc))
                    content.append({
                        "type": "tool_use",
                        "id": tool_id,
                        "name": tool_call_to_acp_name(tc.tool_type, tc.tool_name),
                        "input": tool_call_to_input(tc),
                    })

                msg_uuid = str(uuid.uuid4())
                entry = {
                    "parentUuid": parent_uuid,
                    "isSidechain": False,
                    "userType": "external",
                    "cwd": transcript.cwd,
                    "sessionId": session_id,
                    "version": "2.1.38",
                    "gitBranch": "",
                    "message": {
                        "role": "assistant",
                        "content": content,
                    },
                    "type": "assistant",
                    "uuid": msg_uuid,
                    "timestamp": turn.timestamp,
                }
                jsonl_lines.append(json.dumps(entry, separators=(",", ":")))
                parent_uuid = msg_uuid

                # Tool results as a user message
                result_content = []
                for tool_id, tc in tool_ids:
                    result_content.append({
                        "tool_use_id": tool_id,
                        "type": "tool_result",
                        "content": [{"type": "text", "text": tc.output}],
                    })

                result_uuid = str(uuid.uuid4())
                result_entry = {
                    "parentUuid": parent_uuid,
                    "isSidechain": False,
                    "userType": "external",
                    "cwd": transcript.cwd,
                    "sessionId": session_id,
                    "version": "2.1.38",
                    "gitBranch": "",
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": result_content,
                    },
                    "uuid": result_uuid,
                    "timestamp": tc.timestamp or turn.timestamp,
                }
                jsonl_lines.append(json.dumps(result_entry, separators=(",", ":")))
                parent_uuid = result_uuid

            else:
                # Plain assistant text response
                msg_uuid = str(uuid.uuid4())
                entry = {
                    "parentUuid": parent_uuid,
                    "isSidechain": False,
                    "userType": "external",
                    "cwd": transcript.cwd,
                    "sessionId": session_id,
                    "version": "2.1.38",
                    "gitBranch": "",
                    "message": {
                        "role": "assistant",
                        "content": [{"type": "text", "text": turn.text}],
                    },
                    "type": "assistant",
                    "uuid": msg_uuid,
                    "timestamp": turn.timestamp,
                }
                jsonl_lines.append(json.dumps(entry, separators=(",", ":")))
                parent_uuid = msg_uuid

    return session_id, jsonl_lines


def write_session(
    session_id: str, jsonl_lines: list[str], transcript: Transcript, dry_run: bool
) -> Path | None:
    """Write .jsonl file and update sessions-index.json. Returns the output path."""
    dir_name = cwd_to_project_dir_name(transcript.cwd)
    project_dir = CLAUDE_PROJECTS_DIR / dir_name
    jsonl_path = project_dir / f"{session_id}.jsonl"

    if dry_run:
        print(f"  Would write: {jsonl_path}")
        print(f"  Messages: {len(jsonl_lines)}")
        first_user = next(
            (t for t in transcript.turns if t.role == "user"), None
        )
        if first_user:
            print(f"  First prompt: {first_user.text[:80]}")
        return None

    project_dir.mkdir(parents=True, exist_ok=True)
    with open(jsonl_path, "w") as f:
        for line in jsonl_lines:
            f.write(line + "\n")

    # Update sessions-index.json
    index_path = project_dir / "sessions-index.json"
    if index_path.exists():
        try:
            with open(index_path) as f:
                index_data = json.load(f)
        except (json.JSONDecodeError, OSError):
            index_data = {"version": 1, "entries": []}
    else:
        index_data = {"version": 1, "entries": [], "originalPath": transcript.cwd}

    first_user = next((t for t in transcript.turns if t.role == "user"), None)
    first_prompt = first_user.text[:200] if first_user else "No prompt"
    last_turn = transcript.turns[-1] if transcript.turns else None

    index_data["entries"].append({
        "sessionId": session_id,
        "fullPath": str(jsonl_path),
        "fileMtime": int(jsonl_path.stat().st_mtime * 1000),
        "firstPrompt": first_prompt,
        "summary": first_prompt[:60],
        "messageCount": len(jsonl_lines),
        "created": transcript.started,
        "modified": last_turn.timestamp if last_turn else transcript.started,
        "gitBranch": "",
        "projectPath": transcript.cwd,
        "isSidechain": False,
    })

    with open(index_path, "w") as f:
        json.dump(index_data, f, indent=2)
        f.write("\n")

    return jsonl_path


def convert_file(path: Path, dry_run: bool) -> str | None:
    """Convert a single transcript file. Returns session ID on success."""
    print(f"Converting: {path.name}")
    transcript = parse_transcript(path)

    if not transcript.turns:
        print(f"  Skipping: no turns found")
        return None

    session_id, jsonl_lines = transcript_to_jsonl(transcript)
    result = write_session(session_id, jsonl_lines, transcript, dry_run)

    if not dry_run and result:
        print(f"  Created: {result}")
        print(f"  Session ID: {session_id}")
    return session_id


def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("path", nargs="?", type=Path, help="Transcript .md file to convert")
    p.add_argument(
        "--dir", type=Path, help="Convert all .md transcripts in a directory"
    )
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be created without writing",
    )
    return p.parse_args()


def main():
    args = parse_args()

    if not args.path and not args.dir:
        print("Error: provide a transcript path or --dir", file=sys.stderr)
        sys.exit(1)

    files = []
    if args.dir:
        files = sorted(args.dir.glob("*.md"))
        if not files:
            print(f"No .md files found in {args.dir}", file=sys.stderr)
            sys.exit(1)
    elif args.path:
        if not args.path.exists():
            print(f"File not found: {args.path}", file=sys.stderr)
            sys.exit(1)
        files = [args.path]

    for f in files:
        convert_file(f, args.dry_run)


if __name__ == "__main__":
    main()

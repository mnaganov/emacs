#!/usr/bin/env python3
"""Convert Jetski transcript.jsonl into a clean Markdown document."""

import argparse
import io
import json
import os
import re
import subprocess
import sys


def extract_user_text(raw_content: str, clean_tags: bool = True) -> str:
  """Extracts clean user prompt, stripping <USER_REQUEST> wrapper if present."""
  if clean_tags and "<USER_REQUEST>" in raw_content:
    match = re.search(r"<USER_REQUEST>(.*?)</USER_REQUEST>", raw_content, re.DOTALL)
    if match:
      return match.group(1).strip()
  return raw_content.strip()


def convert_transcript_to_markdown(
    input_stream,
    output_stream,
    clean_user_tags: bool = True,
    include_metadata: bool = False,
):
  for line in input_stream:
    line = line.strip()
    if not line:
      continue

    try:
      step = json.loads(line)
    except json.JSONDecodeError:
      continue

    source = step.get("source")
    step_type = step.get("type")
    content = step.get("content", "")
    created_at = step.get("created_at", "")
    step_idx = step.get("step_index", "")

    # 1. User Input
    if source == "USER_EXPLICIT" and step_type == "USER_INPUT" and content:
      user_text = extract_user_text(content, clean_tags=clean_user_tags)
      output_stream.write("## 👤 User\n\n")
      if include_metadata and (created_at or step_idx != ""):
        output_stream.write(f"> *Step {step_idx} • {created_at}*\n\n")
      output_stream.write(f"{user_text}\n\n---\n\n")

    # 2. Model Planner Response
    elif (
        source == "MODEL"
        and step_type == "PLANNER_RESPONSE"
        and content.strip()
    ):
      output_stream.write("## 🤖 Assistant\n\n")
      if include_metadata and (created_at or step_idx != ""):
        output_stream.write(f"> *Step {step_idx} • {created_at}*\n\n")
      output_stream.write(f"{content.strip()}\n\n---\n\n")


def main():
  parser = argparse.ArgumentParser(
      description="Convert Jetski transcript.jsonl to clean Markdown."
  )
  parser.add_argument(
      "input_file",
      nargs="?",
      default="-",
      help="Path to transcript.jsonl (reads from stdin if omitted or '-')",
  )
  parser.add_argument(
      "-o",
      "--output",
      default=None,
      help="Path to output markdown file (defaults to stdout)",
  )
  parser.add_argument(
      "--raw-user-prompt",
      action="store_true",
      help="Keep raw <USER_REQUEST> XML tags in user prompt instead of stripping them",
  )
  parser.add_argument(
      "--metadata",
      action="store_true",
      help="Include step indices and timestamps in markdown headers",
  )

  args = parser.parse_args()

  # Input stream
  if args.input_file == "-":
    in_f = sys.stdin
  else:
    in_f = open(args.input_file, "r", encoding="utf-8")

  # Output stream
  if args.output:
    out_f = open(args.output, "w", encoding="utf-8")
  else:
    out_f = sys.stdout

  try:
    # Buffer the markdown output
    buffer = io.StringIO()
    convert_transcript_to_markdown(
        in_f,
        buffer,
        clean_user_tags=not args.raw_user_prompt,
        include_metadata=args.metadata,
    )

    markdown_content = buffer.getvalue()

    script_dir = os.path.dirname(os.path.abspath(__file__))
    lua_filter_path = os.path.join(script_dir, "strip-math.lua")

    pandoc_cmd = [
        "pandoc",
        f"--lua-filter={lua_filter_path}",
        "--wrap=preserve",
        "-f", "markdown",
        "-t", "markdown"
    ]

    try:
      # Try running generated markdown through pandoc
      result = subprocess.run(
          pandoc_cmd,
          input=markdown_content,
          capture_output=True,
          text=True,
          check=True
      )
      final_output = result.stdout
    except (FileNotFoundError, subprocess.CalledProcessError) as e:
      # Fallback to unfiltered markdown if pandoc is missing or fails
      err_msg = getattr(e, 'stderr', str(e)).strip()
      sys.stderr.write(f"Warning: Pandoc filter failed ({err_msg}). Falling back to unfiltered markdown.\n")
      final_output = markdown_content

    # Write the final output to the destination
    out_f.write(final_output)

  finally:
    if in_f is not sys.stdin:
      in_f.close()
    if out_f is not sys.stdout:
      out_f.close()


if __name__ == "__main__":
  main()

#!/usr/bin/env python3
"""Measure each wrapping fixture in a fresh JVM and preserve outcomes and diagnostics."""

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import subprocess
import time


def main():
    """Run the selected matrix with a hard process deadline for each cell."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--classpath-file", type=Path, required=True,
                        help="output of sbt --client 'export hkmc2JVM/Runtime/fullClasspath'")
    parser.add_argument("--output", type=Path,
                        default=Path(__file__).with_name("wrapping-results.csv"))
    parser.add_argument("--variants", nargs="+", default=["bubble", "dummy", "rebind"],
                        choices=["bubble", "dummy", "rebind"])
    parser.add_argument("--cases", nargs="+")
    parser.add_argument("--warmups", type=int, default=5)
    parser.add_argument("--repeats", type=int, default=7)
    parser.add_argument("--timeout", type=float, default=30,
                        help="seconds per JVM, including startup and warmup")
    args = parser.parse_args()
    if args.warmups < 0 or args.repeats < 1 or args.timeout <= 0:
        parser.error("require warmups >= 0, repeats >= 1, timeout > 0")
    output = re.sub(r"\x1b\[[0-9;]*[A-Za-z]", "", args.classpath_file.read_text())
    candidates = [line.strip() for line in output.splitlines()
                  if line.startswith("/") and os.pathsep in line]
    if len(candidates) != 1:
        parser.error("classpath file must contain exactly one exported classpath")
    command = ["java", "-Xss8m", "-Xms128m", "-Xmx1g", "-cp", candidates[0],
               "hkmc2.ctml.experiments.Wrapping"]
    listed = subprocess.run(command + ["--list"], check=True, capture_output=True,
                            text=True, timeout=args.timeout)
    fixtures = dict(line.split(",") for line in listed.stdout.splitlines())
    cases = args.cases or list(fixtures)
    if any(case not in fixtures for case in cases):
        parser.error("unknown fixture in --cases")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    metadata = {
        "revision": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
        "tracked_diff": subprocess.check_output(["git", "diff", "--stat"], text=True),
        "java": subprocess.run(["java", "-version"], capture_output=True, text=True).stderr,
        "platform": platform.platform(),
        "sources_sha256": {name: hashlib.sha256(Path(__file__).with_name(name).read_bytes()).hexdigest()
                           for name in ["Wrapping.scala", "wrapping_bench.py"]},
        "variants": args.variants, "cases": cases,
        "warmups": args.warmups, "repeats": args.repeats,
        "timeout_seconds": args.timeout, "jvm_flags": command[1:4],
        "cells": [],
    }
    fields = ["variant", "case", "repetition", "expected", "outcome", "milliseconds", "type_nodes"]
    with args.output.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(fields)
        for case in cases:
            for variant in args.variants:
                start = time.monotonic()
                cell = {"variant": variant, "case": case}
                try:
                    result = subprocess.run(
                        command + [variant, case, str(args.warmups), str(args.repeats)],
                        capture_output=True, text=True, timeout=args.timeout)
                    rows = list(csv.reader(result.stdout.splitlines()))
                    if any(len(row) != len(fields) for row in rows):
                        raise RuntimeError(f"Malformed output: {result.stdout}")
                    writer.writerows(rows)
                    cell.update(returncode=result.returncode, diagnostic=result.stderr)
                    if result.returncode:
                        writer.writerow([variant, case, -1, fixtures[case], "crash", "", ""])
                    elif len(rows) != args.repeats:
                        raise RuntimeError(f"Missing measurements for {variant}/{case}")
                    outcome = "crash" if result.returncode else rows[-1][4]
                except subprocess.TimeoutExpired as error:
                    # subprocess.run kills and waits for this JVM before the next cell starts.
                    writer.writerow([variant, case, -1, fixtures[case], "timeout", "", ""])
                    cell.update(diagnostic=(error.stderr or b"").decode(), outcome="timeout")
                    outcome = "timeout"
                cell["wall_seconds"] = time.monotonic() - start
                metadata["cells"].append(cell)
                stream.flush()
                args.output.with_suffix(".json").write_text(json.dumps(metadata, indent=2) + "\n")
                print(f"{variant}/{case}: {outcome} ({cell['wall_seconds']:.1f}s)", flush=True)


if __name__ == "__main__":
    main()

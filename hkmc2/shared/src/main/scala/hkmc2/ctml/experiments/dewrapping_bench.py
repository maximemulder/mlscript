#!/usr/bin/env python3
"""Compare actual MLscript test sources with the parser's default and dewrapped policies."""

import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


POLICIES = {
    "default": "wrap-ifs wrap-ucs-ifs",
    "no-ctml": "no-wrap-ifs wrap-ucs-ifs",
    "dewrap": "no-wrap-ifs dewrap-ucs-ifs",
}


def blocks(text):
    """Extract comparable source blocks, outcomes, outputs, and optional inference samples."""
    result = []
    for block in re.split(r"\n\s*\n", text):
        lines = block.splitlines()
        source = "\n".join(line for line in lines if not line.startswith("//│"))
        outputs = [line.removeprefix("//│ ") for line in lines if line.startswith("//│ ")]
        samples = []
        for line in outputs:
            if line.startswith("@ctml-infer "):
                _, outcome, millis, nodes = line.split()
                samples.append({"outcome": outcome, "milliseconds": float(millis),
                                "type_nodes": int(nodes)})
        outputs = [line for line in outputs if not line.startswith("@ctml-infer ")]
        if not outputs and not samples:
            continue
        errors = re.findall(r"\[([^\]]*ERROR)\]", "\n".join(outputs))
        crashed = any("Uncaught error:" in line or "Unexpected exception" in line
                      for line in outputs)
        result.append({"source": source,
                       "outcome": "exception" if crashed else errors[0] if errors else "ok",
                       "outputs": outputs, "samples": samples})
    return result


def source_files(root):
    """Select the suite excluding fixtures whose purpose is to switch wrapping policies."""
    excluded = {"ctmlPrelude", "ctmlMatchWrapping", "ctmlSimplification", "ctmlJoinWrapping",
                "ctmlDewrapping"}
    return [p for p in sorted(root.glob("*.mls")) if p.stem not in excluded]


def focused_sources(root):
    """Use anonymous forms of actual suite definitions and source-level scaling fixtures."""
    selected = {"ctmlFlowCorrelation": ["g1", "g4", "g10", "g11"],
                "ctmlFlowRefinement": ["f6"], "ctmlFlowSoundness": ["tbad", "tsel"]}
    sources = {}
    for file, names in selected.items():
        for block in blocks((root / (file + ".mls")).read_text()):
            match = re.match(r"fun (\w+)\(([^)]*)\) =", block["source"])
            if match and match[1] in names:
                sources[match[1]] = re.sub(r"^fun \w+\(([^)]*)\) =", r"(\1) =>",
                                         block["source"], count=1)
    sources["siblings-8"] = "(a) => (" + ", ".join(
        ["(if a is Int then 1 else \"s\")"] * 8) + ")"
    nested = "1"
    for _ in range(8):
        nested = f"if a is Int then ({nested}) else \"s\""
    sources["nested-8"] = "(a) => (" + nested + ")"
    return sources


def run_focused(repo, root, directory, output, warmups, repetitions):
    """Run each source fixture and policy alone, discarding initial warmup samples."""
    sources = focused_sources(root)
    result = {"policies": POLICIES, "warmups": warmups, "repetitions": repetitions,
              "sources": sources, "cells": []}
    rows = []
    for name, source in sources.items():
        for mode, flags in POLICIES.items():
            target = directory / f"{name}-{mode}.mls"
            prefix = f":ctml\n\n:ctml-cfg {flags} error-absurd-constred profile-infer\n\n"
            if name == "g11":
                prefix += 'fun pick(a) = if a is Int then 1 else "s"\n\n'
            expected = "failed" if name == "g10" and mode == "dewrap" else "ok"
            block = (":te\n" if expected == "failed" else "") + source + "\n\n"
            target.write_text(prefix + block * (warmups + repetitions))
            print(f"Timing {name}/{mode}", flush=True)
            run = subprocess.run(["sbt", "--client", f"dtest ctml/{directory.name}/{target.stem}"],
                                 cwd=repo, capture_output=True, text=True, timeout=90)
            entries = [b for b in blocks(target.read_text())
                       if b["source"].removeprefix(":te\n") == source]
            samples = [sample for entry in entries for sample in entry["samples"]]
            if run.returncode or len(samples) != warmups + repetitions:
                raise RuntimeError(f"Focused run failed: {name}/{mode}\n{run.stdout}\n{run.stderr}")
            measured = samples[warmups:]
            if any(s["outcome"] != expected for s in measured):
                raise RuntimeError(f"Unexpected focused outcome: {name}/{mode}")
            result["cells"].append({"case": name, "mode": mode, "expected": expected,
                                    "samples": measured, "output": entries[warmups]["outputs"]})
            rows.extend([name, mode, index, s["outcome"], s["milliseconds"], s["type_nodes"]]
                        for index, s in enumerate(measured))
    output.write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n")
    with output.with_suffix(".csv").open("w", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(["case", "mode", "repetition", "outcome", "milliseconds", "type_nodes"])
        writer.writerows(rows)


def main():
    """Run disposable copies through the real diff-test pipeline and preserve comparisons."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--focused", action="store_true")
    parser.add_argument("--warmups", type=int, default=3)
    parser.add_argument("--repeats", type=int, default=5)
    args = parser.parse_args()
    if args.output is None:
        name = "dewrapping-focused.json" if args.focused else "dewrapping-results.json"
        args.output = Path(__file__).with_name(name)
    if args.warmups < 0 or args.repeats < 1:
        parser.error("require warmups >= 0 and repeats >= 1")
    repo = next(p for p in Path(__file__).resolve().parents if (p / "build.sbt").is_file())
    root = repo / "hkmc2/shared/src/test/mlscript/ctml"
    files = source_files(root)
    directory = Path(tempfile.mkdtemp(prefix="dewrapping-experiment-", dir=root))
    result = {
        "revision": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo,
                                             text=True).strip(),
        "policies": POLICIES, "files": {}, "runs": {},
        "sources_sha256": {str(p.relative_to(repo)): hashlib.sha256(p.read_bytes()).hexdigest()
                           for p in (repo / "hkmc2/shared/src/main/scala/hkmc2/ctml")
                           .rglob("*.scala")},
    }
    try:
        if args.focused:
            run_focused(repo, root, directory, args.output, args.warmups, args.repeats)
            return
        for mode, flags in POLICIES.items():
            target = directory / mode
            target.mkdir()
            for original in files:
                source = "\n".join(line for line in original.read_text().splitlines()
                                   if not line.startswith("//│")) + "\n"
                # Traces alter limits, output, and cost; this experiment measures normal inference.
                source = re.sub(r"(?m)^:ctml-dbg.*$",
                                "// Debugging disabled for measurement.", source)
                source = source.replace(":ctml\n", f":ctml\n\n:ctml-cfg {flags} profile-infer\n", 1)
                (target / original.name).write_text(source)
            filter_path = f"ctml/{directory.name}/{mode}/"
            print(f"Running {mode} on {len(files)} source files", flush=True)
            run = subprocess.run(["sbt", "--client", f"dtest {filter_path}"], cwd=repo,
                                 capture_output=True, text=True, timeout=120)
            result["runs"][mode] = {"exit_code": run.returncode,
                                    "log": run.stdout + run.stderr}
            print(f"{mode}: exit {run.returncode}", flush=True)
            for original in files:
                result["files"].setdefault(original.name, {})[mode] = blocks(
                    (target / original.name).read_text())
        changes = []
        for name, modes in result["files"].items():
            baseline = modes["default"]
            for mode in ["no-ctml", "dewrap"]:
                if len(modes[mode]) != len(baseline):
                    raise RuntimeError(f"Block alignment failed for {name}/{mode}")
                for index, (left, right) in enumerate(zip(baseline, modes[mode])):
                    if left["source"] != right["source"]:
                        raise RuntimeError(f"Source alignment failed for {name}/{mode}/{index}")
                    if left["outcome"] != right["outcome"] or left["outputs"] != right["outputs"]:
                        changes.append({"file": name, "block": index, "mode": mode,
                                        "before": left["outcome"], "after": right["outcome"],
                                        "kind": "outcome" if left["outcome"] != right["outcome"]
                                                else "output"})
        result["changes"] = changes
        args.output.write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n")
        with args.output.with_suffix(".csv").open("w", newline="") as stream:
            writer = csv.writer(stream, lineterminator="\n")
            writer.writerow(["file", "block", "mode", "sample", "outcome", "milliseconds",
                             "type_nodes"])
            for name, modes in result["files"].items():
                for mode, entries in modes.items():
                    for index, entry in enumerate(entries):
                        for sample, measurement in enumerate(entry["samples"]):
                            writer.writerow([name, index, mode, sample, measurement["outcome"],
                                             measurement["milliseconds"],
                                             measurement["type_nodes"]])
        print(f"Saved {len(changes)} changed blocks to {args.output}", flush=True)
    finally:
        shutil.rmtree(directory)


if __name__ == "__main__":
    main()

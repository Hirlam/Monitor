#!/usr/bin/env python3
"""Render Monitor's versioned joint-significance export as a scorecard."""

from __future__ import annotations

import argparse
import math
import re
import sys
from pathlib import Path

NUMBER = r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[EeDd][+-]?\d+)?"
ROW_RE = re.compile(
    rf"^\s*(.*?)\s+([+-]?\d+)\s+([+-]?\d+)\s+({NUMBER})\s+({NUMBER})\s+(\d+)\s*$"
)
LEVEL_RE = re.compile(r"\s+(\d+(?:\.\d+)?)hPa$", re.IGNORECASE)


def read_export(path: Path) -> tuple[dict[str, str], list[dict], list[str]]:
    metadata: dict[str, str] = {}
    rows: list[dict] = []
    order: list[str] = []
    seen_exact: set[tuple] = set()
    seen_keys: dict[tuple, tuple] = {}

    with path.open("r", encoding="utf-8") as stream:
        for line_no, raw in enumerate(stream, 1):
            line = raw.strip()
            if not line:
                continue
            if line.startswith("#"):
                body = line[1:].strip()
                if "=" in body:
                    key, value = body.split("=", 1)
                    key, value = key.strip(), value.strip()
                    if key in metadata and metadata[key] != value:
                        raise ValueError(f"{path}:{line_no}: conflicting metadata value for {key}")
                    metadata[key] = value
                continue
            match = ROW_RE.match(raw)
            if not match:
                raise ValueError(f"{path}:{line_no}: malformed row (expected six columns)")
            variable = match.group(1).strip()
            if not variable:
                raise ValueError(f"{path}:{line_no}: empty variable label")
            lead_index, lead_hour = int(match.group(2)), int(match.group(3))
            difference = float(match.group(4).replace("D", "E").replace("d", "e"))
            ci_width = float(match.group(5).replace("D", "E").replace("d", "e"))
            cases = int(match.group(6))
            if lead_index < 1 or lead_hour < 0 or cases < 0:
                raise ValueError(f"{path}:{line_no}: invalid index, lead hour, or paired-case count")
            if not math.isfinite(difference) or not math.isfinite(ci_width) or ci_width < 0:
                raise ValueError(f"{path}:{line_no}: RMSE difference and CI must be finite; CI nonnegative")
            if cases < 2:
                continue
            exact = (variable, lead_index, lead_hour, difference, ci_width, cases)
            if exact in seen_exact:
                continue
            seen_exact.add(exact)
            key = (variable, lead_hour)
            value = (lead_index, difference, ci_width, cases)
            if key in seen_keys and seen_keys[key] != value:
                raise ValueError(
                    f"{path}:{line_no}: conflicting values for {variable!r} at lead {lead_hour}"
                )
            seen_keys[key] = value
            if variable not in order:
                order.append(variable)
            rows.append({
                "line_no": line_no,
                "variable": variable,
                "lead_index": lead_index,
                "lead_hour": lead_hour,
                "rmse_difference": difference,
                "ci_half_width": ci_width,
                "paired_cases": cases,
                "significant": abs(difference) > ci_width,
            })
    schema_version = metadata.get("schema_version")
    if schema_version is None:
        raise ValueError(f"{path}: missing required schema_version=1 metadata")
    if schema_version != "1":
        raise ValueError(f"{path}: unsupported schema_version={schema_version}")
    return metadata, rows, order


def select_domain(
    rows: list[dict], domain: str, allowed_levels: set[str]
) -> tuple[list[dict], list[str]]:
    selected: list[dict] = []
    order: list[str] = []
    allowed = {str(int(level)) if level.isdigit() else level for level in allowed_levels}
    for row in rows:
        label = row["variable"]
        level_match = LEVEL_RE.search(label)
        if domain == "SURF":
            if level_match:
                continue
        else:
            if not level_match:
                continue
            level = level_match.group(1).removesuffix(".0")
            if level not in allowed:
                continue
        selected.append(row)
        if label not in order:
            order.append(label)
    if domain == "TEMP":
        base_order: dict[str, int] = {}
        for label in order:
            match = LEVEL_RE.search(label)
            base = label[:match.start()].strip() if match else label
            base_order.setdefault(base, len(base_order))
        order.sort(key=lambda label: (
            base_order[label[:LEVEL_RE.search(label).start()].strip()],
            float(LEVEL_RE.search(label).group(1)),
        ))
    return selected, order


def render_export(
    input_path: Path,
    metadata: dict[str, str],
    rows: list[dict],
    outdir: Path,
    domain: str,
    reference_label: str,
    comparison_label: str,
    stem: str,
    levels: str,
) -> tuple[dict[str, str], Path]:
    try:
        from .scorecard import _write_scorecard_outputs, plot_precomputed_scorecard
    except ImportError:
        try:
            from scorecard import _write_scorecard_outputs, plot_precomputed_scorecard
        except ImportError as exc:
            raise RuntimeError(
                "scorecard dependencies are missing; install "
                "src/python/requirements-scorecards.txt"
            ) from exc

    recorded_domain = metadata.get("domain")
    if recorded_domain and recorded_domain.upper() != domain:
        raise ValueError(f"export domain is {recorded_domain}, requested {domain}")
    selected, variable_order = select_domain(rows, domain, set(levels.split(",")))
    if not selected:
        raise ValueError(f"no selected {domain} rows in {input_path}")
    confidence = float(metadata.get("confidence_percent", "90").split()[0])
    if not math.isfinite(confidence) or not 0 < confidence < 100:
        raise ValueError("confidence_percent must be between 0 and 100")
    title_domain = "Surface" if domain == "SURF" else "Upper-Air"
    title = f"{title_domain}: {reference_label} vs {comparison_label}"
    csv_path = _write_scorecard_outputs(selected, str(outdir), stem)
    png_path = plot_precomputed_scorecard(
        selected, variable_order, str(outdir), stem, title,
        reference_label, comparison_label, confidence,
    )
    if not csv_path.is_file() or not png_path.is_file() or png_path.stat().st_size == 0:
        raise RuntimeError(f"renderer output is missing for {input_path}")
    return metadata, png_path


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--outdir", required=True, type=Path)
    parser.add_argument("--domain", required=True, choices=("SURF", "TEMP"))
    parser.add_argument("--reference-label", required=True)
    parser.add_argument("--comparison-label", required=True)
    parser.add_argument("--stem", required=True)
    parser.add_argument("--levels", default="300,500,700,850")
    parser.add_argument("--selection", default="ALL")
    parser.add_argument("--station-scope", default="0")
    parser.add_argument("--initial-time-group", default="ALL")
    parser.add_argument("--period")
    args = parser.parse_args(argv)
    try:
        metadata, rows, _ = read_export(args.input)
        expected = {
            "selection": args.selection,
            "station_scope": args.station_scope,
            "initial_time_group": args.initial_time_group,
        }
        if args.period is not None:
            expected["period"] = args.period
        for key, value in expected.items():
            if metadata.get(key) != value:
                raise ValueError(f"expected {key}={value}, export has {metadata.get(key)}")
        if metadata.get("domain", args.domain).upper() != args.domain:
            raise ValueError(f"export domain is {metadata.get('domain')}, requested {args.domain}")
        metadata, output = render_export(
            args.input, metadata, rows, args.outdir, args.domain,
            args.reference_label, args.comparison_label, args.stem, args.levels,
        )
        print(output)
        return 0
    except (OSError, ValueError, RuntimeError, ImportError) as exc:
        print(f"scorecard generation failed: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())

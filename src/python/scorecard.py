"""Small, self-contained renderer for Monitor joint-score exports."""

from __future__ import annotations

import csv
import os
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import matplotlib.patheffects as patheffects
from matplotlib.lines import Line2D


def _write_scorecard_outputs(rows: list[dict], outdir: str, stem: str) -> Path:
    """Write the rows used by the plot as a CSV alongside the PNG."""
    output = Path(outdir)
    output.mkdir(parents=True, exist_ok=True)
    target = output / f"{stem}.csv"
    temporary = output / f".{stem}.csv.tmp"
    with temporary.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    os.replace(temporary, target)
    return target


def plot_precomputed_scorecard(
    rows: list[dict],
    variable_order: list[str],
    outdir: str,
    stem: str,
    title: str,
    reference_label: str,
    comparison_label: str,
    confidence_percent: float,
) -> Path:
    """Plot signed normalized RMSE differences by variable and lead hour."""
    output = Path(outdir)
    output.mkdir(parents=True, exist_ok=True)
    target = output / f"{stem}.png"
    temporary = output / f".{stem}.png.tmp"

    variables = [name for name in variable_order if any(r["variable"] == name for r in rows)]
    hours = sorted({int(row["lead_hour"]) for row in rows})
    if not variables or not hours:
        raise ValueError("No selected scorecard rows to render")

    significant_values = [
        100.0 * abs(float(row["rmse_difference"]))
        for row in rows if bool(row["significant"])
    ]
    all_values = [100.0 * abs(float(row["rmse_difference"])) for row in rows]
    # Non-significant values do not set the scale. Values outside the
    # significant range saturate at the corresponding end of the colormap.
    limit = max(significant_values or all_values, default=0.0)
    limit = max(limit, 1.0e-12)
    cmap = plt.get_cmap("coolwarm_r")  # Positive means the comparison has lower RMSE.
    norm = plt.Normalize(vmin=-limit, vmax=limit, clip=True)
    x_index = {hour: index for index, hour in enumerate(hours)}
    y_index = {name: index for index, name in enumerate(variables)}
    significant_positive = sum(
        bool(row["significant"]) and float(row["rmse_difference"]) > 0 for row in rows
    )
    significant_negative = sum(
        bool(row["significant"]) and float(row["rmse_difference"]) < 0 for row in rows
    )
    positive = sum(float(row["rmse_difference"]) > 0 for row in rows)
    negative = sum(float(row["rmse_difference"]) < 0 for row in rows)

    upper_air = bool(variables) and all(name.lower().endswith("hpa") for name in variables)
    legend_y = -0.065 if upper_air else -0.095
    row_height = 0.22 if upper_air else 0.21
    height = max(3.0, row_height * len(variables) + 1.5)
    width = max(4.8, 0.24 * len(hours) + 2.5)
    fig, ax = plt.subplots(figsize=(width, height))
    fig.subplots_adjust(left=0.29, right=0.83, top=0.88, bottom=0.22)
    for row in rows:
        x = x_index[int(row["lead_hour"])]
        y = y_index[row["variable"]]
        significant = bool(row["significant"])
        points = ax.scatter(
            [x], [y], marker="s", s=130 if significant else 90,
            c=[100.0 * float(row["rmse_difference"])],
            cmap=cmap, norm=norm, alpha=1.0 if significant else 0.5,
            edgecolors="#222222" if significant else "#777777",
            linewidths=1.15 if significant else 0.45,
        )
        if significant:
            points.set_path_effects([
                patheffects.Stroke(linewidth=3.0, foreground="white"),
                patheffects.Normal(),
            ])

    ax.set_xticks(range(len(hours)), [str(hour) for hour in hours])
    ax.set_yticks(range(len(variables)), variables)
    ax.tick_params(axis="both", labelsize=8, pad=3)
    ax.set_xlabel("Forecast lead (hours)", fontsize=9, labelpad=1)
    ax.set_ylabel("Parameter")
    ax.invert_yaxis()
    ax.set_xlim(-0.6, len(hours) - 0.4)
    ax.set_ylim(len(variables) - 0.5, -0.5)
    ax.set_xticks([index - 0.5 for index in range(len(hours) + 1)], minor=True)
    ax.set_yticks([index - 0.5 for index in range(len(variables) + 1)], minor=True)
    ax.grid(which="minor", color="#d7dce2", linewidth=0.6)
    ax.tick_params(which="minor", bottom=False, left=False)
    ax.set_title(title, loc="left", x=-0.55, ha="left", weight="bold", fontsize=10, pad=9)

    colorbar = fig.colorbar(
        plt.cm.ScalarMappable(norm=norm, cmap=cmap), ax=ax,
        pad=0.035, fraction=0.045, shrink=0.68, aspect=28,
    )
    colorbar.set_label("Normalized RMSE difference (%)\n(reference − comparison)", fontsize=8)
    colorbar.ax.tick_params(labelsize=8)
    ax.legend(
        handles=[
            Line2D([], [], marker="s", linestyle="None", markerfacecolor="white",
                   markeredgecolor="#222222", markeredgewidth=1.2,
                   label=f"Significant at {confidence_percent:g}%"),
            Line2D([], [], marker="s", linestyle="None", markerfacecolor="white",
                   markeredgecolor="#777777", markeredgewidth=0.6,
                   label="Not significant"),
        ],
        loc="upper center", bbox_to_anchor=(0.5, legend_y), ncol=2, frameon=False,
        fontsize=8, handletextpad=0.5, columnspacing=1.2,
    )
    fig.text(
        0.01, 0.045,
        f"Squares (+ / −): significant {significant_positive} / {significant_negative}; "
        f"all {positive} / {negative}.",
        ha="left", va="bottom", fontsize=7,
    )
    fig.text(
        0.01, 0.015,
        f"Positive values mean {comparison_label} has lower RMSE than {reference_label}.",
        ha="left", va="bottom", fontsize=7,
    )
    fig.savefig(temporary, format="png", dpi=100, bbox_inches="tight")
    plt.close(fig)
    if not temporary.is_file() or temporary.stat().st_size == 0:
        raise RuntimeError("Plot renderer did not produce a PNG")
    os.replace(temporary, target)
    return target

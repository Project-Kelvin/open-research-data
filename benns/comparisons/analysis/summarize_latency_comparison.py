"""Summarize latency_comparison.csv, grouped by topology.

For each topology, computes error statistics (approximated - measured latency,
i.e. |error| for MAE-style stats) and threshold-based classification counts
using a latency threshold (default 100).
"""
from __future__ import annotations

import argparse
import csv
import statistics
from pathlib import Path

import numpy as np
from scipy import stats

BASE_DIR = Path(__file__).resolve().parent.parent  # repetitive_hybrid/
CSV_HEADERS = [
    "topology",
    "algorithm",
    "count",
    "mean_mae",
    "median_mae",
    "std_mae",
    "q1_mae",
    "q3_mae",
    "iqr_mae",
    "ci95_lower",
    "ci95_upper",
    "ci95",
    "true_positives",
    "false_positives",
    "precision",
]


def read_records(csv_path: Path) -> list[dict[str, float | str]]:
    records = []
    with csv_path.open("r", newline="", encoding="utf-8") as file_handle:
        reader = csv.DictReader(file_handle)
        for row in reader:
            records.append(
                {
                    "topology": row["topology"],
                    "algorithm": row["algorithm"],
                    "approximated_latency": float(row["approximated_latency"]),
                    "measured_latency": float(row["measured_latency"]),
                }
            )
    return records


def quartiles(values: list[float]) -> tuple[float, float]:
    q1, _, q3 = statistics.quantiles(values, n=4, method="inclusive")
    return q1, q3


def confidence_interval_95(values: list[float]) -> tuple[float, float]:
    """BCa bootstrap 95% CI for the mean, robust to skewed error distributions."""
    if len(values) < 2 or len(set(values)) == 1:
        return values[0], values[0]

    arr = np.asarray(values, dtype=float)
    res = stats.bootstrap(
        (arr,),
        statistic=np.mean,
        confidence_level=0.95,
        n_resamples=10000,
        method="BCa",
        random_state=42,
    )

    return float(res.confidence_interval.low), float(res.confidence_interval.high)


def summarize(records: list[dict[str, float | str]], threshold: float) -> list[dict[str, object]]:
    grouped: dict[tuple[str, str], list[dict[str, float | str]]] = {}
    for record in records:
        grouped.setdefault((record["topology"], record["algorithm"]), []).append(record)

    summaries = []
    for topology in sorted(grouped):
        group = grouped[topology]
        errors = [abs(r["approximated_latency"] - r["measured_latency"]) for r in group]

        q1, q3 = quartiles(errors) if len(errors) >= 2 else (errors[0], errors[0])
        ci_lower, ci_upper = confidence_interval_95(errors)

        true_positives = sum(
            1 for r in group if r["approximated_latency"] <= threshold and r["measured_latency"] <= threshold
        )
        false_positives = sum(
            1 for r in group if r["approximated_latency"] <= threshold and r["measured_latency"] > threshold
        )
        precision = (
            true_positives / (true_positives + false_positives)
            if (true_positives + false_positives) > 0
            else 0.0
        )

        summaries.append(
            {
                "topology": topology[0],
                "algorithm": topology[1],
                "count": len(group),
                "mean_mae": round(statistics.mean(errors), 2),
                "median_mae": round(statistics.median(errors), 2),
                "std_mae": round(statistics.stdev(errors) if len(errors) >= 2 else 0.0, 2),
                "q1_mae": round(q1, 2),
                "q3_mae": round(q3, 2),
                "iqr_mae": round(q3 - q1, 2),
                "ci95_lower": round(ci_lower, 2),
                "ci95_upper": round(ci_upper, 2),
                "ci95": f"[{round(ci_lower, 2)}, {round(ci_upper, 2)}]",
                "true_positives": true_positives,
                "false_positives": false_positives,
                "precision": round(precision, 2),
            }
        )

    return summaries


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=BASE_DIR / "latency_comparison.csv")
    parser.add_argument("--output", type=Path, default=BASE_DIR / "latency_comparison_summary.csv")
    parser.add_argument("--threshold", type=float, default=100.0)
    args = parser.parse_args()

    records = read_records(args.input)
    summaries = summarize(records, args.threshold)

    with args.output.open("w", newline="", encoding="utf-8") as file_handle:
        writer = csv.DictWriter(file_handle, fieldnames=CSV_HEADERS)
        writer.writeheader()
        writer.writerows(summaries)

    print(f"Wrote {len(summaries)} rows to {args.output}")


if __name__ == "__main__":
    main()

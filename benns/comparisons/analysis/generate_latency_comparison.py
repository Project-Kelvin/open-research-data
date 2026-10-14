"""Generate a CSV comparing measured (emulator) vs approximated (surrogate) latency
for each topology/algorithm/run in the repetitive_hybrid experiment data.

For every pfs.csv, the approximated latency for an emulator row is taken from the
surrogate row(s) in the last surrogate generation that precedes that emulator's own
generation, matched by `ar`. Files without any emulator rows are skipped.
"""
from __future__ import annotations

import argparse
import csv
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent.parent  # repetitive_hybrid/
DATA_DIR = BASE_DIR / "data"
CSV_HEADERS = ["topology", "run", "algorithm", "approximated_latency", "measured_latency"]


def extract_latencies(pf_csv_path: Path) -> list[tuple[float, float]]:
    """Return (approximated_latency, measured_latency) pairs for each emulator row."""
    rows = []
    with pf_csv_path.open("r", newline="", encoding="utf-8") as file_handle:
        reader = csv.DictReader(file_handle, skipinitialspace=True)
        for row in reader:
            rows.append(
                {
                    "method": (row.get("method") or "").strip(),
                    "generation": int(float(row["generation"])),
                    "latency": float(row["latency"]),
                    "ar": float(row["ar"]),
                }
            )

    emulator_rows = [r for r in rows if r["method"] == "emulator"]
    if not emulator_rows:
        return []

    surrogate_rows = [r for r in rows if r["method"] == "surrogate"]

    results = []
    for emulator_row in emulator_rows:
        prior_generations = [
            r["generation"] for r in surrogate_rows if r["generation"] < emulator_row["generation"]
        ]
        if not prior_generations:
            continue
        last_generation = max(prior_generations)

        ar_to_latency = {
            round(r["ar"], 9): r["latency"]
            for r in surrogate_rows
            if r["generation"] == last_generation
        }

        approximated_latency = ar_to_latency.get(round(emulator_row["ar"], 9))
        if approximated_latency is None:
            continue
        results.append((approximated_latency, emulator_row["latency"]))

    return results


def collect_records(data_dir: Path) -> list[dict[str, object]]:
    records: list[dict[str, object]] = []
    for topology_dir in sorted(data_dir.iterdir()):
        if not topology_dir.is_dir():
            continue
        for algorithm_dir in sorted(topology_dir.iterdir()):
            if not algorithm_dir.is_dir():
                continue
            runs_dir = algorithm_dir
            if not runs_dir.is_dir():
                continue
            for run_dir in sorted(runs_dir.iterdir()):
                if not run_dir.is_dir():
                    continue
                pf_csv = run_dir / "pfs.csv"
                if not pf_csv.exists():
                    continue

                for approximated_latency, measured_latency in extract_latencies(pf_csv):
                    records.append(
                        {
                            "topology": topology_dir.name,
                            "run": run_dir.name,
                            "algorithm": algorithm_dir.name,
                            "approximated_latency": approximated_latency,
                            "measured_latency": measured_latency,
                        }
                    )

    return records


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-dir", type=Path, default=DATA_DIR)
    parser.add_argument("--output", type=Path, default=BASE_DIR / "latency_comparison.csv")
    args = parser.parse_args()

    records = collect_records(args.data_dir)

    with args.output.open("w", newline="", encoding="utf-8") as file_handle:
        writer = csv.DictWriter(file_handle, fieldnames=CSV_HEADERS)
        writer.writeheader()
        writer.writerows(records)

    print(f"Wrote {len(records)} rows to {args.output}")


if __name__ == "__main__":
    main()

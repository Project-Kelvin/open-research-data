from __future__ import annotations

import scipy.stats as stats
import csv
import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from statistics import mean, median
import numpy as np

QUALIFIED_INDIVIDUALS_PATTERN = re.compile(r"Qualified Individuals:\s*(\d+)")
TIME_TAKEN_PATTERN = re.compile(r"Time taken:\s*([\d.]+)")
CSV_HEADERS = [
    "random_weights",
    "runs",
    "mean_generations",
    "median_generations",
    "max_generations",
    "min_generations",
    "q1_generations",
    "q3_generations",
    "iqr_generations",
    "ci_generations",
    "generations_upper_lower",
    "std_deviation_generations",
    "converged_number",
    "convergence_percent",
    "mean_time_taken",
    "std_deviation_time_taken",
    "q1_time_taken",
    "q3_time_taken",
    "iqr_time_taken",
    "ci_time_taken",
    "time_taken_upper_lower",
]


@dataclass(frozen=True)
class RunRecord:
    random_weights: str
    generations: int
    converged: bool
    time_taken: float


def parse_dir_name(dir_name: str) -> str:
    parts = dir_name.split("_")
    if len(parts) < 3:
        raise ValueError(f"Directory name has too few parts: {dir_name}")

    random_weightsLabel: int = parts.index("weights")
    random_weights = str(parts[random_weightsLabel + 1])

    return random_weights


def parse_generations(data_csv_path: Path) -> int:
    max_generation = 0
    with data_csv_path.open("r", newline="", encoding="utf-8") as file_handle:
        reader = csv.DictReader(file_handle, skipinitialspace=True)
        for row in reader:
            generation_str = (row.get("generation") or "").strip()
            if not generation_str:
                continue
            max_generation = max(max_generation, int(float(generation_str)))

    if max_generation == 0:
        raise ValueError(f"No generation values found in {data_csv_path}")
    return max_generation


def parse_converged(experiment_txt_path: Path) -> bool:
    content = experiment_txt_path.read_text(encoding="utf-8")
    match = QUALIFIED_INDIVIDUALS_PATTERN.search(content)
    if not match:
        raise ValueError(
            f"Could not find 'Qualified Individuals' in {experiment_txt_path}"
        )

    qualified_individuals = int(match.group(1))
    return qualified_individuals >= 1


def parse_time_taken(experiment_txt_path: Path) -> float:
    content = experiment_txt_path.read_text(encoding="utf-8")
    match = TIME_TAKEN_PATTERN.search(content)
    if not match:
        raise ValueError(f"Could not find 'Time taken' in {experiment_txt_path}")

    time_taken = float(match.group(1))
    return time_taken


def collect_runs(base_dir: Path) -> list[RunRecord]:
    runs: list[RunRecord] = []

    for entry in sorted(base_dir.iterdir()):
        if not entry.is_dir():
            continue

        data_csv = entry / "data.csv"
        experiment_txt = entry / "experiment.txt"
        if not data_csv.exists() or not experiment_txt.exists():
            continue

        random_weights = parse_dir_name(entry.name)
        generations = parse_generations(data_csv)
        converged = parse_converged(experiment_txt)
        time_taken = parse_time_taken(experiment_txt)

        runs.append(
            RunRecord(
                random_weights=random_weights,
                generations=generations,
                converged=converged,
                time_taken=time_taken,
            )
        )

    return runs


def summarize(runs: list[RunRecord]) -> list[dict[str, float | int | str]]:
    grouped: dict[tuple[str], list[RunRecord]] = defaultdict(list)
    for run in runs:
        grouped[(run.random_weights,)].append(run)

    summary: list[dict[str, float | int | str]] = []
    for (random_weights,), group_runs in sorted(grouped.items()):
        generations = sorted(r.generations for r in group_runs)
        run_count = len(group_runs)
        times_taken = sorted(r.time_taken for r in group_runs)

        converged_count = sum(1 for r in group_runs if r.converged)
        convergence_percent = (converged_count / run_count) * 100

        df = run_count - 1
        alpha = 0.05
        t = stats.t.ppf(1 - alpha / 2, df)
        ci_generations = round(
            (
                t * (np.std(generations) / np.sqrt(len(generations)))
                if len(generations) > 1
                else 0.0
            ),
            2,
        )
        ci_time_taken = round(
            (
                t * (np.std(times_taken) / np.sqrt(len(times_taken)))
                if len(times_taken) > 1
                else 0.0
            ),
            2,
        )

        summary.append(
            {
                "random_weights": random_weights,
                "runs": run_count,
                "mean_generations": round(mean(generations), 2),
                "median_generations": median(generations),
                "max_generations": max(generations),
                "min_generations": min(generations),
                "q1_generations": round(np.quantile(generations, 0.25), 2),
                "q3_generations": round(np.quantile(generations, 0.75), 2),
                "iqr_generations": round(
                    np.quantile(generations, 0.75) - np.quantile(generations, 0.25), 2
                ),
                "ci_generations": ci_generations,
                "generations_upper_lower": f"{round(mean(generations) - ci_generations, 2)}-{round(mean(generations) + ci_generations, 2)}",
                "std_deviation_generations": round(
                    np.std(generations) if len(generations) > 1 else 0.0, 2
                ),
                "converged_number": converged_count,
                "convergence_percent": round(convergence_percent, 2),
                "mean_time_taken": round(mean(times_taken), 2),
                "std_deviation_time_taken": round(
                    np.std(times_taken) if len(times_taken) > 1 else 0.0, 2
                ),
                "q1_time_taken": round(np.quantile(times_taken, 0.25), 2),
                "q3_time_taken": round(np.quantile(times_taken, 0.75), 2),
                "iqr_time_taken": round(
                    np.quantile(times_taken, 0.75) - np.quantile(times_taken, 0.25), 2
                ),
                "ci_time_taken": ci_time_taken,
                "time_taken_upper_lower": f"{round(mean(times_taken) - ci_time_taken, 2)}-{round(mean(times_taken) + ci_time_taken, 2)}",
            }
        )

    return summary


def write_csv(rows: list[dict[str, float | int | str]], output_path: Path) -> None:
    with output_path.open("w", newline="", encoding="utf-8") as file_handle:
        writer = csv.DictWriter(file_handle, fieldnames=CSV_HEADERS)
        writer.writeheader()
        writer.writerows(rows)


def process_dataset(base_dir: Path, output_path: Path) -> None:
    runs = collect_runs(base_dir)
    if not runs:
        raise SystemExit(f"No run folders found in {base_dir}")

    summary = summarize(runs)
    write_csv(summary, output_path)
    print(f"Saved {len(summary)} grouped rows to {output_path}")


def main() -> None:

    process_dataset(Path("data"), Path("summary.csv"))


if __name__ == "__main__":
    main()

from __future__ import annotations
import os
from typing import Tuple
from pymoo.indicators.hv import HV
import matplotlib.pyplot as plt
import scipy.stats as stats
import csv
import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from statistics import mean, median
import numpy as np

plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["Liberation Serif"]  # or 'Nimbus Roman'
plt.rcParams["figure.constrained_layout.use"] = True

QUALIFIED_INDIVIDUALS_PATTERN = re.compile(r"Qualified Individuals:\s*(\d+)")
TIME_TAKEN_PATTERN = re.compile(r"Time taken:\s*([\d.]+)")
CSV_HEADERS_SUMMARY = [
    "algorithm",
    "topology",
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
    "median_time_taken",
    "std_deviation_time_taken",
    "q1_time_taken",
    "q3_time_taken",
    "iqr_time_taken",
    "ci_time_taken",
    "time_taken_upper_lower",
    "max_time_taken",
    "min_time_taken",
    "mean_hv",
    "median_hv",
    "std_deviation_hv",
    "q1_hv",
    "q3_hv",
    "iqr_hv",
    "ci_hv",
    "hv_upper_lower",
    "max_hv",
    "min_hv",
]

CSV_HEADERS_AGGREGATION = [
    "algorithm",
    "topology",
    "generations",
    "converged",
    "time_taken",
    "hv",
]

@dataclass()
class RunRecord:
    algorithm: str
    topology: str
    generations: int
    converged: bool
    time_taken: float
    pfs: list[tuple[float, float]]
    hv: float

def parse_generations(data_csv_path: Path) -> Tuple[int, bool]:
    max_generation = 0
    isEmulator = False
    with data_csv_path.open("r", newline="", encoding="utf-8") as file_handle:
        reader = csv.DictReader(file_handle, skipinitialspace=True)
        for row in reader:
            if row.get("method") == "emulator":
                isEmulator = True
                continue
            generation_str = (row.get("generation") or "").strip()
            if not generation_str:
                continue
            max_generation = max(max_generation, int(float(generation_str)))

    if max_generation == 0:
        raise ValueError(f"No generation values found in {data_csv_path}")
    return max_generation, isEmulator

def parsePFs(pf_csv_path: Path, generations: int) -> list[Tuple[float, float]]:
    pfs: list[Tuple[float, float]] = []
    with pf_csv_path.open("r", newline="", encoding="utf-8") as file_handle:
        reader = csv.DictReader(file_handle, skipinitialspace=True)
        for row in reader:
            if int(row.get("generation") or 0) == generations:
                ar = (row.get("ar") or "").strip()
                latency = (row.get("latency") or "").strip()
                if not ar or not latency:
                    continue
                pfs.append((float(ar), float(latency)))

    return pfs


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

    for topo in sorted(base_dir.iterdir()):
        if not topo.is_dir():
            continue
        for algo in topo.iterdir():
            if not algo.is_dir():
                continue
            for entry in Path(os.path.join(algo, "data")).iterdir():
                if not entry.is_dir():
                    continue

                data_csv = entry / "data.csv"
                experiment_txt = entry / "experiment.txt"
                if not data_csv.exists() or not experiment_txt.exists():
                    continue

                generations, isEmulator = parse_generations(data_csv)
                converged = parse_converged(experiment_txt)
                time_taken = parse_time_taken(experiment_txt)

                if isEmulator:
                    if topo.name == "dc":
                        time_taken = time_taken - 672 # Experiment time
                    elif topo.name == "25n50e" or topo.name == "milan":
                        time_taken = time_taken - 720 # Experiment time

                    time_taken = time_taken - 60 # 1 minute for the emulator to start

                pf_csv = entry / "pfs.csv"

                if not pf_csv.exists():
                    continue

                pfs_data = parsePFs(pf_csv, generations)

                runs.append(
                    RunRecord(
                        algorithm=algo.name,
                        topology=topo.name,
                        generations=generations,
                        converged=converged,
                        time_taken=time_taken,
                        pfs=pfs_data,
                        hv=0.0
                    )
                )

    allArs = np.array([pf[0] for run in runs for pf in run.pfs])
    allLatencies = np.array([pf[1] for run in runs for pf in run.pfs])
    normalisedArs = (allArs - allArs.min()) / (allArs.max() - allArs.min())
    normalisedLatencies = (allLatencies - allLatencies.min()) / (allLatencies.max() - allLatencies.min())
    nadirAR: float = -1 * normalisedArs.min() * 0.90
    nadirLatency: float = normalisedLatencies.max() * 1.10

    ind = HV(ref_point=np.array([nadirAR, nadirLatency]))
    for run in runs:
        normalisedPfs = np.array([( -1 * (pf[0] - allArs.min()) / (allArs.max() - allArs.min()), (pf[1] - allLatencies.min()) / (allLatencies.max() - allLatencies.min()) ) for pf in run.pfs])
        run.hv = ind(normalisedPfs) or 0.0

    return runs


def summarize(runs: list[RunRecord]) -> Tuple[list[dict[str, float | int | str]], list[dict[str, float | int | str]]]:
    grouped: dict[tuple[str, str], list[RunRecord]] = defaultdict(list)
    for run in runs:
        grouped[(run.algorithm, run.topology)].append(run)

    summary: list[dict[str, float | int | str]] = []
    aggregation: list[dict[str, float | int | str]] = []

    for (algorithm, topology), group_runs in sorted(grouped.items()):
        generations = sorted(r.generations for r in group_runs)
        run_count = len(group_runs)
        times_taken = sorted(r.time_taken for r in group_runs)
        hvs = sorted(r.hv for r in group_runs)

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
        ci_hv = round(
            (
                t * (np.std(hvs) / np.sqrt(len(hvs)))
                if len(hvs) > 1
                else 0.0
            ),
            2,
        )

        summary.append(
            {
                "algorithm": algorithm,
                "topology": topology,
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
                "median_time_taken": median(times_taken),
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
                "max_time_taken": max(times_taken),
                "min_time_taken": min(times_taken),
                "mean_hv": round(mean(hvs), 2),
                "median_hv": median(hvs),
                "std_deviation_hv": round(
                    np.std(hvs) if len(hvs) > 1 else 0.0, 2
                ),
                "q1_hv": round(np.quantile(hvs, 0.25), 2),
                "q3_hv": round(np.quantile(hvs, 0.75), 2),
                "iqr_hv": round(
                    np.quantile(hvs, 0.75) - np.quantile(hvs, 0.25), 2
                ),
                "ci_hv": ci_hv,
                "hv_upper_lower": f"{round(mean(hvs) - ci_hv, 2)}-{round(mean(hvs) + ci_hv, 2)}",
                "max_hv": max(hvs),
                "min_hv": min(hvs),
            }
        )

        for run in group_runs:
            aggregation.append(
                {
                    "algorithm": algorithm,
                    "topology": topology,
                    "generations": run.generations,
                    "converged": run.converged,
                    "time_taken": run.time_taken,
                    "hv": run.hv,
                }
            )

    return summary, aggregation

def drawPFs(runs: list[RunRecord]) -> None:
    grouped: dict[str, list[RunRecord]] = defaultdict(list)
    for run in runs:
        grouped[run.topology].append(run)

    fig, axes = plt.subplots(
        1, 3, figsize=(10, 5), sharey=True
    )

    topoNames = {
        "dc": "DC 2",
        "milan": "Milan",
        "25n50e": "25N50E",
    }

    algorithmNames = {
        "genesis": "GENESIS",
        "bega": "BEGA",
        "gaha": "REGA",
    }

    palette = [
            "#1f77b4",
            "#ff7f0e",
            "#2ca02c",
            "#d62728",
            "#9467bd",
            "#8c564b",
            "#e377c2",
            "#7f7f7f",
            "#bcbd22",
            "#17becf",
            "#003f5c",
            "#ffd60a",
            "#2ec4b6",
            "#e6007e",
            "#8fce00",
            "#5c677d",
        ]


    for ax, (topo, group_runs) in zip(axes.reshape(-1), grouped.items()):
        runs_by_algorithm: dict[str, list[RunRecord]] = defaultdict(list)
        for run in group_runs:
            runs_by_algorithm[run.algorithm].append(run)
        colors = {a: palette[i % len(palette)] for i, a in enumerate(runs_by_algorithm.keys())}
        for algorithm, algo_runs in runs_by_algorithm.items():
            ax.scatter(
                [pf[0] for run in algo_runs for pf in run.pfs],
                [pf[1] for run in algo_runs for pf in run.pfs],
                alpha=0.5,
                color=colors[algorithm],
                label=algorithmNames.get(algorithm, algorithm.upper()),
            )
        ax.set_xlabel("Acceptance Ratio")
        ax.grid(alpha=0.3)
        ax.set_ylim(0.0, 200)
        ax.legend(loc="upper right", fontsize=9)
        ax.set_ylabel("Median Traffic Latency (ms)")
        ax.set_title(f"{topoNames.get(topo, topo.upper())}")
    plt.tight_layout()
    plt.savefig("pareto_fronts.png", dpi=300, bbox_inches="tight")


def write_csv(rows: list[dict[str, float | int | str]], output_path: Path, fieldnames: list[str]) -> None:
    with output_path.open("w", newline="", encoding="utf-8") as file_handle:
        writer = csv.DictWriter(file_handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def process_dataset(base_dir: Path, summaryPath: Path, aggregationPath: Path) -> None:
    runs = collect_runs(base_dir)
    if not runs:
        raise SystemExit(f"No run folders found in {base_dir}")

    summary, aggregation = summarize(runs)
    write_csv(summary, summaryPath, CSV_HEADERS_SUMMARY)
    write_csv(aggregation, aggregationPath, CSV_HEADERS_AGGREGATION)
    print(f"Saved {len(summary)} grouped rows to {summaryPath}")
    print(f"Saved {len(aggregation)} individual rows to {aggregationPath}")
    drawPFs(runs)


def main() -> None:

    process_dataset(Path("data"), Path("summary.csv"), Path("aggregation.csv"))


if __name__ == "__main__":
    main()

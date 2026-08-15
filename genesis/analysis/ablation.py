"""
This file defines the code for the analysis of the ablation experiments.
"""

import csv
from dataclasses import dataclass
import os
from pathlib import Path
import re
from typing import Tuple

QUALIFIED_INDIVIDUALS_PATTERN = re.compile(r"Qualified Individuals:\s*(\d+)")
TIME_TAKEN_PATTERN = re.compile(r"Time taken:\s*([\d.]+)")

experiments: list[str] =[
    "genesis_activation",
    "genesis_gaussian",
    "genesis_chain",
    "genesis_dijkstra",
    "genesis_dijkstra_chain",
    "genesis_weights",
    "genesis_neurons",
    "genesis_control_offline",
    os.path.join("genesis_random_host", "data", "genesis_random_host_1"),
    os.path.join("genesis_random_host", "data", "genesis_random_host_2"),
    os.path.join("genesis_random_host", "data", "genesis_random_host_3"),
    os.path.join("genesis_random_host", "data", "genesis_random_host_4"),
]

@dataclass(frozen=True)
class RunRecord:
    experiment: str
    generations: int
    converged: bool
    time_taken: float


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

def run() -> None:
    """
    Run the analysis for all ablation experiments.
    """

    runs: list[RunRecord] = []
    for experiment in experiments:
        dataPath: Path = Path(experiment) / "data"

        for run in dataPath.iterdir():
            if not run.is_dir():
                continue

            if experiment == "genesis_activation" and run.name.split("_")[8] == "sin":
                continue

            dataFile: Path = run / "data.csv"
            experimentFile: Path = run / "experiment.txt"

            if not dataFile.exists() or not experimentFile.exists():
                continue

            expName: str = experiment

            if "genesis_random_host" in experiment:
                expName = f"Random Host: {experiment.split(os.path.sep)[2].split('_')[3]}"

            if experiment == "genesis_activation":
                expName = run.name.split('_')[8]

            if experiment == "genesis_neurons":
                expName = f"Neurons: {run.name.split('_')[8]}"

            generations, isEmulator = parse_generations(dataFile)
            converged: bool = parse_converged(experimentFile)
            time_taken: float = parse_time_taken(experimentFile)

            if isEmulator:
                time_taken = time_taken - 672 # Experiment time
                time_taken = time_taken - 60 # 1 minute for the emulator to start

            runs.append(
                RunRecord(
                    experiment=expName,
                    generations=generations,
                    converged=converged,
                    time_taken=time_taken,
                )
            )

    rows: list[dict[str, float | int | str]] = []
    for run in runs:
        rows.append(
            {
                "experiment": run.experiment,
                "generations": run.generations,
                "converged": run.converged,
                "time_taken": run.time_taken,
            }
        )
    with Path("ablation.csv").open("w", newline="", encoding="utf-8") as file_handle:
        writer = csv.DictWriter(file_handle, fieldnames=["experiment", "generations", "converged", "time_taken"])
        writer.writeheader()
        writer.writerows(rows)

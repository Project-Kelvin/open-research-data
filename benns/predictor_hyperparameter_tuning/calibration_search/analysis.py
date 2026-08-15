from __future__ import annotations

import csv
import re
from pathlib import Path
from typing import List, Tuple

BASE_COLUMNS = ["calibration", "architecture", "activation functions", "initialiser"]
BEST_COLUMNS = ["component", "calibration", "architecture", "activation functions", "initialiser", "MAPE"]


def parse_architecture_file(architecture_file: Path) -> Tuple[str, str, str]:
    lines = [line.strip() for line in architecture_file.read_text(encoding="utf-8").splitlines() if line.strip()]

    if len(lines) >= 4:
        # Most files follow: target (CPU/MEMORY), architecture, activation, initializer
        architecture, activation, initializer = lines[1], lines[2], lines[3]
    elif len(lines) >= 3:
        architecture, activation, initializer = lines[0], lines[1], lines[2]
    else:
        raise ValueError(f"Unexpected architecture format in {architecture_file}")

    return architecture, activation, initializer


def parse_mape(result_file: Path) -> str | None:
    pattern = re.compile(r"(?i)\bMAPE\b\s*,?\s*([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)")

    for line in result_file.read_text(encoding="utf-8").splitlines():
        match = pattern.search(line)
        if match:
            return match.group(1)

    return None


def collect_pivot_rows(
    root_dir: Path,
    result_filename: str,
    calibration_name_prefix: str,
) -> Tuple[List[dict[str, str]], List[str]]:
    rows: List[dict[str, str]] = []
    component_names: set[str] = set()

    calibration_dirs = sorted(
        [
            d
            for d in root_dir.iterdir()
            if d.is_dir()
            and d.name.startswith(calibration_name_prefix)
            and (d / "architecture").exists()
        ],
        key=lambda p: p.name,
    )

    for calibration_dir in calibration_dirs:
        architecture_file = calibration_dir / "architecture"
        architecture, activation, initializer = parse_architecture_file(architecture_file)

        row: dict[str, str] = {
            "calibration": calibration_dir.name,
            "architecture": architecture,
            "activation functions": activation,
            "initialiser": initializer,
        }
        has_component_value = False

        for result_file in sorted(calibration_dir.rglob(result_filename), key=lambda p: str(p)):
            mape = parse_mape(result_file)
            if mape is None:
                continue

            component = result_file.parent.name
            row[component] = mape
            component_names.add(component)
            has_component_value = True

        if has_component_value:
            rows.append(row)

    return rows, sorted(component_names)


def write_csv(output_file: Path, rows: List[dict[str, str]], columns: List[str]) -> None:
    with output_file.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=columns)
        writer.writeheader()
        writer.writerows(rows)


def extract_best_per_component(rows: List[dict[str, str]], components: List[str]) -> List[dict[str, str]]:
    best_by_component: dict[str, tuple[float, dict[str, str]]] = {}

    for row in rows:
        for component in components:
            value = row.get(component)
            if value is None or value == "":
                continue

            try:
                mape_value = float(value)
            except ValueError:
                continue

            current_best = best_by_component.get(component)
            if current_best is None or mape_value < current_best[0]:
                best_by_component[component] = (
                    mape_value,
                    {
                        "component": component,
                        "calibration": row["calibration"],
                        "architecture": row["architecture"],
                        "activation functions": row["activation functions"],
                        "initialiser": row["initialiser"],
                        "MAPE": value,
                    },
                )

    best_rows = [best_by_component[component][1] for component in sorted(best_by_component)]
    return best_rows


def main() -> None:
    # Package module lives in calibration_search/, but data folders are one level up.
    base_dir = Path(__file__).resolve().parent.parent

    cpu_rows, cpu_components = collect_pivot_rows(
        base_dir,
        "cpu_test_result.txt",
        "calibrations_v",
    )
    memory_rows, memory_components = collect_pivot_rows(
        base_dir,
        "memory_test_result.txt",
        "calibrations_mem_v",
    )

    cpu_csv = base_dir / "cpu_usage_prediction.csv"
    memory_csv = base_dir / "memory_usage_prediction.csv"
    cpu_best_csv = base_dir / "cpu_best_config_by_component.csv"
    memory_best_csv = base_dir / "memory_best_config_by_component.csv"

    write_csv(cpu_csv, cpu_rows, BASE_COLUMNS + cpu_components)
    write_csv(memory_csv, memory_rows, BASE_COLUMNS + memory_components)

    cpu_best_rows = extract_best_per_component(cpu_rows, cpu_components)
    memory_best_rows = extract_best_per_component(memory_rows, memory_components)

    write_csv(cpu_best_csv, cpu_best_rows, BEST_COLUMNS)
    write_csv(memory_best_csv, memory_best_rows, BEST_COLUMNS)

    print(f"Wrote {len(cpu_rows)} rows to {cpu_csv}")
    print(f"Wrote {len(memory_rows)} rows to {memory_csv}")
    print(f"Wrote {len(cpu_best_rows)} rows to {cpu_best_csv}")
    print(f"Wrote {len(memory_best_rows)} rows to {memory_best_csv}")


if __name__ == "__main__":
    main()

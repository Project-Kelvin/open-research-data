from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

HERE = Path(__file__).parent
BIN_WIDTH_MS = 10
MAJOR_TICK_MS = 50
MINOR_TICK_MS = 10
# Latency axis is right-censored: runs above this are sparse (1-3 per bin).
X_MAX_MS = 500
METHODS = ["benns", "gaha"]
BAR_COLOR = "#375c63"

plt.rcParams.update(
    {
        "font.family": "Liberation Serif",
        "font.size": 8,
        "axes.labelsize": 8,
        "xtick.labelsize": 7,
        "ytick.labelsize": 7,
        "axes.linewidth": 0.6,
        "xtick.major.width": 0.6,
        "ytick.major.width": 0.6,
        "xtick.minor.width": 0.4,
    }
)
# IEEE single-column width is 3.5 in.
FIG_SIZE = (3.5, 2.3)
DPI = 600
STATS = {"mean": "Mean", "median": "Median"}


def load() -> pd.DataFrame:
    data_files = sorted(
        data_file
        for data_file in HERE.glob("data_*.csv")
        if data_file.stem.removeprefix("data_").isdigit()
    )
    if not data_files:
        raise FileNotFoundError(f"No data CSV files found in {HERE}")
    df = pd.concat((pd.read_csv(data_file) for data_file in data_files), ignore_index=True)
    # 50000 is a sentinel latency for a failed online run.
    df = df[(df["online"] != 50000) & (df["online"] != 0)].copy()
    for method in METHODS:
        df[f"{method}_vs_online"] = (df[method] - df["online"]).abs()
    return df


def main() -> None:
    df = load()
    df = df[df["online"] < X_MAX_MS].copy()
    # Latency is the online (ground truth) latency, binned into 40 ms bins.
    df["bin"] = (df["online"] // BIN_WIDTH_MS).astype(int)
    bins = np.arange(df["bin"].min(), df["bin"].max() + 1)
    x_lim = (0, X_MAX_MS)

    for stat, label in STATS.items():
        values = {m: df.groupby("bin")[f"{m}_vs_online"].agg(stat) for m in METHODS}
        # Identical axis limits on both plots keep them directly comparable.
        positive_values = np.concatenate(
            [v.to_numpy()[v.to_numpy() > 0] for v in values.values()]
        )
        if positive_values.size == 0:
            raise ValueError(f"No positive {label.lower()} absolute errors for log scale")
        y_lim = (positive_values.min() * 0.8, max(v.max() for v in values.values()) * 1.05)

        for method in METHODS:
            s = values[method]
            fig, ax = plt.subplots(figsize=FIG_SIZE)
            ax.bar(
                s.index * BIN_WIDTH_MS,
                s.to_numpy(),
                width=BIN_WIDTH_MS,
                align="edge",
                color=BAR_COLOR,
                edgecolor="white",
                linewidth=0.2,
            )
            ax.set_xlabel("Median Traffic Latency")
            ax.set_ylabel(f"{label} Absolute Error")
            ax.set_xlim(*x_lim)
            ax.set_yscale("log")
            ax.set_ylim(*y_lim)
            ax.set_xticks(np.arange(0, x_lim[1] + MAJOR_TICK_MS, MAJOR_TICK_MS))
            ax.set_xticks(np.arange(0, x_lim[1] + MINOR_TICK_MS, MINOR_TICK_MS), minor=True)
            ax.tick_params(axis="x", which="major", length=3)
            ax.tick_params(axis="x", which="minor", length=1.5)
            fig.tight_layout()
            out = HERE / f"{method}_online_{stat}_ae_by_latency.png"
            fig.savefig(out, dpi=DPI)
            fig.savefig(out.with_suffix(".pdf"))
            plt.close(fig)
            print(f"Saved {out}")


if __name__ == "__main__":
    main()

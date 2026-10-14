from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

HERE = Path(__file__).parent
BIN_WIDTH_MS = 10
X_LIMIT_MS = 500
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
        df[f"{method}_vs_online"] = df[method] - df["online"]
    return df


def main() -> None:
    df = load()
    edges = np.arange(-X_LIMIT_MS, X_LIMIT_MS + BIN_WIDTH_MS, BIN_WIDTH_MS)
    counts = {m: np.histogram(df[f"{m}_vs_online"], bins=edges)[0] for m in METHODS}
    # Identical axis limits on both plots keep them directly comparable.
    x_lim = (-X_LIMIT_MS, X_LIMIT_MS)
    y_lim = (0, max(c.max() for c in counts.values()) * 1.05)

    for method in METHODS:
        fig, ax = plt.subplots(figsize=FIG_SIZE)
        ax.bar(
            edges[:-1],
            counts[method],
            width=BIN_WIDTH_MS,
            align="edge",
            color=BAR_COLOR,
            edgecolor=BAR_COLOR,
            linewidth=0.3,
        )
        ax.set_xlabel("Error")
        ax.set_ylabel("Frequency")
        ax.set_xlim(*x_lim)
        ax.set_ylim(*y_lim)
        ax.set_xticks(np.arange(-X_LIMIT_MS, X_LIMIT_MS + 1, 100))
        fig.tight_layout()
        out = HERE / f"{method}_online_error_hist.png"
        fig.savefig(out, dpi=DPI)
        fig.savefig(out.with_suffix(".pdf"))
        plt.close(fig)
        print(f"Saved {out}")


if __name__ == "__main__":
    main()

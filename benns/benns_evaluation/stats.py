from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import bootstrap, kendalltau, spearmanr

HERE = Path(__file__).parent


def summarize(values: pd.Series) -> dict:
    x = values.to_numpy(dtype=float)
    q1, q3 = np.percentile(x, [25, 75])
    try:
        res = bootstrap(
            (x,),
            np.mean,
            confidence_level=0.95,
            method="BCa",
            n_resamples=10000,
            random_state=42,
        )
        lo, hi = res.confidence_interval.low, res.confidence_interval.high
    except Exception:
        lo = hi = np.nan
    return {
        "n": len(x),
        "mean": x.mean(),  # equals MAE when x is an absolute error
        "median": np.median(x),
        "iqr": q3 - q1,
        "ci_bca_low": lo,
        "ci_bca_high": hi,
        "std": x.std(ddof=1),
    }


def main() -> None:
    data_files = sorted(
        data_file
        for data_file in HERE.glob("data_*.csv")
        if data_file.stem.removeprefix("data_").isdigit()
    )
    if not data_files:
        raise FileNotFoundError(f"No data CSV files found in {HERE}")

    df = pd.concat((pd.read_csv(data_file) for data_file in data_files), ignore_index=True)
    _analyze(df)


def _analyze(df: pd.DataFrame) -> None:
    # 50000 is a sentinel latency for a failed online run.
    df = df[df["online"] != 50000].copy()
    df = df[df["online"] != 0].copy()
    df["benns_vs_online"] = (df["benns"] - df["online"]).abs()
    df["gaha_vs_online"] = (df["gaha"] - df["online"]).abs()

    rows = []
    for topology, group in df.groupby("topology"):
        for col in ["gaha_vs_online", "benns_vs_online"]:
            rows.append({"topology": topology, "comparison": col, **summarize(group[col])})
    pd.DataFrame(rows).round(2).to_csv(HERE / "diff_stats.csv", index=False)

    rows = []
    for topology, group in df.groupby("topology"):
        for col in ["benns_time", "gaha_time", "online_time"]:
            rows.append({"topology": topology, "metric": col, **summarize(group[col])})
    pd.DataFrame(rows).round(2).to_csv(HERE / "time_stats.csv", index=False)

    # Online is treated as the ground truth.
    rows = []
    for topology, group in df.groupby("topology"):
        y = group["online"].to_numpy(dtype=float)
        for col in ["gaha", "benns"]:
            err = group[col].to_numpy(dtype=float) - y
            rows.append(
                {
                    "topology": topology,
                    "method": col,
                    "n": len(y),
                    "mae": np.abs(err).mean(),
                    "mape_percent": np.abs(err / y).mean() * 100,
                    "rmse": np.sqrt((err**2).mean()),
                    "r2": 1 - (err**2).sum() / ((y - y.mean()) ** 2).sum(),
                }
            )
    pd.DataFrame(rows).round(2).to_csv(HERE / "error_metrics.csv", index=False)

    # A value <= 100 is positive; online is the ground truth.
    def ratio(num: float, den: float) -> float:
        return num / den if den else np.nan

    def f1(p: float, r: float) -> float:
        return 2 * p * r / (p + r) if p + r else np.nan

    rows = []
    for topology, group in df.groupby("topology"):
        actual = group["online"] <= 100
        for col in ["gaha", "benns"]:
            pred = group[col] <= 100
            tp = int((pred & actual).sum())
            fp = int((pred & ~actual).sum())
            tn = int((~pred & ~actual).sum())
            fn = int((~pred & actual).sum())
            pos_p, pos_r = ratio(tp, tp + fp), ratio(tp, tp + fn)
            neg_p, neg_r = ratio(tn, tn + fn), ratio(tn, tn + fp)
            rows.append(
                {
                    "topology": topology,
                    "method": col,
                    "tp": tp,
                    "fp": fp,
                    "tn": tn,
                    "fn": fn,
                    "positive_precision": pos_p,
                    "positive_recall": pos_r,
                    "positive_f1": f1(pos_p, pos_r),
                    "negative_precision": neg_p,
                    "negative_recall": neg_r,
                    "negative_f1": f1(neg_p, neg_r),
                }
            )
    pd.DataFrame(rows).round(2).to_csv(HERE / "classification_metrics.csv", index=False)

    rows = []
    for topology, group in df.groupby("topology"):
        for col in ["gaha", "benns"]:
            rho, rho_p = spearmanr(group[col], group["online"])
            tau, tau_p = kendalltau(group[col], group["online"])
            rows.append(
                {
                    "topology": topology,
                    "comparison": f"{col}_vs_online",
                    "n": len(group),
                    "spearman_rho": rho,
                    "spearman_p": rho_p,
                    "kendall_tau": tau,
                    "kendall_p": tau_p,
                }
            )
    # p-values are kept at higher precision so small values are not shown as 0.
    out = pd.DataFrame(rows)
    out[["spearman_rho", "kendall_tau"]] = out[["spearman_rho", "kendall_tau"]].round(2)
    out[["spearman_p", "kendall_p"]] = out[["spearman_p", "kendall_p"]].map(lambda v: float(f"{v:.3g}"))
    out.to_csv(HERE / "rank_correlation.csv", index=False)


if __name__ == "__main__":
    main()

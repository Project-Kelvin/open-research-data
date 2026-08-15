"""
Convergence and generations-to-converge analysis for GENESIS vs BEGA / GAHA
across topologies (dc, milan, 25n50e).

Expects a CSV with one row per run:
    algorithm,topology,generations,converged,time_taken

Non-converged runs are treated as right-censored at the generation budget
(e.g. 100), not dropped and not treated as if 100 were a true convergence
value. This matters for both the log-rank test and the Cox model below.

Install deps:
    pip install lifelines pandas scipy --break-system-packages
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import fisher_exact
from lifelines import KaplanMeierFitter, CoxPHFitter
from lifelines.statistics import logrank_test

plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["Liberation Serif"]  # or 'Nimbus Roman'
plt.rcParams["figure.constrained_layout.use"] = True


# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
CSV_PATH = "aggregation.csv"  # one row per run
REFERENCE_ALGO = "genesis"  # the algorithm you're comparing everything else against
BASELINES = ["bega", "gaha"]  # algorithms to compare REFERENCE_ALGO against
TOPOLOGIES = ["dc", "milan", "25n50e"]


def haldane_or(a, b, c, d):
    """Odds ratio with Haldane-Anscombe continuity correction (+0.5 to each
    cell) applied only when at least one cell is zero. Avoids inf/nan ORs
    when a group has 0% or 100% convergence."""
    if 0 in (a, b, c, d):
        a, b, c, d = a + 0.5, b + 0.5, c + 0.5, d + 0.5
    return (a * d) / (b * c)


def vargha_delaney_a12(x, y):
    """Probability that a random value from x is smaller than a random value
    from y (ties count as 0.5). For generations-to-converge, A12 < 0.5 means
    x (typically your algorithm) tends to need fewer generations than y."""
    m, n = len(x), len(y)
    wins = sum(1 for xi in x for yi in y if xi < yi)
    ties = sum(1 for xi in x for yi in y if xi == yi)
    return (wins + 0.5 * ties) / (m * n)


def a12_magnitude(a12):
    mag = abs(a12 - 0.5)
    if mag < 0.06:
        return "negligible"
    elif mag < 0.14:
        return "small"
    elif mag < 0.21:
        return "medium"
    return "large"


def main():
    df = pd.read_csv(CSV_PATH)
    df["converged"] = df["converged"].astype(bool)

    # -----------------------------------------------------------------
    # 1. Convergence rate: Fisher's exact test + effect sizes
    # -----------------------------------------------------------------
    print("=" * 72)
    print("1. CONVERGENCE RATE — Fisher's exact test + effect sizes")
    print("=" * 72)
    for topo in TOPOLOGIES:
        sub = df[df.topology == topo]
        print(f"\n-- {topo} --")
        counts = sub.groupby("algorithm")["converged"].agg(["sum", "count"])
        print(counts.rename(columns={"sum": "converged", "count": "n"}))

        ref = sub[sub.algorithm == REFERENCE_ALGO]["converged"]
        p_ref = ref.mean()
        a, b = ref.sum(), len(ref) - ref.sum()

        for other in BASELINES:
            oth = sub[sub.algorithm == other]["converged"]
            p_oth = oth.mean()
            c, d = oth.sum(), len(oth) - oth.sum()

            table = [[a, b], [c, d]]
            _, p_fisher = fisher_exact(table)

            risk_diff = p_ref - p_oth
            rel_risk = p_ref / p_oth if p_oth > 0 else np.inf
            odds = haldane_or(a, b, c, d)

            print(
                f"  {REFERENCE_ALGO} ({p_ref:.0%}) vs {other} ({p_oth:.0%}): "
                f"Fisher p={p_fisher:.6f} | risk diff={risk_diff:+.2f} | "
                f"RR={rel_risk:.2f} | OR(Haldane)={odds:.2f}"
            )

    # -----------------------------------------------------------------
    # 2. Generations-to-converge: log-rank test (handles censoring)
    # -----------------------------------------------------------------
    print("\n" + "=" * 72)
    print("2. LOG-RANK TEST — generations-to-converge, censored at budget")
    print("=" * 72)
    for topo in TOPOLOGIES:
        sub = df[df.topology == topo]
        ref = sub[sub.algorithm == REFERENCE_ALGO]
        print(f"\n-- {topo} --")
        for other in BASELINES:
            oth = sub[sub.algorithm == other]
            result = logrank_test(
                ref["generations"],
                oth["generations"],
                event_observed_A=ref["converged"],
                event_observed_B=oth["converged"],
            )
            print(
                f"  {REFERENCE_ALGO} vs {other}: "
                f"chi2={result.test_statistic:.3f}, p={result.p_value:.6f}"
            )

    # -----------------------------------------------------------------
    # 3. Kaplan-Meier median generations-to-converge
    # -----------------------------------------------------------------
    print("\n" + "=" * 72)
    print("3. KAPLAN-MEIER median generations-to-converge")
    print("=" * 72)
    for topo in TOPOLOGIES:
        sub = df[df.topology == topo]
        print(f"\n-- {topo} --")
        for algo in [REFERENCE_ALGO] + BASELINES:
            a_sub = sub[sub.algorithm == algo]
            kmf = KaplanMeierFitter()
            kmf.fit(a_sub["generations"], event_observed=a_sub["converged"])
            median = kmf.median_survival_time_
            print(
                f"  {algo}: KM median = {median} "
                f"(converged {a_sub['converged'].sum()}/{len(a_sub)})"
            )

    # -----------------------------------------------------------------
    # 4. Hazard ratio via Cox proportional hazards (skip on perfect separation)
    # -----------------------------------------------------------------
    print("\n" + "=" * 72)
    print("4. HAZARD RATIO — Cox proportional hazards model")
    print("=" * 72)
    for topo in TOPOLOGIES:
        sub = df[df.topology == topo]
        print(f"\n-- {topo} --")
        for other in BASELINES:
            pair = sub[sub.algorithm.isin([REFERENCE_ALGO, other])].copy()
            pair["is_other"] = (pair.algorithm == other).astype(int)

            ref_conv = pair.loc[pair.algorithm == REFERENCE_ALGO, "converged"]
            oth_conv = pair.loc[pair.algorithm == other, "converged"]
            if (
                ref_conv.nunique() <= 1
                or oth_conv.nunique() <= 1
                and (oth_conv.sum() == 0 or ref_conv.sum() == len(ref_conv))
            ):
                # near-complete separation (e.g. 20/20 vs 0/20) makes the HR
                # a numerical artifact rather than a real estimate
                print(
                    f"  {REFERENCE_ALGO} vs {other}: skipped — convergence rate "
                    f"perfectly/near-perfectly separates the groups "
                    f"({REFERENCE_ALGO} {ref_conv.sum()}/{len(ref_conv)}, "
                    f"{other} {oth_conv.sum()}/{len(oth_conv)}). "
                    f"Report the risk difference from section 1 instead."
                )
                continue

            cph = CoxPHFitter()
            cph.fit(
                pair[["generations", "converged", "is_other"]],
                duration_col="generations",
                event_col="converged",
            )
            hr = np.exp(cph.params_["is_other"])
            ci_low, ci_high = np.exp(cph.confidence_intervals_.loc["is_other"])
            p_val = cph.summary.loc["is_other", "p"]
            print(
                f"  {REFERENCE_ALGO} vs {other}: HR={hr:.3f} "
                f"(95% CI {ci_low:.3f}-{ci_high:.3f}), p={p_val:.4f} "
                f"[HR<1 means {other} converges more slowly than {REFERENCE_ALGO}]"
            )

    # -----------------------------------------------------------------
    # 5. Vargha-Delaney A12 on generations, converged runs only (secondary)
    # -----------------------------------------------------------------
    print("\n" + "=" * 72)
    print("5. VARGHA-DELANEY A12 — generations among CONVERGED runs (secondary)")
    print("=" * 72)
    conv = df[df.converged]
    for topo in TOPOLOGIES:
        sub = conv[conv.topology == topo]
        print(f"\n-- {topo} --")
        ref = sub[sub.algorithm == REFERENCE_ALGO]["generations"].values
        if len(ref) == 0:
            print(f"  {REFERENCE_ALGO} has no converged runs here — n/a")
            continue
        for other in BASELINES:
            oth = sub[sub.algorithm == other]["generations"].values
            if len(oth) == 0:
                print(
                    f"  {REFERENCE_ALGO} vs {other}: {other} has 0 converged runs — A12 undefined"
                )
                continue
            a12 = vargha_delaney_a12(ref, oth)
            print(
                f"  {REFERENCE_ALGO} vs {other}: A12={a12:.3f} "
                f"({a12_magnitude(a12)} effect; A12<0.5 favors {REFERENCE_ALGO})"
            )

    _df = pd.read_csv(CSV_PATH)
    _df["converged"] = _df["converged"].astype(bool)
    plot_km_curves(_df)


def plot_km_curves(df, out_path="km_plot.png"):
    """One KM subplot per topology, one curve per algorithm (REFERENCE_ALGO + BASELINES)."""
    algos = [REFERENCE_ALGO] + BASELINES
    palette = ["#2563eb", "#dc2626", "#16a34a", "#a855f7", "#ea580c"]
    colors = {a: palette[i % len(palette)] for i, a in enumerate(algos)}

    fig, axes = plt.subplots(
        1, len(TOPOLOGIES), figsize=(5 * len(TOPOLOGIES), 4.5), sharey=True
    )
    if len(TOPOLOGIES) == 1:
        axes = [axes]

    topoNames = {
        "dc": "DC 2",
        "milan": "Milan",
        "25n50e": "25N50E",
    }
    algorithmNames = {
        REFERENCE_ALGO: "GENESIS",
        "bega": "BEGA",
        "gaha": "REGA",
    }
    for ax, topo in zip(axes, TOPOLOGIES):
        sub = df[df.topology == topo]
        for algo in algos:
            a_sub = sub[sub.algorithm == algo]
            kmf = KaplanMeierFitter()
            kmf.fit(
                a_sub["generations"],
                event_observed=a_sub["converged"],
                label=algorithmNames.get(algo, algo.upper()),
            )
            kmf.plot_survival_function(
                ax=ax, color=colors[algo], ci_show=True, linewidth=2
            )
        ax.set_title(topoNames.get(topo, topo.upper()), fontsize=13)
        ax.set_xlabel("Generations")
        ax.set_ylim(-0.02, 1.02)
        ax.grid(alpha=0.3)
        ax.legend(loc="upper right", fontsize=9)

    axes[0].set_ylabel("Proportion\nNOT yet converged")

    plt.tight_layout()
    plt.savefig(out_path, dpi=300, bbox_inches="tight")
    print(f"\nKM plot saved to {out_path}")


if __name__ == "__main__":
    main()

""" "
Defines the analysis functions for the phenotype exploration experiments.
"""

import os
import polars as pl
import matplotlib.pyplot as plt
import seaborn as sns

plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["Liberation Serif"]  # or 'Nimbus Roman'
sns.set_theme(style="whitegrid", context="paper", font_scale=2, font="Liberation Serif")
plt.rcParams["figure.constrained_layout.use"] = True

def run() -> None:
    """
    Runs the analysis for the phenotype exploration experiments.
    """
    selectedRows: list[list[str]] = []
    with open(
        os.path.join("phenotype_genesis", "phenotype_input_output_records.csv"), "r"
    ) as f:
        for line in f:
            row = line.strip().split(",")
            selectedRow: list[str] = []
            selectedRow.extend(row[0:6])
            selectedRow.append(row[-1])
            selectedRows.append(selectedRow)

    data: pl.DataFrame = pl.DataFrame(
        selectedRows,
        orient="row",
        schema=[
            "w1",
            "w2",
            "w3",
            "w4",
            "w5",
            "w6",
            "id",
        ],
    )

    aggData: pl.DataFrame = data.group_by("id").agg(pl.struct(["w1", "w2", "w3", "w4", "w5", "w6"]).n_unique().alias("Duplicates")).sort("Duplicates", descending=True)
    print("Duplicate Mean", aggData["Duplicates"].mean())
    print("Duplicate Median", aggData["Duplicates"].median())
    print("Duplicate Q1", aggData["Duplicates"].quantile(0.25))
    print("Duplicate Q3", aggData["Duplicates"].quantile(0.75))

    aggData.write_csv("duplicates_distribution.csv")
    topId: str | None = aggData[0, "id"] if aggData.height > 0 else None

    plt.figure(figsize=(10, 6))
    sns.histplot(
        data=aggData.to_pandas(),
        x="Duplicates",
        binwidth=1,
        alpha=1,
        color="#375c63"
    )
    plt.xlabel("Number of Genotypes Discovered per Phenotype")
    plt.ylabel("Frequency")
    plt.gcf().savefig("duplicates_distribution.png")
    plt.close()

    if topId is not None:
        topIdRows: pl.DataFrame = data.filter(pl.col("id") == topId)

        fig, axes = plt.subplots(2, 3, figsize=(50, 50))
        for index, column in enumerate(["w1", "w2", "w3", "w4", "w5", "w6"]):
            axis = axes[index // 3][index % 3]
            sns.histplot(
                data=topIdRows.select(column).sort(column).to_pandas(),
                x=column,
                binwidth=0.1,
                ax=axis,
                alpha=1,
                color="#375c63"
            )

            axis.tick_params(axis="x", rotation=90)
            axis.set_title(f"{column} Distribution for id {topId}")

        plt.tight_layout()
        fig.savefig("top_id_weights_distribution.png")

    algorithms: list[str] = ["phenotype_genesis", "phenotype_bega", "phenotype_gaha"]
    dataFiles: list[str] = [
        "phenotype_host_eval",
        "phenotype_cc_eval",
        "phenotype_cc_host_eval",
        "phenotype_host_links_eval",
        "phenotype_eval"
    ]

    dfs: dict[str, pl.DataFrame] = {}

    for algorithm in algorithms:
        for dataFile in dataFiles:
            filePath: str = os.path.join(algorithm, f"{dataFile}.csv")
            if os.path.exists(filePath):
                with open(filePath, "r") as f:
                    times: list[int] = []
                    for line in f:
                        row = line.strip().split(",")
                        times.append(int(row[-1]))
                    df: pl.DataFrame = pl.DataFrame({"id": [i for i in range(len(times))], "times": times})

                    dfs[f"{algorithm}_{dataFile}"] = df
    print("Generated data. Now plotting.")
    fig, axes = plt.subplots(3, 5, figsize=(20, 10))

    columnTitles: dict[str, str] = {
        "phenotype_host_eval": "VNF-EM",
        "phenotype_cc_eval": "VNF-CC",
        "phenotype_cc_host_eval": "VNF-CC,VNF-EM",
        "phenotype_host_links_eval": "VNF-EM,VL-EM",
        "phenotype_eval": "VNF-CC,VNF-EM,VL-EM",
    }
    algorithmTitles: dict[str, str] = {
        "phenotype_genesis": "GENESIS",
        "phenotype_bega": "BEGA",
        "phenotype_gaha": "REGA",
    }

    for index, dataFile in enumerate(dataFiles):
        axes[0][index].set_title(columnTitles[dataFile])

    for algorithm in algorithms:
        for index, dataFile in enumerate(dataFiles):
            key: str = f"{algorithm}_{dataFile}"
            if key in dfs:
                axis = axes[algorithms.index(algorithm)][index]
                df = dfs[key]
                axis.bar(
                    df["id"].to_list(),
                    df["times"].to_list(),
                    width=1.0,
                    color="#375c63",
                    linewidth=0,
                )
                axis.set_xlabel("Permutation ID")
                if index == 0:
                    axis.set_ylabel(f"{algorithmTitles[algorithm]}\nFrequency")
                else:
                    axis.set_ylabel("Frequency")
                if dataFile == "phenotype_eval":
                    axis.set_xticks([0, 20000, 40000, 60000, 80000])
                if dataFile == "phenotype_host_eval":
                    axis.set_xticks([0, 50, 100, 150, 200, 250])
                if dataFile == "phenotype_cc_eval":
                    axis.set_xticks([0, 5, 10, 15, 20, 25])
                if dataFile == "phenotype_cc_host_eval":
                    axis.set_xticks([0, 1000, 3000, 6000])
                if dataFile == "phenotype_host_links_eval":
                    axis.set_xticks([0, 500, 1500, 3000])

    fig.savefig("algorithm_times_distribution.png", dpi=300)

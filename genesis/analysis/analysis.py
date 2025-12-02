"""
Defines the analysis script for the Genesis experiments.
"""

import os
import polars as pl
import numpy as np


def generateExperimentName(name: str) -> str:
    """
    Generates experiment name.

    Parameters:
        name (str): The base name of the experiment.

    Returns:
        str: The generated experiment name.
    """

    expNames: list[str] = name.split("_")
    expNames[0] = str(int(expNames[0]) * 4)
    expNames[1] = str(int(float(expNames[1]) * 10))
    expNames[2] = "A" if expNames[2] == "False" else "B"
    name = "_".join(expNames)

    return name


def run():
    """
    Runs the script to generate experiment data from experiment results.
    """

    data: "list[list[str]]" = []
    algorithms: list[str] = ["genesis", "ga_hybrid_100", "ga_hybrid_2000", "gaha", "gda"]
    algorithmNames: list[str] = ["GENESIS", "BEGA 100", "BEGA 2000", "GAHA", "GDA"]

    for algorithmName, algorithmDisplayName in zip(algorithms, algorithmNames):
        noOfGenerations: list[int] = []

        if algorithmName == "gda":
            experimentFiles: list[str] = [f for f in os.listdir(algorithmName) if f.endswith(".txt")]
            for experimentFile in experimentFiles:
                with open(os.path.join(algorithmName, experimentFile), "r", encoding="utf-8") as f:
                    lines = f.readlines()
                    convergenceTime: float = 0.0
                    didConverge: bool = False
                    bestInd: tuple[float, float] = None
                    ar: float = 0.0
                    lat: float = 0.0
                    for line in lines:
                        if "Time taken:" in line:
                            convergenceTime = float(line.split("Time taken: ")[1])
                        if "Acceptance Ratio" in line:
                            ar = float(line.split("Acceptance Ratio: ")[1])
                        if "Latency" in line:
                            lat = float(line.split("Latency: ")[1])
                    if ar >=1 and lat <=100:
                        didConverge = True
                    bestInd = (lat, ar)
                data.append(
                    [
                        algorithmDisplayName,
                        generateExperimentName(experimentFile.replace(".txt", "")),
                        str(didConverge),
                        str(bestInd[0]) if bestInd is not None else "N/A",
                        str(bestInd[1]) if bestInd is not None else "N/A",
                        str(convergenceTime),
                    ]
                )
            continue

        computedLatencies: list[float] = []
        measuredLatencies: list[float] = []
        for expName in os.listdir(algorithmName):
            if not os.path.isdir(os.path.join(algorithmName, expName)):
                continue
            convergenceTime: float = 0.0
            didConverge: bool = False
            bestInd: tuple[float, float] = None

            if algorithmName == "gaha":
                with open(os.path.join(algorithmName, expName, "experiment.txt"), "r", encoding="utf-8") as f:
                    lines = f.readlines()
                    ar: float = 0
                    lat: float = 0
                    measuredLat: float = 0
                    for line in lines:
                        if "Evolution Time taken:" in line:
                            convergenceTime = float(line.split("Evolution Time taken: ")[1])
                        if "Average Latency(computed):" in line:
                            lat = float(line.split("Average Latency(computed): ")[1])
                            computedLatencies.append(lat)
                        if "Average Latency(measured):" in line:
                            measuredLat = float(line.split("Average Latency(measured): ")[1])
                            measuredLatencies.append(measuredLat)
                            lat = measuredLat if measuredLat > 0 else lat
                        if "Acceptance Ratio:" in line:
                            ar = float(line.split("Acceptance Ratio: ")[1])
                if ar >=1 and lat <=500:
                    didConverge = True
                    bestInd = (lat, ar)
            else:
                # Get convergence time
                with open(os.path.join(algorithmName, expName, "experiment.txt"), "r", encoding="utf-8") as f:
                    lines = f.readlines()
                    for line in lines:
                        if "Time taken:" in line:
                            convergenceTime = float(line.split("Time taken: ")[1])

                # Check if there was convergence
                df: pl.DataFrame = pl.read_csv(
                    os.path.join(algorithmName, expName, "pfs.csv"),
                    schema={
                        "method": pl.String,
                        "generation": pl.Float64,
                        "latency": pl.Float64,
                        "ar": pl.Float64,
                    },
                )
                noOfGenerations.append(
                    df.filter((pl.col("method") == "surrogate"))
                    .select(pl.max("generation"))
                    .to_numpy()[0][0]
                )
                maxGen: float = df.select(pl.col("generation").max()).to_numpy()[0][0]
                maxGenRow: np.ndarray = df.filter((pl.col("method") == "emulator") & (pl.col("generation") == maxGen)).to_numpy()
                if len(maxGenRow) > 0:
                    didConverge = True
                    bestInd = (maxGenRow[0][2], maxGenRow[0][3])

            data.append(
                [
                    algorithmDisplayName,
                    generateExperimentName(expName),
                    str(didConverge),
                    str(bestInd[0]) if bestInd is not None else "N/A",
                    str(bestInd[1]) if bestInd is not None else "N/A",
                    str(convergenceTime),
                ]
            )
        if len(noOfGenerations) > 0:
            mean_gen = np.mean(noOfGenerations)
            std_gen = np.std(noOfGenerations, ddof=1)
            n = len(noOfGenerations)
            z = 1.96  # 95% CI for normal distribution
            ci = z * std_gen / np.sqrt(n)
            print(f"{algorithmDisplayName}: mean={mean_gen:.2f}, 95% CI=±{ci:.2f}")
            print(f"Min generations: {min(noOfGenerations)}, Max generations: {max(noOfGenerations)}")

        diffLatencies: list[float] = []
        if len(computedLatencies) > 0 and len(measuredLatencies) > 0:
            for compLat, measLat in zip(computedLatencies, measuredLatencies):
                diffLatencies.append(compLat - measLat)

            mean_diff = np.mean(diffLatencies)
            std_diff = np.std(diffLatencies, ddof=1)
            n_diff = len(diffLatencies)
            z = 1.96  # 95% CI for normal distribution
            ci_diff = z * std_diff / np.sqrt(n_diff)
            print(f"Mean GAHA Latency Difference (Computed - Measured): {mean_diff:.2f}, 95% CI=±{ci_diff:.2f}")
            print(f"Min latency difference: {min(diffLatencies):.2f}, Max latency difference: {max(diffLatencies):.2f}")
    expData: pl.DataFrame = pl.DataFrame(
        data,
        schema=[
            "algorithm",
            "experiment",
            "did_converge",
            "best_latency",
            "best_ar",
            "convergence_time",
        ],
    )
    expData.write_csv(os.path.join("analysis", "analysis_results.csv"))

"""
This defines the Page trend test, which is a non-parametric test for ordered alternatives in repeated measures designs.
It is used to determine if there is a significant trend across multiple conditions or time points.
"""

import os
import polars as pl
from scipy.stats import norm, binomtest

expDirs: list[str] = [
    "8_0.1_False_10_2",
    "8_0.1_False_5_2",
    "8_0.1_False_10_1",
    "8_0.1_True_10_2",
    "8_0.2_False_10_2",
]

def generateExperimentDFs(
    algorithms: list[tuple[str, str]],
) -> tuple[dict[str, dict[str, pl.DataFrame]], dict[str, dict[str, int]]]:
    """
    Generates dataframes for each experiment, which can then be used for statistical analysis.

    Parameters:
        algorithms (list[tuple[str, str]]): A list of tuples containing algorithm names and their corresponding paths.

    Returns:
        tuple[dict[str, dict[str, pl.DataFrame]], dict[str, dict[str, int]]]: A tuple containing a dictionary with dataframes for latency and acceptance ratio for each algorithm, and a dictionary with convergence information.
    """

    data: dict[str, dict[str, pl.DataFrame]] = {}
    convergence: dict[str, dict[str, int]] = {}

    for algorithmName, algorithmPath in algorithms:
        expDirs: list[str] = [
            d
            for d in os.listdir(algorithmPath)
            if os.path.isdir(os.path.join(algorithmPath, d))
        ]
        expDataLatency: pl.DataFrame = pl.DataFrame()
        expDataAR: pl.DataFrame = pl.DataFrame()
        for expDir in expDirs:
            if expDir not in expDirs:
                continue
            pfData: pl.DataFrame = pl.read_csv(
                os.path.join(algorithmPath, expDir, "data.csv")
            )
            pfData.columns = pl.Series(pfData.columns).str.strip_chars()
            stringCols: list[str] = [
                col for col, dtype in pfData.schema.items() if dtype == pl.Utf8
            ]
            if stringCols:
                pfData = pfData.with_columns(
                    [pl.col(col).str.strip_chars().alias(col) for col in stringCols]
                )

            didConverge: bool = False
            if algorithmName != "gaha" and algorithmName != "bega_online":
                didConverge = pfData.filter(pl.col("method") == "emulator").height > 0
                pfData = pfData.filter(pl.col("method") != "emulator")

            if didConverge:
                lastGeneration: int = int(
                    pfData.tail(1).select(pl.col("generation")).item()
                )
                if algorithmName not in convergence:
                    convergence[algorithmName] = {}
                convergence[algorithmName][expDir] = lastGeneration
            else:
                if algorithmName not in convergence:
                    convergence[algorithmName] = {}
                convergence[algorithmName][expDir] = -1

            castExprs: list[pl.Expr] = []
            for col in pfData.columns:
                if col == "method":
                    castExprs.append(pl.col(col).cast(pl.Utf8))
                elif col == "generation":
                    castExprs.append(pl.col(col).cast(pl.Int64))
                else:
                    castExprs.append(pl.col(col).cast(pl.Float64))
            pfData = pfData.with_columns(castExprs)

            noOfGenerations: int = 500 if algorithmName != "bega_online" and algorithmName != "genesis_online" else 15
            if pfData.height < noOfGenerations:
                missingRows: int = noOfGenerations - pfData.height
                generationStart: int = (
                    int(pfData.select(pl.col("generation").max()).item())
                    if pfData.height > 0
                    else -1
                )

                padData: dict[str, list] = {}
                for col in pfData.columns:
                    if col == "generation":
                        padData[col] = list(
                            range(
                                generationStart + 1, generationStart + 1 + missingRows
                            )
                        )
                    elif col == "method":
                        padData[col] = ["surrogate"] * missingRows
                    else:
                        if "ar" in col:
                            padData[col] = list(
                                float(x) for x in range(2, missingRows + 2)
                            )
                        else:
                            padData[col] = list(
                                float(-x) for x in range(2, missingRows + 2)
                            )

                pfData = pl.concat([pfData, pl.DataFrame(padData)], how="vertical")

            latData: pl.DataFrame = pfData.select(
                [
                    pl.col("generation"),
                    pl.col("min_latency").alias(expDir),
                ]
            )

            if expDataLatency.is_empty():
                expDataLatency = latData
            else:
                expDataLatency = expDataLatency.join(
                    latData, on="generation", how="outer"
                )
                if "generation_right" in expDataLatency.columns:
                    expDataLatency = expDataLatency.drop("generation_right")

            arData: pl.DataFrame = pfData.select(
                [
                    pl.col("generation"),
                    pl.col("max_ar").alias(expDir),
                ]
            )
            if expDataAR.is_empty():
                expDataAR = arData
            else:
                expDataAR = expDataAR.join(arData, on="generation", how="outer")
                if "generation_right" in expDataAR.columns:
                    expDataAR = expDataAR.drop("generation_right")

        # Bin 50 generations together and take the mean of the values in each bin
        # if not expDataLatency.is_empty():
        #     latencyValueCols: list[str] = [
        #         col for col in expDataLatency.columns if col != "generation"
        #     ]
        #     expDataLatency = (
        #         expDataLatency.with_columns(
        #             (pl.col("generation") // 50).alias("generation_bin")
        #         )
        #         .group_by("generation_bin")
        #         .agg([pl.col(col).mean().alias(col) for col in latencyValueCols])
        #         .sort("generation_bin")
        #         .rename({"generation_bin": "generation"})
        #     )

        # if not expDataAR.is_empty():
        #     arValueCols: list[str] = [col for col in expDataAR.columns if col != "generation"]
        #     expDataAR = (
        #         expDataAR.with_columns((pl.col("generation") // 50).alias("generation_bin"))
        #         .group_by("generation_bin")
        #         .agg([pl.col(col).mean().alias(col) for col in arValueCols])
        #         .sort("generation_bin")
        #         .rename({"generation_bin": "generation"})
        #     )

        data[algorithmName] = {"latency": expDataLatency, "ar": expDataAR}

    return data, convergence


def generateRanks(
    algo1: str,
    algo2: str,
    data: dict[str, dict[str, pl.DataFrame]],
    convergence: dict[str, dict[str, int]],
) -> tuple[pl.DataFrame, pl.DataFrame]:
    """
    Generates ranks for two algorithms based on their performance data.

    Parameters:
        algo1 (str): The name of the first algorithm.
        algo2 (str): The name of the second algorithm.
        data (dict[str, dict[str, pl.DataFrame]]): A dictionary containing the performance data for each algorithm.
        convergence (dict[str, dict[str, int]]): A dictionary containing convergence information for both algorithms.

    Returns:
        tuple[pl.DataFrame, pl.DataFrame]: A tuple containing two dataframes, one for latency ranks and one for acceptance ratio ranks.
    """

    fitness: list[str] = ["latency", "ar"]
    rankedData: list[pl.DataFrame] = []

    for fit in fitness:
        algo1Data: pl.DataFrame = data[algo1][fit]
        algo2Data: pl.DataFrame = data[algo2][fit]

        common_columns: list[str] = [
            col
            for col in algo1Data.columns
            if col in algo2Data.columns and col != "generation"
        ]
        rankData: pl.DataFrame = algo1Data.select(
            [
                (pl.col(col) - algo2Data.get_column(col)).alias(col)
                for col in common_columns
            ]
            + [pl.col("generation")]
        )

        rankData = rankData.select(
            [
                pl.col(col).rank(descending=fit == "latency").alias(col)
                for col in rankData.columns
                if col != "generation"
            ]
            + [pl.col("generation")]
        )

        for col in rankData.columns:
            if col == "generation":
                continue
            algo1Converged = convergence[algo1][col]
            algo2Converged = convergence[algo2][col]

            if (algo1Converged < algo2Converged and algo1Converged != -1) or (
                algo1Converged != -1 and algo2Converged == -1
            ):
                rankData = rankData.with_columns(
                    pl.when(pl.col("generation") >= algo1Converged)
                    .then(
                        pl.col(col)
                        .filter(pl.col("generation") == algo1Converged)
                        .first()
                        + pl.col("generation")
                        - algo1Converged
                    )
                    .otherwise(pl.col(col))
                    .alias(col)
                )
            elif (algo2Converged < algo1Converged and algo2Converged != -1) or (
                algo2Converged != -1 and algo1Converged == -1
            ):
                rankData = rankData.with_columns(
                    pl.when(pl.col("generation") >= algo2Converged)
                    .then(
                        pl.col(col)
                        .filter(pl.col("generation") == algo2Converged)
                        .first()
                        - pl.col("generation")
                        + algo2Converged
                    )
                    .otherwise(pl.col(col))
                    .alias(col)
                )

        # Add a column that sums the rows
        rankData = rankData.with_columns(
            pl.sum_horizontal(
                [pl.col(col) for col in rankData.columns if col != "generation"]
            ).alias("sum")
        )

        rankedData.append(rankData)

    return tuple(rankedData)


def generateL(latency: pl.DataFrame, ar: pl.DataFrame) -> tuple[float, float]:
    """
    Generates a combined dataframe for latency and acceptance ratio.

    Parameters:
        latency (pl.DataFrame): A dataframe containing latency data.
        ar (pl.DataFrame): A dataframe containing acceptance ratio data.

    Returns:
        tuple[float, float]: A tuple containing the L of latency and acceptance ratio.
    """

    latencySums: list[float] = latency.select(pl.col("sum")).to_series().to_list()
    arSums: list[float] = ar.select(pl.col("sum")).to_series().to_list()

    lLatency: float = 0.0
    lAr: float = 0.0

    index: int = 1
    for lat, ar in zip(latencySums, arSums):
        lLatency += index * lat
        lAr += index * ar
        index += 1

    return lLatency, lAr


def generateZ(lLatency: float, lAr: float, gen: int, exp: int) -> tuple[float, float]:
    """
    Generates the Z statistic for latency and acceptance ratio.

    Parameters:
        lLatency (float): The L of latency.
        lAr (float): The L of acceptance ratio.
        gen (int): The number of generations.
        exp (int): The number of experiments.

    Returns:
        tuple[float, float]: A tuple containing the Z statistic for latency and acceptance ratio.
    """

    xSquareLatency: float = (
        (12 * (lLatency - 0.5)) - (3 * exp * gen * (gen + 1) ** 2)
    ) / (gen * (gen + 1) * (exp * (gen - 1)) ** (0.5))
    xSquareAr: float = ((12 * (lAr - 0.5)) - (3 * exp * gen * (gen + 1) ** 2)) / (
        gen * (gen + 1) * (exp * (gen - 1)) ** (0.5)
    )

    return xSquareLatency, xSquareAr


def calculatePValue(z: float) -> float:
    """
    Calculates the p-value for a given Z statistic.

    Parameters:
        z (float): The Z statistic.

    Returns:
        float: The p-value corresponding to the Z statistic.
    """

    pValue: float = 1 - norm.cdf(z)

    return pValue


def generateSignTestScore(
    algo1: str, algo2: str, data: dict[str, dict[str, int]]
) -> tuple[float, float]:
    """
    Generates the score for a sign test comparing two algorithms.

    Parameters:
        algo1 (str): The name of the first algorithm.
        algo2 (str): The name of the second algorithm.
        data (dict[str, dict[str, int]]): A dictionary containing the performance data for each algorithm.

    Returns:
        tuple[float, float]: A tuple containing the p value and the Cohen g
    """

    wins: int = 0
    losses: int = 0
    total: int = 0

    for exp in data[algo1].keys():
        if exp not in expDirs:
            continue
        if exp not in data[algo2]:
            continue

        total += 1
        if data[algo1][exp] != -1 and data[algo2][exp] == -1:
            wins += 1
        elif data[algo1][exp] == -1 and data[algo2][exp] != -1:
            losses += 1

    z: float = (
        (losses - ((losses + wins) * 0.5))
        / ((losses + wins) * (0.5 * (1 - 0.5))) ** 0.5
        if losses + wins > 0
        else 0.0
    )

    cohen: float = wins / (wins + losses) if wins + losses > 0 else 0.0
    cohen = cohen - 0.5

    return norm.cdf(z), cohen

def generateSignTestScore2(
    algo1: str, algo2: str, path: str
) -> tuple[float, float]:
    """
    Generates the score for a sign test comparing two algorithms.

    Parameters:
        algo1 (str): The name of the first algorithm.
        algo2 (str): The name of the second algorithm.
        path (str): The path to the directory containing the performance data for each algorithm.

    Returns:
        tuple[float, float]: A tuple containing the p value and the Cohen g
    """

    wins: int = 0
    losses: int = 0
    total: int = 0

    data: pl.DataFrame = pl.read_csv(path).select([algo1, algo2])

    for row in data.iter_rows(named=True):
        total += 1
        if row[algo1] == 1 and row[algo2] == 0:
            wins += 1
        elif row[algo1] == 0 and row[algo2] == 1:
            losses += 1

    z: float = (
        (losses - ((losses + wins) * 0.5))
        / ((losses + wins) * (0.5 * (1 - 0.5))) ** 0.5
        if losses + wins > 0
        else 0.0
    )

    b: float = binomtest(wins, n=wins+losses, p=0.5, alternative='greater').pvalue

    cohen: float = wins / (wins + losses) if wins + losses > 0 else 0.0
    cohen = cohen - 0.5

    return norm.cdf(z) if total > 25 else b, cohen

def run():
    """
    Runs the script to generate dataframes for each experiment.
    """

    gahaPath: str = os.path.join(os.path.dirname(os.getcwd()), "genesis_experiments", "gaha")
    gaHybrid100Path: str = os.path.join(os.path.dirname(os.getcwd()), "genesis_experiments", "ga_hybrid_100")
    gaHybrid2000Path: str = os.path.join("bega_2000_hybrid", "v3")
    genesisPath: str = os.path.join(os.path.dirname(os.getcwd()), "genesis_experiments", "genesis")
    begaOnline: str = os.path.join("bega_online", "v3")

    staticConvergencePath: str = os.path.join(
        "analysis", "v3", "convergence_static.csv"
    )
    milanConvergencePath: str = os.path.join(
        "analysis", "v3", "convergence_milan.csv"
    )
    dcConvergencePath: str = os.path.join(
        "analysis", "v3", "convergence_dc.csv"
    )
    randomConvergencePath: str = os.path.join(
        "analysis", "v3", "convergence_25N50E.csv"
    )

    algorithms: list[tuple[str, str]] = [
        ("gaha", gahaPath),
        ("ga_hybrid_100", gaHybrid100Path),
        ("ga_hybrid_2000", gaHybrid2000Path),
        ("genesis", genesisPath),
        ("genesis_online", genesisPath),
        ("bega_online", begaOnline),
    ]
    data = generateExperimentDFs(algorithms)

    comparisonsTrend: list[tuple[str, str, int]] = [
        ("genesis", "ga_hybrid_100", 5),
        ("genesis", "ga_hybrid_2000", 5),
        ("genesis", "gaha", 5),
        ("genesis_online", "bega_online", 5)
    ]

    comparisonStatic: list[tuple[str, str, int]] = [
        ("genesis", "bega_hybrid_100", 5),
        ("genesis", "bega_hybrid_2000", 5),
        ("genesis", "gaha", 5),
        ("genesis", "gda", 5),
        ("genesis", "bega_online", 5),
        ("genesis", "rl", 5)
    ]

    comparisonsMilan: list[tuple[str, str, int]] = [
        ("genesis", "rl", 12),
    ]

    comparisonsDC: list[tuple[str, str, int]] = [
        ("genesis", "rl", 10),
    ]

    comparisonsRandom: list[tuple[str, str, int]] = [
        ("genesis", "rl", 12),
    ]

    for algo1, algo2, exp in comparisonsTrend:
        genesis = generateRanks(algo1, algo2, data[0], data[1])
        lLatency, lAr = generateL(genesis[0], genesis[1])
        zLatency, zAr = generateZ(lLatency, lAr, 500 if algo2 != "bega_online" else 15, exp)
        lP: float = calculatePValue(zLatency)
        arP: float = calculatePValue(zAr)
        signP, signCohen = generateSignTestScore(algo1, algo2, data[1])
        print(f"Comparison: {algo1} vs {algo2}")
        print(f"Z for latency: {zLatency}")
        print(f"Z for acceptance ratio: {zAr}")
        print(f"P-value for latency: {lP}")
        print(f"P-value for acceptance ratio: {arP}")
        print(f"Sign test score: {signP}")
        print(f"Cohen's g: {signCohen}")

    print("\n\nStatic Comparisons:")
    for algo1, algo2, exp in comparisonStatic:
        signP, signCohen = generateSignTestScore2(algo1, algo2, staticConvergencePath)
        print(f"Comparison: {algo1} vs {algo2}")
        print(f"Sign test score: {signP}")
        print(f"Cohen's g: {signCohen}")

    print("\n\nMilan Comparisons:")
    for algo1, algo2, exp in comparisonsMilan:
        signP, signCohen = generateSignTestScore2(algo1, algo2, milanConvergencePath)
        print(f"Comparison: {algo1} vs {algo2}")
        print(f"Sign test score: {signP}")
        print(f"Cohen's g: {signCohen}")

    print("\n\nRandom Comparisons:")
    for algo1, algo2, exp in comparisonsRandom:
        signP, signCohen = generateSignTestScore2(algo1, algo2, randomConvergencePath)
        print(f"Comparison: {algo1} vs {algo2}")
        print(f"Sign test score: {signP}")
        print(f"Cohen's g: {signCohen}")

    print("\n\nDC Comparisons:")
    for algo1, algo2, exp in comparisonsDC:
        signP, signCohen = generateSignTestScore2(algo1, algo2, dcConvergencePath)
        print(f"Comparison: {algo1} vs {algo2}")
        print(f"Sign test score: {signP}")
        print(f"Cohen's g: {signCohen}")

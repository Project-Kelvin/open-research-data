"""
Defines the analysis functions for the emulation data.
"""

import os
from pathlib import Path
from matplotlib import pyplot as plt
import tensorflow as tf
import numpy as np
import polars as pl
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error, r2_score, root_mean_squared_error
import seaborn as sns

# Set global font parameters
import matplotlib.pyplot as plt

plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["Liberation Serif"]  # or 'Nimbus Roman'
sns.set_theme(style="whitegrid", context="paper", font_scale=2, font="Liberation Serif")
plt.rcParams["figure.constrained_layout.use"] = True

def analyse() -> None:
    """
    Analyses the emulation data and generates a summary report.
    """

    dataPath: Path = Path("data")
    data: pl.DataFrame | None = None
    modelPath: str = os.path.join(os.pardir, "v3", "surrogate.keras")
    model: tf.keras.Model = tf.keras.models.load_model(modelPath)
    totalExp: int = 0
    totalRows: int = 0

    # for file in dataPath.iterdir():
    #     with open(file, "r") as f:
    #         fileData = pl.read_csv(f)

    #         # if fileData.columns[0] != "individual":
    #         #     continue

    #         totalExp += 1
    #         fileData = fileData.select(
    #             pl.col("max_link_score"),
    #             pl.col("max_cpu"),
    #             pl.col("total_delay"),
    #             pl.col("latency"),
    #             pl.col("real_reqps"),
    #             pl.col("reqps")
    #         )
    #         fileData = fileData.with_columns(
    #             pl.lit(0).alias("id")
    #         )
    #         fileData = fileData.filter(pl.col("latency") > 0)
    #         fileData = fileData.with_columns(
    #             pl.col("latency") - pl.col("total_delay").alias("latency")
    #         )
    #         #fileData = fileData.filter(abs(pl.col("real_reqps") - pl.col("reqps"))  <= 1)

    #         q1 = fileData.select(pl.col("latency")).quantile(0.25).to_numpy()[0][0]
    #         q3 = fileData.select(pl.col("latency")).quantile(0.75).to_numpy()[0][0]
    #         iqr = q3 - q1
    #         lowerBound = q1 - 1.5 * iqr
    #         upperBound = q3 + 1.5 * iqr
    #         # fileData = fileData.filter((pl.col("latency") >= lowerBound) & (pl.col("latency") <= upperBound))
    #         totalRows += fileData.height

    #         fileData = fileData.with_row_index("idx")
    #         fileData = fileData.with_columns(
    #             (pl.col("idx") // 1).alias("idx")
    #         )
    #         fileData = fileData.group_by("idx").agg(
    #             pl.col("max_link_score").median().alias("max_link_score"),
    #             pl.col("max_cpu").median().alias("max_cpu"),
    #             pl.col("total_delay").median().alias("total_delay"),
    #             pl.col("latency").median().alias("latency"),
    #             pl.col("real_reqps").median().alias("real_reqps"),
    #             pl.col("reqps").median().alias("reqps")
    #         )

    #         inputData: np.ndarray = fileData.select(
    #             pl.col("max_link_score"),
    #             pl.col("max_cpu"),
    #         ).to_numpy()
    #         output: np.ndarray = model.predict(inputData, verbose=0)
    #         fileData = fileData.with_columns(
    #             pl.Series("prediction", output.flatten()))

    #         data = pl.concat([data, fileData], how="vertical") if data is not None else fileData

    # if data is None:
    #     print("No data found.")

    #     return
    # data.write_csv(os.path.join("summary.csv"))

    validationData: pl.DataFrame = pl.read_csv(os.path.join(os.pardir, "v3", "predictions.csv"))
    data = validationData.with_columns(
        pl.col("PredictedLatency").alias("prediction")
    )
    # All Data
    printErrorMetrics(generateErrorMetrics(data.select(pl.col("latency")).to_numpy().flatten(),
                                            data.select(pl.col("prediction")).to_numpy().flatten()), "All Data")

    errorData: pl.DataFrame | None = None

    # Binned
    binSize: int = 40
    startIndex: int = 0
    endIndex: int = startIndex + binSize
    while endIndex <= int(np.ceil(max(data.select(pl.col("latency")).to_numpy().flatten()))):
        slicedData: pl.DataFrame = data.filter((pl.col("latency") >= startIndex) & (pl.col("latency") < endIndex))
        if slicedData.height == 0:
            startIndex = endIndex
            endIndex = startIndex + binSize
            if endIndex > int(np.ceil(max(data.select(pl.col("latency")).to_numpy().flatten()))):
                endIndex = int(np.ceil(max(data.select(pl.col("latency")).to_numpy().flatten())))

            if startIndex >= endIndex:
                break
            continue
        errorMetric: dict[str, float] = generateErrorMetrics(slicedData.select(pl.col("latency")).to_numpy().flatten(),
                                                slicedData.select(pl.col("prediction")).to_numpy().flatten())
        if errorData is None:
            errorData = pl.DataFrame(
                {
                    "bin": [f"{startIndex}-{endIndex}"],
                    "MAE": [errorMetric["MAE"]],
                    "MAPE": [errorMetric["MAPE"]],
                    "R2": [errorMetric["R2"]],
                    "RMSE": [errorMetric["RMSE"]],
                    "count": [slicedData.height]
                }
            )
        else:
            errorData = pl.concat(
                [
                    errorData,
                    pl.DataFrame(
                        {
                            "bin": [f"{startIndex}-{endIndex}"],
                            "MAE": [errorMetric["MAE"]],
                            "MAPE": [errorMetric["MAPE"]],
                            "R2": [errorMetric["R2"]],
                            "RMSE": [errorMetric["RMSE"]],
                            "count": [slicedData.height]
                        }
                    )
                ],
                how="vertical"
            )
        # printErrorMetrics(errorMetric, f"Bin {startIndex}-{endIndex}")
        startIndex = endIndex
        endIndex = startIndex + binSize
        if endIndex > int(np.ceil(max(data.select(pl.col("latency")).to_numpy().flatten()))):
            endIndex = int(np.ceil(max(data.select(pl.col("latency")).to_numpy().flatten())))

        if startIndex >= endIndex:
            break
    if errorData is not None:
        errorData.write_csv(os.path.join("error_metrics.csv"))

    under100: pl.DataFrame = data.filter(pl.col("latency") <= 100)
    over100: pl.DataFrame = data.filter(pl.col("latency") > 100)

    tp: int = round(under100.filter(pl.col("prediction") <= 100).height, 2)
    fn: int = round(under100.filter(pl.col("prediction") > 100).height, 2)
    fp: int = round(over100.filter(pl.col("prediction") <= 100).height, 2)
    tn: int = round(over100.filter(pl.col("prediction") > 100).height, 2)

    recall: float = round(tp / (tp + fn) if (tp + fn) > 0 else 0, 2)
    precision: float = round(tp / (tp + fp) if (tp + fp) > 0 else 0, 2)

    negativeRecall: float = round(tn / (tn + fp) if (tn + fp) > 0 else 0, 2)
    negativePrecision: float = round(tn / (tn + fn) if (tn + fn) > 0 else 0, 2)

    f1: float = round(2 * tp / (2 * tp + fp + fn) if (2 * tp + fp + fn) > 0 else 0, 2)
    f1Negative: float = round(2 * tn / (2 * tn + fp + fn) if (2 * tn + fp + fn) > 0 else 0, 2)

    print(f"True Positives: {tp}\nFalse Negatives: {fn}\nFalse Positives: {fp}\nTrue Negatives: {tn}")
    print(f"Precision: {precision}\nRecall: {recall}\nF1 Score: {f1}")
    print(f"Negative Precision: {negativePrecision}\nNegative Recall: {negativeRecall}\nNegative F1 Score: {f1Negative}")
    print(f"Total Experiments: {totalExp}\nTotal Rows: {totalRows}")

    if errorData is not None:
        plt.figure(figsize=(10, 6))
        sns.barplot(data=errorData.to_pandas(), x="bin", y="MAE", color="#375c63", alpha=1)
        plt.xlabel("Measured Traffic Latency Bin (ms)")
        plt.xticks([errorData.select(pl.col("bin")).to_numpy().flatten()[i] for i in range(0, errorData.height, 10)], rotation=45)
        plt.ylabel("Mean Absolute Error (MAE)")
        plt.gcf().savefig("mae_distribution.png")

    error: np.ndarray = data.select(pl.col("latency")).to_numpy().flatten() - data.select(pl.col("prediction")).to_numpy().flatten()
    plt.figure(figsize=(10, 6))
    sns.histplot(error, binwidth=10, color="#375c63", alpha=1)
    plt.xlabel("Error (Measured Traffic Latency - Predicted Traffic Latency)")
    plt.xticks([i for i in range(-200, 200 + 1, 25)])
    plt.ylabel("Frequency")
    plt.xlim(-200, 200)
    plt.gcf().savefig("error_distribution.png")

    filteredData: pl.DataFrame = data.filter((pl.col("latency") >= 80) & (pl.col("latency") < 120))
    error: np.ndarray = filteredData.select(pl.col("latency")).to_numpy().flatten() - filteredData.select(pl.col("prediction")).to_numpy().flatten()
    plt.figure(figsize=(10, 8))
    sns.histplot(error, binwidth=10)
    plt.xlabel("Error")
    plt.xticks([i for i in range(-200, 200 + 1, 25)], rotation=90)
    plt.ylabel("Count")
    plt.xlim(-200, 200)
    plt.gcf().savefig("threshold_error_distribution.png")

def generateErrorMetrics(true: np.ndarray, predicted: np.ndarray) -> dict:
    """
    Generates error metrics for the given true and predicted values.

    Parameters:
        true (np.ndarray): The true values.
        predicted (np.ndarray): The predicted values.

    Returns:
        dict: A dictionary containing the error metrics.
    """

    mae = mean_absolute_error(true, predicted)
    mape = mean_absolute_percentage_error(true, predicted)
    r2 = r2_score(true, predicted)
    rmse = root_mean_squared_error(true, predicted)

    return {
        "MAE": mae,
        "MAPE": mape,
        "R2": r2,
        "RMSE": rmse
    }

def printErrorMetrics(metrics: dict, label: str) -> None:
    """
    Prints the error metrics.

    Parameters:
        metrics (dict): A dictionary containing the error metrics.
        label (str): A label for the error metrics.
    """

    print(f"Error Metrics for {label}:")
    for metric, value in metrics.items():
        print(f"{metric}: {value}")

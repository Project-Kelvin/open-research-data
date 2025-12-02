## Experiment Specifications

| Attribute        | Value                                                                                                       |
| ---------------- | ----------------------------------------------------------------------------------------------------------- |
| Date             | 21/06/2025                                                                                                  |
| Algorithm        | GA Dijkstra                                                                                                 |
| Device           | University of Glasgow Server                                                                                |
| OS               | Ubuntu 20.04.6 LTS                                                                                          |
| Hypervisor       | QEMU                                                                                                        |
| CPU              | Intel(R) Xeon(R) Gold 6240R CPU @ 2.40GHz                                                                   |
| No. of Cores     | 64                                                                                                          |
| RAM              | 64 GB                                                                                                       |
| App Technology   | Node.js/Express                                                                                             |
| Calibration Data | calibrations_server_12_02_2025                                                                              |
| GA Parameters    | Population Size: 10, Generations: 10, Mutation Rate: 1.0, Crossover Rate: 1.0, Individual Mutation Rate 1.0 |
| Topology         | 4-ary Fat Tree                                                                                              |

## Experiment Configurations

| Name              | No. of SFCRs | Traffic Scale | Traffic Pattern            | CPUs   | Memory | Link Bandwidth | Total Time Taken(s) |
| ----------------- | ------------ | ------------- | -------------------------- | ------ | ------ | -------------- | ------------------- |
| 8_0.1_False_5_2   | 32           | 1 (0.1)       | No Phase Shift (Pattern A) | 2 CPUs | 5 GB   | 5 Mbps         | 62577.926120975986  |
| 8_0.1_False_10_1  | 32           | 1 (0.1)       | No Phase Shift (Pattern A) | 1 CPU  | 5 GB   | 10 Mbps        | 64246.144316849066  |
| 8_0.1_False_10_2  | 32           | 1 (0.1)       | No Phase Shift (Pattern A) | 2 CPUs | 5 GB   | 10 Mbps        | 63788.07564635598   |
| 8_0.1_True_10_2   | 32           | 1 (0.1)       | Phase Shifted (Pattern B)  | 2 CPUs | 5 GB   | 10 Mbps        | 65943.3625021649    |
| 8_0.2_False_10_1  | 32           | 2 (0.2)       | No Phase Shift (Pattern A) | 2 CPUs | 5 GB   | 10 Mbps        | 63756.44584052893   |
| 16_0.1_False_10_2 | 64           | 1 (0.1)       | No Phase Shift (Pattern A) | 2 CPUs | 5 GB   | 10 Mbps        | 66481.62726873416   |

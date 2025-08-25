Energy Anomaly Detection in R

Anomaly-based intrusion detection on electricity consumption data using PCA and Hidden Markov Models (HMMs).

📌 Overview

This project implements an anomaly detection system for electricity consumption data using Principal Component Analysis (PCA) and Hidden Markov Models (HMMs) in R.
The goal is to identify deviations in normal household energy usage that may indicate intrusions, faults, or abnormal behavior in smart grid systems.

This work was developed as part of CMPT 318 – Cyber Security (Spring 2025).

⚡ Motivation

Modern SCADA (Supervisory Control and Data Acquisition) systems managing critical infrastructure (e.g., power grids) are vulnerable to cyberattacks and abnormal behavior.

Signature-based IDS → Detects only known attacks.

Anomaly-based IDS → Detects deviations from normal behavior, effective for zero-day attacks.

This project focuses on anomaly-based intrusion detection to improve resilience against unknown threats.

🔑 Features

Feature Scaling – Standardization applied before PCA for robust variance preservation.

Dimensionality Reduction – PCA used to select the most informative electricity consumption variables.

HMM Training – Multivariate Hidden Markov Models trained on historical electricity usage.

Anomaly Detection – Log-likelihood deviations computed on weekly test data to detect abnormal patterns.

Thresholding – Maximum deviation from training data used to define anomaly thresholds.

🛠️ Tech Stack

Language: R

Libraries:

depmixS4 – Hidden Markov Models

stats – PCA & standardization

ggplot2 – Data visualization

📊 Dataset

The dataset consists of household electricity consumption measurements, including:

Global Active Power

Global Reactive Power

Voltage

Global Intensity

Sub-metering 1, 2, 3

Training data: 2006–2008
Testing data: 2009

🚀 Results

Optimal HMM model found with 7 states, trained on PCA-selected features.

Maximum log-likelihood deviation observed: 5004.589 (Week 5) → anomaly threshold.

Weeks exceeding this threshold flagged as potential anomalies.



👥 Authors

Kwong Hoi Chan

Duc Ta

Duc Anh Nguyen

Andrew Yu

🏆 Lessons Learned

Importance of feature scaling and PCA interpretation.

Challenges in setting a robust anomaly threshold.

Variability in results due to machine/environment differences.

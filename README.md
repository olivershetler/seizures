# README

This repository contains a project that compares and contrasts the algorithmic / ML approach to data analysis
with the classical statistical modeling approach.

It contains
- an [article](./shetler-seizures-article.pdf) that gives a detailed report on the topic;
- the R code required to reproduce the classical statistical modeling results;
- a Jupyter notebook for reproducing the ML results;
- the original data for reproducing the analysis in full in the file `./data/seizures_original.csv`;
- three transformed data sets so that you can pick up at the modeling stage without having to re-run feature extraction;
    - the features extracted for the Python Jupyter notebook in the file `./data/seizures_features.csv`;
    - the features extracted for the R code in the file `./data/fdata.csv`;
    - autocovariance data produced as an intermediate feature extraction step (for the R file) in the file `./data/AC.csv`.
- finally, the repository contains the python package requirements in `requirements.txt` reproducibility.
# Loan default prediction
## Introduction
In this repository, you find the code for a loan default prediction. The format of the repository is based on the traditional machine learning pipeline.


## Flow of the repository
### data
In the data folder, you can find the bronze, silver, gold and result folder. In the bronze folder consists of the raw data. The silver data has no longer missing values and the gold data is the data after feature engineering. The result folder consist of all our predictions of the models. These are csv files with 2 columns: the first column is the id and the second one is the prediction.

### src
In the src folders are all the R files. In the data cleaning file, the missing variables are handled. In the feature engineering file, feature enigneering happend. Hereafter there are all the files of the different models. The models are : a logistic model, a svm, a gam, a random forest, a general boosting model and an extreme gradient boosting model.

### Contributors Team 02
Kasper Halewyck     /  01707513     /   kasper.halewyck@ugent.be
Stijn Heuninck    /  01807847    /    stijn.heuninck@ugent.be
Jesse Lema   /   01707412     /   jesse.lema@ugent.be
Pieter-Jan Verhelst      /   01807049    /     piverhel.verhelst@ugent.be


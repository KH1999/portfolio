# Loan default prediction
## Introduction
In this repository, you find the code for a loan default prediction. The format of the repository is based on the traditional machine learning pipeline.


## Flow of the repository
### data
In the data folder, you can find the bronze, silver, gold and result folder. The bronze folder consists of the raw data. The silver data no longer contains missing values and the gold data is the data after feature engineering. The result folder contains all our predictions of the models. These are csv files with 2 columns: the first column is the id and the second one is the prediction.

### src
In the src folders are all the R files. In the data cleaning file, the missing variables are handled. In the feature engineering file, feature enigneering happend. Hereafter there are all the files of the different models. The models are : - logistic model - svm - gam - random forest - general boosting model - extreme gradient boosting model.




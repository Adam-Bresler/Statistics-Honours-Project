# Readme

Taking the data from a raw format given by Lin and Juko to reasonable predictions requires much manipulation and model fitting.

The R code used is split into 10 files.

1. Data Cleaning
- Reading in the data, changing column names.
2. YTD Creation
- Creating the historical averages.
3. Merging Matches
- Taking matches from individual players and merging them such that each match is a row with both players' past statistics.
4. Head-to-Head
- Creating the head to head record.
5. Creating Features
- Taking all the data and creating the features we designed.
6. Dividing the Datasets
- Splitting into raw and engineered and by historical averaging type.
7. GLM Final
- Fitting and getting predictions from logistic regression.
8. Classification Tree Final
- Fitting and getting predictions from classification tree.
9. Random Forest Final
- Fitting and getting predictions from random forest.
10. Gradient Boosting Final
- Fitting and getting predictions from gradient boosting.

Files 7-10 are the final models and use the data created in files 1-6. They can be run using the csv files created by 6. Dividing the Datasets. These are:

raw_rolled \
raw_rolled_bc \
raw_weighted \
raw_weighted_bc   
engineered_rolled \
engineered_rolled_bc \
engineered_weighted \
engineered_weighted_bc  

# Time Series Project

Team members: Si Chen, Ernest Kim, Sooraj Subrahmannian, Xiaowen(Sarah) Zhang

## Goal

Predict Canadian monthly bankruptcy rate from January 2011 to December 2012.

## Biref Intro

Accurately forecasting national bankruptcy rates is of interest to national banks, insurance companies, credit-lenders, politicians etc. The goal of this project will be to precisely and accurately forecast monthly bankruptcy rates for Canada.

Our approach includes an initial exploratory data analysis to get familiar with the relationship between variables to inform our model building. Next, we fit models using four different modeling approaches, both univariate and multivariate. For each approach, we tuned the appropriate parameters with the goal of maximizing prediction accuracy--in this case we measured that by minimizing root mean squared error (RMSE). RMSE measures differences between the bankruptcy rate the model predicted for the validation set to the actual bankruptcy rate given by the observed data. The validation set is a subset of data from January 2009 to December 2010 that is used to test model predictions, since this data includes the actual bankruptcy rate.

After producing optimal models for each approach, we evaluated each model’s predictions and RMSE. Our final model took a combination of the best two approaches to give our final predictions for Canada bankruptcy rates between 2011-2012.

## Description of Data
Data available for modeling includes the monthly data from January 1987 to December 2010 for Canada for the following four variables:  Unemployment Rate, Population, Housing Price Index and Bankruptcy Rate.

## Deliverable 

The [report](https://github.com/Sarah-Zhang/Bankruptcy_Rate_Forecasting/blob/master/Report.pdf) summarized the feature selection and modeling process and our conclusions.

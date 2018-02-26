# Capstone Project - Real Estate
Data: Boston Housing Dataset (HousingData.csv) 

Programming language(s): R 

Tool(s): RStudio  

Business problem: To understand the drivers behind the value of houses in Boston and provide data-driven recommendation to the client on how they can increase the value of housing.The Boston housing dataset consisted of 506 observations and 14 variables.  

Project challenge(s): MEDV (Median value of homes in Boston) was identified as the dependent variable. While the rest, were the independent variables. The goal was to find out which among the independent variables were statistically significant in driving the house prices (MEDV). The dataset consisted of missing values and outliers. Some of the variables had a skewed distribution. There was multicollinearity among few independent variables.  

Our Approach: Prior to model building, we tidied up our dataset by eliminating the rows that contained missing values. Replacing the missing values with median and mean of those variables were also done. Considering the three approaches, median imputation(replacing missing values with mean) was found to be the best approach. As the dependent variable "MEDV" (median value of houses) was continuous(numerical) in nature, we implemented the Multiple linear regression to build our model. Additional models were built from Decision trees and Random forest. On further investigation, we discovered that the dependent variable had a skewed distribution. By log transformation of this variable, we were able to get a normal distribution. Post transformation, we found out that the model built from Multiple linear regression with log transformed MEDV was the best in terms of MSE (Mean squared error) value and Adjusted R^2. All the assumptions of linear regression were met.

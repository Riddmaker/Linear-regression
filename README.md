# Loan regression
A university project we did in loan linear regression, winning the price of best model within our class.
(Had to ZIP the data, just append the second one to the first csv.)
Excuse the messy code.
- Massive amounts of data cleaning
- Some feature engineering
- Correlation analysis among independent variables
- Comparing model performance with different metrics such as:
	- RSQ (R-squared - measures how much of the dependent variable variance is explained by the independent variables.)
	- RSS (Residual sum of squares - Sum of squared residual errors.)
	- Adj_R^2 (Similar to R-squared, put punishes usage of independent variables that not add significantly to the models 	performance.)
	- CP (Measures model complexity - a CP value close to the number of independent variables is a well fit.)
	- BIC (Bayesian Information Criterion - Similar to AIC (Akaike Information Criterion), but uses a different formula to 	calculate the penalty for the number of indepndent variables.)
- Doing stepwise backward selection.
- Doing stepwise forward selection and afterwards cross validation to find the model with the lowest cross-validation error.
- Deploying final model.


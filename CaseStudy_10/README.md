Southern Methodist University
MSDS 7333
Quantifying the World
Course Designer: Associate Professor Monnie McGee; Department of Statistical Science; Southern Methodist University, Dallas, Texas
Course Instructors: Alan Elliott, Daniel Engels, Eric Larson, Monnie McGee and Melvin Greer
Required Texts:
McKinley, Wes (2014, 2017). Python for Data Analysis. O’Reilly
Publishers, available in PDF download (McKinley).



Assignment Case Study 10: Imputation
Missing Data Assignment

Step 1:
Using Sklearn get the Boston Housing dataset. https://www.kaggle.com/c/boston-housing
Fit a linear regressor to the data as a baeline.  There is no need to do Cross-Validation.  We are exploring the change in results

What is the loss and what are the goodness of fit parameters?  This will be our baseline for comparison

Step 2: (repeated)
For select between 1, 5 10, 20, 33, and 50% of your data on a single column (Completely at random), replace the present value with
a NAN and then perform an imputation of that value.   

In. each case perform a fit with the imputed data and compare the loss and goodness of fit to your baseline.

Step 3: Take 2 different columns and create data “Missing at Random” when controlled for a third variable (i.e if
Variable Z is > 30, than Variables X, Y are randomly missing).  Make runs with 10%, 20% and 30% missing data imputed
via your best guess.  Repeat your fit and comparisons to the baseline.

Step 4:  Create a Missing Not at Random pattern in which 25% of the data is missing for a single column.    
Impute your data, fit the results and compare to a baseline.

Step 5 (Extra Credit) (10 points):  Using the MCMC method, and your data from step 4, What is the difference 
in performance between imputation via ‘guess’ (mean/median, etc) and MCMC. 

Dataset:
Variables:
Housing Values in Suburbs of Boston
The medv variable is the target variable.

Data description
The Boston data frame has 506 rows and 14 columns.

This data frame contains the following columns:

crim - per capita crime rate by town.
zn - proportion of residential land zoned for lots over 25,000 sq.ft.
indus - proportion of non-retail business acres per town.
chas - Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
nox - nitrogen oxides concentration (parts per 10 million).
rm - average number of rooms per dwelling.
age - proportion of owner-occupied units built prior to 1940.
dis - weighted mean of distances to five Boston employment centres.
rad - index of accessibility to radial highways.
tax - full-value property-tax rate per $10,000.
ptratio - pupil-teacher ratio by town.
black - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
lstat - lower status of the population (percent).
medv - median value of owner-occupied homes in $1000s.

Source
Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the demand for clean air. J. Environ. Economics and Management 5, 81–102.
Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity. New York: Wiley.


Abstract:
Many real-world datasets may contain missing values due to various reasons. Missing data is one of the most common problem we deal with during exploratory analysis and data cleaning. In this case study, we tried to create three patterns of missingness, namely Missing completely at random (MCAR), Missing at random (MAR)and Missing not at random (MNAR). We used Boston housing dataset which contains information about different houses in Boston. There are 506 samples and 13 feature variables in this dataset. We created baseline multiple regression model to predict the value of prices of the house using the given features. Next, we created the many new datasets from the original to simulate MCAR, MAR and MNAR patterns. We used different imputation techniques like Linear Regression, imputation using Mean values, imputation using constant values. Finally, we compared the loss and goodness of fit of the imputation implementation models to our baseline model.



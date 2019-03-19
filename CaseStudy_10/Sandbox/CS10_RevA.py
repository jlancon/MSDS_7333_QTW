# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import os
import numpy as np
import pandas as pd
from numpy.random import randn
pd.options.display.max_rows = 12
np.set_printoptions(precision=4, suppress=True)
import matplotlib.pyplot as plt
from sklearn import datasets



# https://towardsdatascience.com/simple-and-multiple-linear-regression-in-python-c928425168f9
bostonDS = datasets.load_boston()
print(bostonDS.data.shape)
type(bostonDS)
print (bostonDS.DESCR)
bostonDS.feature_names
bostonDS.target

# define the data/predictors as the pre-set feature names  
boston = pd.DataFrame(bostonDS.data, columns=bostonDS.feature_names)

# Put the target (housing value -- MEDV) in another DataFrame
target = pd.DataFrame(bostonDS.target, columns=["MEDV"])


print('Dataframe Size:',boston.shape)
print('Dataframe Null values:',boston.isnull().values.ravel().sum())
print ('Number of unique classes:\n',boston.nunique())

# Base Line - Full Dataset
from sklearn.linear_model import LinearRegression
reg = LinearRegression().fit(boston, target)
reg.score(boston, target)
reg.coef_

# https://becominghuman.ai/stats-models-vs-sklearn-for-linear-regression-f19df95ad99b
import statsmodels.api as sm
X = sm.add_constant(boston)
regOLS = sm.OLS(target,X).fit()
regOLS.summary()
regOLS._results.rsquared

##########################################
###### Step 2 
# Select between 1,5,10,20,33, and 50% of dataset on a single column completely
# random.
# Replace the present value with NaN
# Perform an imputation of that value
# In each case, perform a fit with the imputed data and compare the loss
# and goodness of fit to your baseline.

np.random.seed(42)
percent = 0.33
# Set percentage of values randomly to NA
indexer = np.sort(np.random.permutation(len(boston))[int(len(boston)*percent):])
boston_imp = boston.copy()
# Chose LSTAT varialbe to randomly set to NaN. Chose LSTAT becuase of the 
# large t-statistic in the original analysis
boston_imp.LSTAT[indexer] = np.nan

# https://scikit-learn.org/stable/modules/generated/sklearn.impute.SimpleImputer.html#sklearn.impute.SimpleImputer
from sklearn.impute import SimpleImputer
imp_mean = SimpleImputer(missing_values=np.nan, strategy='mean')
imp_mean.fit(boston_imp)

imp_mean_33 = pd.DataFrame(imp_mean.transform(boston_imp),columns=boston.columns, index=boston.index)

X = sm.add_constant(imp_mean_33)
regOLS_imp = sm.OLS(target,X).fit()
regOLS_imp.summary()
regOLS_imp._results.rsquared
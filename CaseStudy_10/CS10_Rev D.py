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
np.set_printoptions(precision=5, suppress=True)
import matplotlib.pyplot as plt
from sklearn import datasets

##############################################
#--------   Problem 1   ----------------------
# Using sklearn, get Boston Housing Dataset
# Fit Linear regressor to the data as a baseline. (No need to cross validate)
# Determine Loss and Goodness-of-Fit of model 

# https://towardsdatascience.com/simple-and-multiple-linear-regression-in-python-c928425168f9
# https://scikit-learn.org/stable/modules/linear_model.html
# https://scikit-learn.org/stable/modules/generated/sklearn.datasets.load_boston.html
# https://scikit-learn.org/stable/auto_examples/plot_missing_values.html#sphx-glr-auto-examples-plot-missing-values-py
# https://bigdata-madesimple.com/how-to-run-linear-regression-in-python-scikit-learn/
# https://heartbeat.fritz.ai/5-regression-loss-functions-all-machine-learners-should-know-4fb140e9d4b0
bostonDS = datasets.load_boston()
print(bostonDS.data.shape)
type(bostonDS)
print (bostonDS.DESCR) # Prints characteristics of dataset
bostonDS.feature_names
bostonDS.target

# define the data/predictors as the pre-set feature names  
boston = pd.DataFrame(bostonDS.data, columns=bostonDS.feature_names)

# Put the target (housing value -- MEDV) in another DataFrame
target = pd.DataFrame(bostonDS.target, columns=["MEDV"])

# Exploring predictors dataset
print('Dataframe Size:',boston.shape) # (506, 13)
print('Dataframe Null values:',boston.isnull().values.ravel().sum()) # No null values
print ('Number of unique classes:\n',boston.nunique())

# Base Line - Full Dataset
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
reg = LinearRegression().fit(boston, target) #Fitting linearRegression line to dataset
baselineR2 = reg.score(boston, target) #
print('Baseline R2:',baselineR2) # 0.7406426641094094
reg.coef_ # Regression coefficients:    array([[ -0.10801,   0.04642,   0.02056,   2.68673, -17.76661,   3.80987,
                                          #0.00069,  -1.47557,   0.30605,  -0.01233,  -0.95275,   0.00931,
                                          #-0.52476]])

# Predicting MEDV using LinearRession model
target_pred = reg.predict(boston)

#Calculating Mean_Squared_error 
    #Note: This MSE does not include an intercept value in the model
mse = mean_squared_error(target, target_pred) 
print('Baseline Mean Square Error(MSE) with intercept:',mse) #21.894831181729206

# https://becominghuman.ai/stats-models-vs-sklearn-for-linear-regression-f19df95ad99b

### Secondary way of calculating LinearModel ###
### Used as a sanity check
import statsmodels.api as sm
#X = boston
X = sm.add_constant(boston) #This model does include an intercept value
regOLS = sm.OLS(target,X).fit()
regOLS.summary()
regOLS._results.rsquared # 0.7406426641094095
regOLS._results.mse_total # 84.58672359409856; Not sure where getting this figure

sm_pred = regOLS.predict(X) # Predicting MEDV using LinearRession model
smMSE = mean_squared_error(target, sm_pred)
print('Mean Square Error with intercept:',smMSE) #24.166099330126492


##############################################
#--------   Problem 2   ----------------------
# Select between 1,5,10,20,33, and 50% of dataset on a single column completely
# random.
# Replace the present value with NaN
# Perform an imputation of that value
# In each case, perform a fit with the imputed data and compare the loss
# and goodness of fit to your baseline.


######################## Creation of Prob 2 Function #############
########################      get_results()        #############
def get_results(dataset,target,percent,imp_var,strategy):
    # Set percentage of values randomly to NA
    if percent != 0:
        np.random.seed(42)
        indexer = np.sort(np.random.permutation(len(dataset))[len(dataset)-(int(len(dataset)*percent)):])
        dataset_imp = dataset.copy()
        # Chose LSTAT varialbe to randomly set to NaN. Chose LSTAT becuase of the 
        # large t-statistic in the original analysis
        dataset_imp[imp_var][indexer] = np.nan
        
        # https://scikit-learn.org/stable/modules/generated/sklearn.impute.SimpleImputer.html#sklearn.impute.SimpleImputer
        from sklearn.impute import SimpleImputer
        imp_model = SimpleImputer(missing_values=np.nan, strategy=strategy)
        imp_model.fit(dataset_imp)
        
        imp_model = pd.DataFrame(imp_model.transform(dataset_imp),columns=dataset.columns, index=dataset.index)
    else:
        imp_model = dataset.copy()
    
    X = sm.add_constant(imp_model)
    regOLS_imp = sm.OLS(target,X).fit()
    #regOLS_imp.summary()
    #regOLS_imp._results.rsquared
    target_pred = reg.predict(imp_model)
    mse = mean_squared_error(target, target_pred) # Not sure about MSE values
    return((percent,regOLS_imp._results.rsquared,regOLS_imp._results.rsquared_adj,
            regOLS_imp._results.bic,mse,imp_model))

#----- End of get_results() -------------


###  Input variables for Problem #2 function  ##########
    
results_boston = pd.DataFrame([])
per_num = [0.0,0.01,0.05,0.10,0.20,0.33,0.50]
imp_var = ['LSTAT'] #Choose from CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,B,LSTAT
strategy = 'mean' #Choose from 'mean', 'median', 'constant'
#fill_value = 0 # If you choose 'constant', need to select value to fill NaNs


for i in range(len(per_num)):
    results_boston[i] = np.array(get_results(boston,target,per_num[i],imp_var[0],strategy))

results_boston = results_boston.T
results_boston.columns=['Imputed_Percent','RSquared','AdjRSquared','BIC','MSE','Model']

print(results_boston.loc[:,results_boston.columns !='Model']) # Prints results of analysis for each percent of Imputed variables

# Plot of R2 values analysis results vs different Percentage of Imputation
results_boston.plot('Imputed_Percent','RSquared')
plt.ylabel('RSquared Value')
plt.title('RSquared Value vs Imputed Percentage')

# Plot of MSE values analysis results vs different Percentage of Imputation
results_boston.plot('Imputed_Percent','MSE')
plt.ylabel('Mean Square Error')
plt.title('Mean Square Error vs Imputed Percentage')



# Grid Distribution/Histogram of Imputed variable vs different Percentage of Imputation
import seaborn as sns
f, axes = plt.subplots(4, 2, figsize=(7, 7), sharex=True)
sns.distplot( results_boston.Model[0]['LSTAT'] , color="skyblue",ax=axes[0, 0])
sns.distplot( results_boston.Model[1]['LSTAT'] , color="olive", ax=axes[0, 1])
sns.distplot( results_boston.Model[2]['LSTAT'] , color="gold", ax=axes[1, 0])
sns.distplot( results_boston.Model[3]['LSTAT'] , color="teal", ax=axes[1, 1])
sns.distplot( results_boston.Model[4]['LSTAT'] , color="blue", ax=axes[2, 0])
sns.distplot( results_boston.Model[5]['LSTAT'] , color="red", ax=axes[2, 1])
sns.distplot( results_boston.Model[6]['LSTAT'] , color="green", ax=axes[3, 0])


imputed_dataframe = pd.DataFrame([])
plot_result = pd.DataFrame([])
for k in range(0,len(results_boston)):
    imputed_dataframe = results_boston.Model[k]
    imputed_dataframe['Imputed_Percent'] = per_num[k]
    plot_result = plot_result.append(imputed_dataframe, ignore_index=True)

#  Boxplot of Imputed Variable vs different Percentage of Imputation
sns.set(style="whitegrid")
ax = sns.boxplot(x="Imputed_Percent", y="LSTAT", data=plot_result)
ax.set_title('Box Plot - LSTAT Variable vs Percent Imputed')


summaryResult = pd.DataFrame([])
for k in range(0,len(results_boston)):
    tempResults = results_boston.Model[k]['LSTAT'].describe()
    tempResults['Imputed_Percent']=per_num[k]
    summaryResult = summaryResult.append(tempResults, ignore_index=True)

print(summaryResult)



##############################################
#--------   Problem 3   ---------------------- 

# Take 2 different columns and create data 'Missing at Random' when controlled
# for a third variable. (i.e. if Variable Z is >30, then Variable X, Y are randomly
# missing).  Make runs with 10%,20%, and 30% missing Data imputed via your best 
# guess.  Repeat your fit and comparisons to the baseline.

# The code below is for reference only.  Was used to figure out how to
# create function get_resultsP3().  Can follow through step-by-step
# to gain understanding of what function does.

Prob3 = boston.copy()

Prob3Desc = Prob3.describe()
Prob3Desc
Prob3Desc.loc['25%'] # Identifying 1st Quartile
Prob3Desc.loc['25%']['B'] 
SubProb3 = Prob3[Prob3.B > 375.377]
#SubProb3Count = SubProb3.B.count()
#SubProb3['tempIndex'] = pd.Series(range(0,len(SubProb3)),index=SubProb3.index)
#SubProb3.iloc[18]

Prob3SubIndex = Prob3[Prob3.B > 375.377].index
lenIndex = len(Prob3[Prob3.B > 375.377])
SubProb3 = Prob3[Prob3.B > 375.377] # Note Index #18 is the first one missing
Prob3SubIndex[18]
np.random.seed(42)
percent = 0.30
numbVariables = 2
impvar = ['LSTAT','PTRATIO'] 
Prob3['LSTAT'][4]
Prob3[impvar[0]][4]
  
indexer1 = np.sort(np.random.permutation(Prob3SubIndex)[lenIndex-(int(lenIndex*percent)):])
Prob3[impvar[0]][indexer1] = np.nan

np.random.seed(39)
indexer2 = np.sort(np.random.permutation(Prob3SubIndex)[lenIndex-(int(lenIndex*percent)):])
Prob3[impvar[1]][indexer2] = np.nan

# https://scikit-learn.org/stable/modules/generated/sklearn.impute.SimpleImputer.html#sklearn.impute.SimpleImputer
from sklearn.impute import SimpleImputer
imp_meanP3 = SimpleImputer(missing_values=np.nan, strategy='constant',fill_value=0)
imp_meanP3.fit(Prob3)
    
imp_meanP3 = pd.DataFrame(imp_meanP3.transform(Prob3),columns=Prob3.columns, index=Prob3.index)
    
X = sm.add_constant(imp_meanP3)
regOLS_impP3 = sm.OLS(target,X).fit()
regOLS_impP3.summary()
regOLS_impP3._results.rsquared






######################## Creation of Prob 3 Function #############
########################      get_resultsP3()        #############

def get_resultsP3(dataset,target,percent,ran_seed,cond_var,cond_var_val,imp_var,strategy,fill_value=None):
    datasetDesc = dataset.describe()
    
    if percent != 0:
        Prob3SubIndex = dataset[dataset[cond_var] > datasetDesc.loc[cond_var_val][cond_var]].index
        lenIndex = len(dataset[dataset[cond_var] > datasetDesc.loc[cond_var_val][cond_var]])
        from sklearn.impute import SimpleImputer
        for i in range(len(imp_var)):   
            np.random.seed(ran_seed[i])
            indexer = np.sort(np.random.permutation(Prob3SubIndex)[lenIndex-(int(lenIndex*percent)):])
            Prob3[imp_var[i]][indexer] = np.nan
            
            imp_modelP3 = SimpleImputer(missing_values=np.nan, strategy=strategy,fill_value=fill_value)
            imp_modelP3.fit(dataset)
                
            imp_modelP3 = pd.DataFrame(imp_modelP3.transform(dataset),columns=dataset.columns, index=dataset.index)
    else:
        imp_modelP3 = dataset.copy()

    X = sm.add_constant(imp_modelP3)
    regOLS_impP3 = sm.OLS(target,X).fit()
    regOLS_impP3.summary()
    regOLS_impP3._results.rsquared
    target_pred = reg.predict(imp_modelP3)
    mse = mean_squared_error(target, target_pred) # Not sure about MSE values
    return((percent,regOLS_impP3._results.rsquared,regOLS_impP3._results.rsquared_adj,
            regOLS_impP3._results.bic,mse))

#--- End of get_resultsP3 ----------------


###  Input variables for Problem #3 function  ##########

per_numP3 = [0.0,0.10,0.20, 0.30]  # Percentage of imputed values in dataset
ran_seed = [42,39] #Random seed numbers for permutation
cond_var = 'B'   #Choose from CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,B,LSTAT
cond_var_val ='25%'   # Choose from 25%, 50%, 75%, mean,min
imp_var = ['LSTAT','PTRATIO'] #Choose from CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,B,LSTAT
strategy = 'constant' #Choose from 'mean', 'median', 'constant'
fill_value = 0 # If you choose 'constant', need to select value to fill NaNs


###  Execution of Problem #3 function #####

Prob3 = boston.copy() # Making copy of original dataset
#Prob3.describe()
results_bostonP3 = pd.DataFrame([]) # Initalizing results dataframe
for j in range(len(per_numP3)):
    results_bostonP3[j] = np.array(get_resultsP3(Prob3,target,per_numP3[j],ran_seed,cond_var,cond_var_val,imp_var,strategy,fill_value))

results_bostonP3 = results_bostonP3.T
results_bostonP3.columns=['PercentImputed','RSquared','AdjRSquared','BIC','MSE']

print(results_bostonP3) # Prints results of analysis for each percent of Imputed variables

results_bostonP3.RSquared.plot()

results_bostonP3.MSE.plot()



#---------  End of Problem #3 -------------------------------------------




##############################################
#--------   Problem 4   ---------------------- 

# Create a Missing Not at Random pattern in which 25% of the data is missing
# for a single column.  Impute your data, fit the results and compare to a baseline.


######################## Creation of Prob 4 Function #############
########################      get_resultsP4()        #############
def get_resultsP4(dataset,target,percent,imp_var,strategy,fill_value=None):
    # Set 25% percent of values to NA, selecting every 4th entry
    dataset_imp = dataset.copy()
    indexer = dataset_imp.iloc[::4, :].index
    dataset_imp[imp_var][indexer] = np.nan
    
    from sklearn.impute import SimpleImputer
    imp_model = SimpleImputer(missing_values=np.nan, strategy=strategy,fill_value=fill_value)
    imp_model.fit(dataset_imp)
    
    imp_model = pd.DataFrame(imp_model.transform(dataset_imp),columns=dataset.columns, index=dataset.index)
    
    X = sm.add_constant(imp_model)
    regOLS_imp = sm.OLS(target,X).fit()
    #regOLS_imp.summary()
    #regOLS_imp._results.rsquared
    target_pred = reg.predict(imp_model)
    mse = mean_squared_error(target, target_pred) # Not sure about MSE values
    return((percent,regOLS_imp._results.rsquared,regOLS_imp._results.rsquared_adj,
            regOLS_imp._results.bic,mse))

#--------- End of get_resultsP4() -----------------------------------------


###  Input variables for Problem #4 function  ##########

imp_var = ['LSTAT'] #Choose from CRIM,ZN,INDUS,CHAS,NOX,RM,AGE,DIS,RAD,TAX,PTRATIO,B,LSTAT
strategy = 'mean' #Choose from 'mean', 'median', 'constant'
fill_value = 0 # If you choose 'constant', need to select value to fill NaNs
per_numP4 = [0.25]  # Percentage of imputed values in dataset; set at 25% for this exercise

###  Execution of Problem #4 function #####

Prob4 = boston.copy() # Making copy of original dataset
#Prob4.describe()
results_bostonP4 = pd.DataFrame([]) # Initalizing results dataframe
for j in range(len(imp_var)):
    results_bostonP4[j] = np.array(get_resultsP4(Prob4,target,per_numP4[j],imp_var[j],strategy,fill_value))

results_bostonP4 = results_bostonP4.T
results_bostonP4.columns=['PercentImputed','RSquared','AdjRSquared','BIC','MSE']

print(results_bostonP4) # Prints results of analysis for each percent of Imputed variables


#---------  End of Problem #4 -------------------------------------------





############################################################################
############################################################################

##  Below is scrap code.  Just left here for troubleshooting purposes
##  Will be deleted prior to final submission

dataset_imp = Prob4.copy()
indexer = dataset_imp.iloc[::4, :].index
dataset_imp.LSTAT[indexer] = np.nan
dataset_imp[imp_var[0]][indexer] = np.nan

from sklearn.impute import SimpleImputer
imp_model = SimpleImputer(missing_values=np.nan, strategy=strategy,fill_value=fill_value)
imp_model.fit(dataset_imp)

imp_model = pd.DataFrame(imp_model.transform(dataset_imp),columns=dataset.columns, index=dataset.index)
    
X = sm.add_constant(imp_model)
regOLS_imp = sm.OLS(target,X).fit()
    #regOLS_imp.summary()
    #regOLS_imp._results.rsquared
target_pred = reg.predict(imp_model)
mse = mean_squared_error(target, target_pred) # Not sure about MSE values





# Set percentage of values randomly to NA
indexer = np.sort(np.random.permutation(len(boston))[len(boston)-(int(len(boston)*percent)):])
dataset_imp = boston.copy()
# Chose LSTAT varialbe to randomly set to NaN. Chose LSTAT becuase of the 
# large t-statistic in the original analysis
dataset_imp.LSTAT[indexer] = np.nan
    
# https://scikit-learn.org/stable/modules/generated/sklearn.impute.SimpleImputer.html#sklearn.impute.SimpleImputer
from sklearn.impute import SimpleImputer
imp_mean = SimpleImputer(missing_values=np.nan, strategy='mean')
imp_mean.fit(dataset_imp)
    
imp_mean = pd.DataFrame(imp_mean.transform(dataset_imp),columns=boston.columns, index=boston.index)
    
X = sm.add_constant(imp_mean)
regOLS_imp = sm.OLS(target,X).fit()
regOLS_imp.summary()
regOLS_imp._results.rsquared

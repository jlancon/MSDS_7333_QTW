


# Data Set Information:
# https://archive.ics.uci.edu/ml/datasets/HIGGS

# The data has been produced using Monte Carlo simulations. The first 21
# features (columns 2-22) are kinematic properties measured by the particle
# detectors in the accelerator. The last seven features are functions of the
# first 21 features; these are high-level features derived by physicists to
# help discriminate between the two classes. There is an interest in using deep
# learning methods to obviate the need for physicists to manually develop such
# features. Benchmark results using Bayesian Decision Trees from a standard
# physics package and 5-layer neural networks are presented in the original
# paper. The last 500,000 examples are used as a test set.


import numpy as np
import pandas as pd
N=10500000. #Change this line adjust the number of rows. 
data=pd.read_csv("C:/Users/Prodigy/Documents/Personal Info/SMU/MSDS 7333 - Quantifying the World/Unit_12/HIGGS.csv",nrows=N,header=None)
test_data=pd.read_csv("C:/Users/Prodigy/Documents/Personal Info/SMU/MSDS 7333 - Quantifying the World/Unit_12/HIGGS.csv",nrows=500000,header=None,skiprows=10500000)

#The first column is the class label (1 for signal, 0 for background), followed 
#by the 28 features (21 low-level features then 7 high-level features): 
data.columns = ['lepton pT','lepton eta','lepton phi','missing energy magnitude',
                'missing energy phi','jet 1 pt','jet 1 eta','jet 1 phi',
                'jet 1 b-tag','jet 2 pt','jet 2 eta','jet 2 phi','jet 2 b-tag',
                'jet 3 pt','jet 3 eta','jet 3 phi','jet 3 b-tag','jet 4 pt',
                'jet 4 eta','jet 4 phi','jet 4 b-tag','m_jj','m_jjj','m_lv',
                'm_jlv','m_bb','m_wbb','m_wwbb']
test_data.columns = ['lepton pT','lepton eta','lepton phi','missing energy magnitude',
                'missing energy phi','jet 1 pt','jet 1 eta','jet 1 phi',
                'jet 1 b-tag','jet 2 pt','jet 2 eta','jet 2 phi','jet 2 b-tag',
                'jet 3 pt','jet 3 eta','jet 3 phi','jet 3 b-tag','jet 4 pt',
                'jet 4 eta','jet 4 phi','jet 4 b-tag','m_jj','m_jjj','m_lv',
                'm_jlv','m_bb','m_wbb','m_wwbb']


from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from keras.optimizers import SGD
from sklearn.metrics import roc_auc_score

y = np.array(data.loc[:,0])
x = np.array(data.loc[:,1:])
x_test = np.array(test_data.loc[:,1:])
y_test = np.array(test_data.loc[:,0])

#Begin 

model = Sequential()
model.add(Dense(50, input_dim=x.shape[1], kernel_initializer='uniform')) # X_train.shape[1] == 28 here
model.add(Activation('sigmoid'))
model.add(Dropout(0.10))
model.add(Dense(50, kernel_initializer='uniform'))
model.add(Activation('sigmoid'))
model.add(Dropout(0.10))
model.add(Dense(1, kernel_initializer='uniform')) 
model.add(Activation('sigmoid'))

sgd = SGD(lr=0.1, decay=1e-6, momentum=0.9, nesterov=True)
model.compile(loss='binary_crossentropy', metrics=['accuracy'], optimizer=sgd)

model.fit(x, y, epochs=5, batch_size=1000)
roc_auc_score(y_test,model.predict(x_test))
#end


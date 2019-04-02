# -*- coding: utf-8 -*-
"""
Created on Sat Mar 30 14:50:17 2019

@author: Prodigy
"""
import os
import tensorflow as tf
import pandas as pd
import numpy as np
import seaborn as sns; sns.set()
import matplotlib.pyplot as plt

N=2000000 #Chosen as a decent compromise between sample size and limitations of hardware
input_data=pd.read_csv("C:/Users/Prodigy/Documents/Personal Info/SMU/MSDS 7333 - Quantifying the World/Unit_12/HIGGS.csv",nrows=10500000,header=None)

cc = input_data.iloc[:,1:]

# Creating a random sample of observations from the 10.5M observations
np.random.seed(42)
data = input_data.sample(n=N)

# Selecting the 500K test data set (last 500K rows in the data set)
test_data=pd.read_csv("C:/Users/Prodigy/Documents/Personal Info/SMU/MSDS 7333 - Quantifying the World/Unit_12//HIGGS.csv",nrows=500000,header=None,skiprows=10500000)

##############################
##----  Correlation Matrix
#The first column is the class label (1 for signal, 0 for background), followed 
#by the 28 features (21 low-level features then 7 high-level features): 
cc.columns = ['lepton pT','lepton eta','lepton phi','missing energy magnitude',
                'missing energy phi','jet 1 pt','jet 1 eta','jet 1 phi',
                'jet 1 b-tag','jet 2 pt','jet 2 eta','jet 2 phi','jet 2 b-tag',
                'jet 3 pt','jet 3 eta','jet 3 phi','jet 3 b-tag','jet 4 pt',
                'jet 4 eta','jet 4 phi','jet 4 b-tag','m_jj','m_jjj','m_lv',
                'm_jlv','m_bb','m_wbb','m_wwbb']

corr = cc.corr()
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap
cmap = sns.diverging_palette(220, 10, as_cmap=True)

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, cmap=cmap, vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5})

###########################################


from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from keras.optimizers import SGD
from sklearn.metrics import roc_auc_score
from keras import optimizers

y_train = np.array(data.loc[:,0])
y_train = y_train.reshape(-1,1) # nessecarily for method 2
x_train = np.array(data.loc[:,1:])
x_test = np.array(test_data.loc[:,1:])
y_test = np.array(test_data.loc[:,0])



# Model #1 Hidden Layers-1
# Number of neurons;  50
# Dropout % 0.20
act = tf.nn.relu
np.random.seed(42)
model1 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act, kernel_initializer='random_uniform'),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])

model1.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])

model1.fit(x_train, y_train, epochs=25,batch_size=1000)
# See Sheet
model1.evaluate(x_test, y_test)
    #sigmoid 500000/500000 [==============================] - 10s 20us/sample - loss: 0.5463 - acc: 0.7192
    #Out[67]: [0.5462561998291016, 0.719156]
        #tanh 500000/500000 [==============================] - 12s 24us/sample - loss: 0.6074 - acc: 0.6611
        #Out[109]: [0.6074042194004059, 0.66107]
            #relu 500000/500000 [==============================] - 13s 25us/sample - loss: 0.5833 - acc: 0.6935
            #Out[136]: [0.58332225872612, 0.693536]
roc_auc_score(y_test,model1.predict(x_test))
    #sigmoid Out[68]: 0.7955977870008317
        #tanh Out[110]: 0.7491576370383417
            #relu Out[137]: 0.7656150409797512
model1.summary()
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense_13 (Dense)             (None, 50)                1450      
    #_________________________________________________________________
    #dense_14 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_7 (Dropout)          (None, 50)                0         
    #_________________________________________________________________
    #dense_15 (Dense)             (None, 1)                 51        
    #=================================================================
    #Total params: 4,051
    #Trainable params: 4,051
    #Non-trainable params: 0
    #_________________________________________________________________

#Saving models for future use
model1_2_2_relu_25epoch = model1.history.history
model1.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model1_2_2_relu_25epoch.h5')
#model1_sigmoid_25epoch['acc'][1]



# Model #2 Hidden Layers-3
# Number of neurons;  50,50,100
# Dropout % 0.20
model2 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])
model2.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])
model2.fit(x_train, y_train, epochs=25,batch_size=1000)
# See Spreadsheet
model2.evaluate(x_test, y_test)
    #sigmoid 500000/500000 [==============================] - 16s 32us/sample - loss: 0.5434 - acc: 0.7204
    #Out[123]: [0.5434014899806976, 0.720364]
        #tanh 500000/500000 [==============================] - 15s 29us/sample - loss: 0.5446 - acc: 0.7214
        #Out[115]: [0.5446132134284973, 0.721358]
            #relu 500000/500000 [==============================] - 15s 30us/sample - loss: 0.5746 - acc: 0.7117
            #Out[141]: [0.574634434967041, 0.711668]
roc_auc_score(y_test,model2.predict(x_test))
    #sigmoid Out[73]: 0.7909795835368331
        #tanh Out[116]: 0.7974080490996827
            #relu Out[142]: 0.7836337294987561
model2.summary()
    #_________________________________________________________________
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense_16 (Dense)             (None, 50)                1450      
    #_________________________________________________________________
    #dense_17 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_8 (Dropout)          (None, 50)                0         
    #_________________________________________________________________
    #dense_18 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_9 (Dropout)          (None, 50)                0         
    #_________________________________________________________________
    #dense_19 (Dense)             (None, 100)               5100      
    #_________________________________________________________________
    #dropout_10 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_20 (Dense)             (None, 1)                 101       
    #=================================================================
    #Total params: 11,751
    #Trainable params: 11,751
    #Non-trainable params: 0
    #_________________________________________________________________


#Saving models for future use
model2_2_2_relu_25epoch = model2.history.history
model2.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model2_2_2_relu_25epoch.h5')



# Model #3 Hidden Layers-6
# Number of neurons;  50,50,100,100,80,80
# Dropout % 0.20
model3 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])
model3.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])

model3.fit(x_train, y_train, epochs=25,batch_size=1000)

model3.evaluate(x_test, y_test)
    #Sigmoid 500000/500000 [==============================] - 17s 34us/sample - loss: 0.5463 - acc: 0.7191
    #Out[86]: [0.5463351220207214, 0.719114]
        #tanh 500000/500000 [==============================] - 19s 38us/sample - loss: 0.5283 - acc: 0.7342
        #Out[130]: [0.5283023863420486, 0.734198]
            #relu 500000/500000 [==============================] - 19s 38us/sample - loss: 0.5584 - acc: 0.7094
            #Out[146]: [0.5583731457977295, 0.70944]

roc_auc_score(y_test,model3.predict(x_test))
    #sigmoid Out[87]: 0.7951442546686375
        #tanh Out[131]: 0.8124479478926048
            #relu Out[147]: 0.8005893624291227

model3.summary()
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense_36 (Dense)             (None, 50)                1450      
    #_________________________________________________________________
    #dense_37 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_16 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_38 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_17 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_39 (Dense)             (None, 100)               5100      
    #_________________________________________________________________
    #dropout_18 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_40 (Dense)             (None, 100)               10100     
    #_________________________________________________________________
    #dropout_19 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_41 (Dense)             (None, 80)                8080      
    #_________________________________________________________________
    #dropout_20 (Dropout)         (None, 80)                0         
    #_________________________________________________________________
    #dense_42 (Dense)             (None, 80)                6480      
    #_________________________________________________________________
    #dropout_21 (Dropout)         (None, 80)                0         
    #_________________________________________________________________
    #dense_43 (Dense)             (None, 1)                 81        
    #=================================================================
    #Total params: 36,391
    #Trainable params: 36,391
    #Non-trainable params: 0
    #_________________________________________________________________

model3_2_2_relu_25epoch = model3.history.history
model3.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model3_2_2_relu_25epoch.h5')




Prob11_SigmoidDF = pd.DataFrame.from_dict(model1_sigmoid_25epoch)
Prob12_SigmoidDF = pd.DataFrame.from_dict(model2_sigmoid_25epoch)
Prob13_SigmoidDF = pd.DataFrame.from_dict(model3_sigmoid_25epoch)
Prob1_SigmoidDF = pd.concat([Prob11_SigmoidDF,Prob12_SigmoidDF,Prob13_SigmoidDF],axis=1)
Prob1_SigmoidDF.columns = ['model1_sigmoid_25epoch_loss','model1_sigmoid_25epoch_acc',
                         'model2_sigmoid_25epoch_loss','model2_sigmoid_25epoch_acc',
                         'model3_sigmoid_25epoch_loss','model3_sigmoid_25epoch_acc']
Prob1_SigmoidDF.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob1_SigmoidDF.csv')

#######################################
# Problem #2 graphics - Activation Functions - Sigmoid

Prob2A_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob1_SigmoidDF.csv') 
Prob2B_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_tanhDF.csv')
Prob2C_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_reluDF.csv')

fig, ax = plt.subplots()
ax.plot(Prob2A_DF_plot.model1_sigmoid_25epoch_acc, '-b', label='Sigmoid')
ax.plot(Prob2B_DF_plot.model1_tanh_25epoch_acc, '-g', label='Tanh')
ax.plot(Prob2C_DF_plot.model1_relu_25epoch_acc, '-g', label='Relu')
plt.title('Problem #2 Model Type-1   Hidden Layers 1; Neurons 50-50-1\nAccuracy-Activation Function vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#########################################




Mod1_2_1_tanhDF = pd.DataFrame.from_dict(model1_2_1_tanh_25epoch)
Mod2_2_1_tanhDF = pd.DataFrame.from_dict(model2_2_1_tanh_25epoch)
Mod3_2_1_tanhDF = pd.DataFrame.from_dict(model3_2_1_tanh_25epoch)
Prob2_tanhDF = pd.concat([Mod1_2_1_tanhDF,Mod2_2_1_tanhDF,Mod3_2_1_tanhDF],axis=1)
Prob2_tanhDF.columns = ['model1_tanh_25epoch_loss','model1_tanh_25epoch_acc',
                         'model2_tanh_25epoch_loss','model2_tanh_25epoch_acc',
                         'model3_tanh_25epoch_loss','model3_tanh_25epoch_acc']
Prob2_tanhDF.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_tanhDF.csv')


Mod1_2_2_reluDF = pd.DataFrame.from_dict(model1_2_2_relu_25epoch)
Mod2_2_2_reluDF = pd.DataFrame.from_dict(model2_2_2_relu_25epoch)
Mod3_2_2_reluDF = pd.DataFrame.from_dict(model3_2_2_relu_25epoch)
Prob2_reluDF = pd.concat([Mod1_2_2_reluDF,Mod2_2_2_reluDF,Mod3_2_2_reluDF],axis=1)
Prob2_reluDF.columns = ['model1_relu_25epoch_loss','model1_relu_25epoch_acc',
                         'model2_relu_25epoch_loss','model2_relu_25epoch_acc',
                         'model3_relu_25epoch_loss','model3_relu_25epoch_acc']
Prob2_reluDF.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_reluDF.csv')


#######################################
# Problem #2 graphics - Activation Functions - Model1

Prob2A_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob1_SigmoidDF.csv') 
Prob2B_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_tanhDF.csv')
Prob2C_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_reluDF.csv')

fig, ax = plt.subplots()
ax.plot(Prob2A_DF_plot.model1_sigmoid_25epoch_acc, '-b', label='Sigmoid')
ax.plot(Prob2B_DF_plot.model1_tanh_25epoch_acc, '-g', label='Tanh')
ax.plot(Prob2C_DF_plot.model1_relu_25epoch_acc, '-r', label='Relu')
plt.title('Problem #2 Model Type-1   Hidden Layers 1; Neurons 50-50-1\nAccuracy-Activation Function vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#------
# graphics - Activation Functions - Model2

fig, ax = plt.subplots()
ax.plot(Prob2A_DF_plot.model2_sigmoid_25epoch_acc, '-b', label='Sigmoid')
ax.plot(Prob2B_DF_plot.model2_tanh_25epoch_acc, '-g', label='Tanh')
ax.plot(Prob2C_DF_plot.model2_relu_25epoch_acc, '-r', label='Relu')
plt.title('Problem #2 Model Type-2   Hidden Layers 3; Neurons 50-50-50-100-1\nAccuracy-Activation Function vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#------
#graphics - Activation Functions - Model3

fig, ax = plt.subplots()
ax.plot(Prob2A_DF_plot.model3_sigmoid_25epoch_acc, '-b', label='Sigmoid')
ax.plot(Prob2B_DF_plot.model3_tanh_25epoch_acc, '-g', label='Tanh')
ax.plot(Prob2C_DF_plot.model3_relu_25epoch_acc, '-r', label='Relu')
plt.title('Problem #2 Model Type-3   Hidden Layers 6; Neurons 50-50-50-100-100-80-80-1\nAccuracy-Activation Function vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();


#------
#graphics - Activation Functions - tanh - comparing Models
fig, ax = plt.subplots()
ax.plot(Prob2C_DF_plot.model1_relu_25epoch_acc, '-b', label='Model 1')
ax.plot(Prob2C_DF_plot.model2_relu_25epoch_acc, '-g', label='Model 2')
ax.plot(Prob2C_DF_plot.model3_relu_25epoch_acc, '-r', label='Model 3')
plt.title('Problem #2 Activation Function - Relu vs Models\nAccuracy-Activation Function vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();



#####################################################################
#####################################################################
#Problem #3

# Best Model: Model 3; Activation Function - tanh

# Running Model with batch size at least 2 orders of magnitude

act = tf.nn.tanh
np.random.seed(42)
# Model #3 Hidden Layers-6
# Number of neurons;  50,50,100,100,80,80
# Dropout % 0.20
model3 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])
model3.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])

model3.fit(x_train, y_train, epochs=25,batch_size=1000)

model3.evaluate(x_test, y_test)
    #500000/500000 [==============================] - 17s 33us/sample - loss: 0.6703 - acc: 0.5843
    #Out[9]: [0.6703165130882264, 0.584286]
roc_auc_score(y_test,model3.predict(x_test))
    #Out[10]: 0.634416375272185
model3.summary()
    #_________________________________________________________________
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense (Dense)                (None, 50)                1450      
    #_________________________________________________________________
    #dense_1 (Dense)              (None, 50)                2550      
    #_________________________________________________________________
    #dropout (Dropout)            (None, 50)                0         
    #_________________________________________________________________
    #dense_2 (Dense)              (None, 50)                2550      
    #_________________________________________________________________
    #dropout_1 (Dropout)          (None, 50)                0         
    #_________________________________________________________________
    #dense_3 (Dense)              (None, 100)               5100      
    #_________________________________________________________________
    #dropout_2 (Dropout)          (None, 100)               0         
    #_________________________________________________________________
    #dense_4 (Dense)              (None, 100)               10100     
    #_________________________________________________________________
    #dropout_3 (Dropout)          (None, 100)               0         
    #_________________________________________________________________
    #dense_5 (Dense)              (None, 80)                8080      
    #_________________________________________________________________
    #dropout_4 (Dropout)          (None, 80)                0         
    #_________________________________________________________________
    #dense_6 (Dense)              (None, 80)                6480      
    #_________________________________________________________________
    #dropout_5 (Dropout)          (None, 80)                0         
    #_________________________________________________________________
    #dense_7 (Dense)              (None, 1)                 81        
    #=================================================================
    #Total params: 36,391
    #Trainable params: 36,391
    #Non-trainable params: 0
    #_________________________________________________________________

model3_tanh_25epoch_100Kbatch = model3.history.history
model3.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model_Prob3_tanh_25epoch_100Kbatch.h5')

Prob3_tanh_25epoch_100kbatchDF = pd.DataFrame.from_dict(model3_tanh_25epoch_100Kbatch)
Prob3_tanh_25epoch_100kbatchDF.columns = ['Prob3_tanh_25epoch_100kbatchDF_loss','Prob3_tanh_25epoch_100kbatchDF_acc']
Prob3_tanh_25epoch_100kbatchDF.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob3_tanh_25epoch_100kbatchDF.csv')

#######################################
# Problem #3 graphics

Prob3A_DF_plot=pd.read_csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob3_tanh_25epoch_100kbatchDF.csv") 
Prob3B_DF_plot=pd.read_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob2_tanhDF.csv')
fig, ax = plt.subplots()
ax.plot(Prob3A_DF_plot.Prob3_tanh_25epoch_100kbatchDF_acc, '-b', label='100K Batch')
ax.plot(Prob3B_DF_plot.model3_tanh_25epoch_acc, '-g', label='1K Batch')
plt.title('Problem #3 \nModel Accuracy-Batch Size vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#########################################


#####################################################################
#####################################################################
#  Problem #4 Take best model from 3 and try 3 different kernal initializers 


act = tf.nn.tanh
kern = 'VarianceScaling'
np.random.seed(42)
# Model #3 Hidden Layers-6
# Number of neurons;  50,50,100,100,80,80
# Dropout % 0.20
model4 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act,kernel_initializer=kern),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])
model4.compile(optimizer='adam',
              loss='binary_crossentropy',
              metrics=['accuracy'])

model4.fit(x_train, y_train, epochs=25,batch_size=1000)

model4.evaluate(x_test, y_test)
    #uniform 500000/500000 [==============================] - 18s 36us/sample - loss: 0.5617 - acc: 0.7117
    #Out[21]: [0.5616875746669769, 0.711664]
        #RandomNormal 500000/500000 [==============================] - 18s 36us/sample - loss: 0.5615 - acc: 0.7146
        #Out[27]: [0.5614667445354462, 0.714616]
            #Orthogonal 500000/500000 [==============================] - 18s 37us/sample - loss: 0.5388 - acc: 0.7249
            #Out[33]: [0.5388194880161286, 0.724886]
                #VarianceScaling 500000/500000 [==============================] - 19s 38us/sample - loss: 0.5324 - acc: 0.7296
                #Out[38]: [0.5324102998027801, 0.729626]

roc_auc_score(y_test,model4.predict(x_test))
    #uniform Out[22]: 0.7825732460190485
        #RandomNormal Out[28]: 0.7897035163009849
            #Ortogonal Out[34]: 0.8037352701470606
                #VarianceScaling Out[39]: 0.807966897119839

model4.summary()
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense_16 (Dense)             (None, 50)                1450      
    #_________________________________________________________________
    #dense_17 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_12 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_18 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_13 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_19 (Dense)             (None, 100)               5100      
    #_________________________________________________________________
    #dropout_14 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_20 (Dense)             (None, 100)               10100     
    #_________________________________________________________________
    #dropout_15 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_21 (Dense)             (None, 80)                8080      
    #_________________________________________________________________
    #dropout_16 (Dropout)         (None, 80)                0         
    #_________________________________________________________________
    #dense_22 (Dense)             (None, 80)                6480      
    #_________________________________________________________________
    #dropout_17 (Dropout)         (None, 80)                0         
    #_________________________________________________________________
    #dense_23 (Dense)             (None, 1)                 81        
    #=================================================================
    #Total params: 36,391
    #Trainable params: 36,391
    #Non-trainable params: 0
    #_________________________________________________________________


model4_4_VarianceScaling = model4.history.history
model4.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model4_4_VarianceScaling.h5')



Prob4_uniformDF = pd.DataFrame.from_dict(model4_1_uniform)
Prob4_RandomNormalDF = pd.DataFrame.from_dict(model4_2_RandomNormal)
Prob4_OrtogonalDF = pd.DataFrame.from_dict(model4_3_Ortogonal)
Prob4_VarianceScalingDF = pd.DataFrame.from_dict(model4_4_VarianceScaling)

Prob4_DF = pd.concat([Prob4_uniformDF,Prob4_RandomNormalDF,Prob4_OrtogonalDF,Prob4_VarianceScalingDF],axis=1)
Prob4_DF.columns = ['uniform_loss','uniform_acc',
                         'RandomNormal_loss','RandomNormal_acc',
                         'Ortogonal_loss','Ortogonal_25epoch_acc',
                         'Variance_loss','Variance_acc']
Prob4_DF.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob4_DF.csv')

#######################################
# Problem #4 graphics

Prob4_DF_plot=pd.read_csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob4_DF.csv") 

fig, ax = plt.subplots()
ax.plot(Prob4_DF_plot.uniform_acc, '-b', label='Uniform')
ax.plot(Prob4_DF_plot.RandomNormal_acc, '-g', label='RandomNormal')
ax.plot(Prob4_DF_plot.Ortogonal_25epoch_acc, '-y', label='Ortogonal')
ax.plot(Prob4_DF_plot.Variance_acc, '-r', label='Variance')
plt.title('Problem #4 \nModel Accuracy-Kernel Initializers vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#########################################







###############################################################################
###############################################################################
#  Problem #5 Take best model from 3 and try 3 different optimizers 


act = tf.nn.tanh
np.random.seed(42)
# Model #3 Hidden Layers-6
# Number of neurons;  50,50,100,100,80,80
# Dropout % 0.20
model5 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(100, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(80, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])
model5.compile(optimizer='adadelta',
              loss='binary_crossentropy',
              metrics=['accuracy'])

model5.fit(x_train, y_train, epochs=25,batch_size=1000)

model5.evaluate(x_test, y_test)
    #adam 500000/500000 [==============================] - 20s 40us/sample - loss: 0.5433 - acc: 0.7274
    #Out[51]: [0.5433235804748535, 0.727376]
        #RMSprop 500000/500000 [==============================] - 20s 40us/sample - loss: 0.5189 - acc: 0.7406
        #Out[62]: [0.5189299085597991, 0.74063]
            #sgd 500000/500000 [==============================] - 19s 37us/sample - loss: 0.6297 - acc: 0.6502
            #Out[68]: [0.6296809244155884, 0.650238]
                #adadelta 500000/500000 [==============================] - 19s 39us/sample - loss: 0.5366 - acc: 0.7273
                #Out[75]: [0.5366370195713043, 0.727302]

roc_auc_score(y_test,model5.predict(x_test))
    #adam Out[52]: 0.8069738775920415
        #RMSprop Out[63]: 0.8201887933972172
            #sgd Out[69]: 0.6956203140084707
                #adadelta Out[76]: 0.8047374342268395

model5.summary()
    #_________________________________________________________________
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense_96 (Dense)             (None, 50)                1450      
    #_________________________________________________________________
    #dense_97 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_72 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_98 (Dense)             (None, 50)                2550      
    #_________________________________________________________________
    #dropout_73 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_99 (Dense)             (None, 100)               5100      
    #_________________________________________________________________
    #dropout_74 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_100 (Dense)            (None, 100)               10100     
    #_________________________________________________________________
    #dropout_75 (Dropout)         (None, 100)               0         
    #_________________________________________________________________
    #dense_101 (Dense)            (None, 80)                8080      
    #_________________________________________________________________
    #dropout_76 (Dropout)         (None, 80)                0         
    #_________________________________________________________________
    #dense_102 (Dense)            (None, 80)                6480      
    #_________________________________________________________________
    #dropout_77 (Dropout)         (None, 80)                0         
    #_________________________________________________________________
    #dense_103 (Dense)            (None, 1)                 81        
    #=================================================================
    #Total params: 36,391
    #Trainable params: 36,391
    #Non-trainable params: 0
    #_________________________________________________________________


model5_4_adadelta = model5.history.history
model5.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model5_4_adadelta.h5')
 
Prob5_adamDF = pd.DataFrame.from_dict(model5_1_adam)
Prob5_RMSpropDF = pd.DataFrame.from_dict(model5_2_RMSprop)
Prob5_sgdDF = pd.DataFrame.from_dict(model5_3_sgd)
Prob5_adadeltaDF = pd.DataFrame.from_dict(model5_4_adadelta)

Prob5_DF = pd.concat([Prob5_adamDF,Prob5_RMSpropDF,Prob5_sgdDF,Prob5_adadeltaDF],axis=1)
Prob5_DF.columns = ['adam_loss','adam_acc',
                         'RMSprop_loss','RMSprop_acc',
                         'sgd_loss','sgd_acc',
                         'adadelta_loss','adadelta_acc']
Prob5_DF.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob5_DF.csv')

#######################################
# Problem #5 graphics

Prob5_DF_plot=pd.read_csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob5_DF.csv") 

fig, ax = plt.subplots()
ax.plot(Prob5_DF_plot.adam_acc, '-b', label='adam')
ax.plot(Prob5_DF_plot.RMSprop_acc, '-g', label='RMSprop')
ax.plot(Prob5_DF_plot.sgd_acc, '-y', label='SGD')
ax.plot(Prob5_DF_plot.adadelta_acc, '-r', label='AdaDelta')
plt.title('Problem #5 \nModel Optimizer Accuracy vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#########################################


# Problem #6 - Final push for a high AUC score.
act = tf.nn.tanh
kern = 'VarianceScaling'
np.random.seed(42)
# Model #3 Hidden Layers-6
# Number of neurons;  50,50,100,100,80,80
# Dropout % 0.20
model6 = tf.keras.models.Sequential([
  tf.keras.layers.Dense(50, input_shape=(28,), activation=act,kernel_initializer=kern),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(50, activation=act),
  tf.keras.layers.Dropout(0.2),
  tf.keras.layers.Dense(1, activation=act)
])
model6.compile(optimizer='RMSprop',
              loss='binary_crossentropy',
              metrics=['accuracy'])

model6.fit(x_train, y_train, epochs=100,batch_size=300)

model6.evaluate(x_test, y_test)
    #500000/500000 [==============================] - 20s 41us/sample - loss: 0.5139 - acc: 0.7469
    #Out[176]: [0.5139309719181061, 0.746896]

roc_auc_score(y_test,model6.predict(x_test))
    #roc_auc_score(y_test,model6.predict(x_test))
    #Out[178]: 0.8274673123171997


model6.summary()
    # _________________________________________________________________
    #Layer (type)                 Output Shape              Param #   
    #=================================================================
    #dense_105 (Dense)            (None, 50)                1450      
    #_________________________________________________________________
    #dense_106 (Dense)            (None, 50)                2550      
    #_________________________________________________________________
    #dropout_78 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_107 (Dense)            (None, 50)                2550      
    #_________________________________________________________________
    #dropout_79 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_108 (Dense)            (None, 50)                2550      
    #_________________________________________________________________
    #dropout_80 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_109 (Dense)            (None, 50)                2550      
    #_________________________________________________________________
    #dropout_81 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_110 (Dense)            (None, 50)                2550      
    #_________________________________________________________________
    #dropout_82 (Dropout)         (None, 50)                0         
    #_________________________________________________________________
    #dense_111 (Dense)            (None, 1)                 51        
    #=================================================================
    #Total params: 14,251
    #Trainable params: 14,251
    #Non-trainable params: 0
    #_________________________________________________________________


model6_ultimate = model6.history.history

model6.save('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model6_ultimate.h5')
Prob6_ultimate = pd.DataFrame.from_dict(model6_ultimate)
Prob6_ultimate.columns = ['loss','acc']
Prob6_ultimate.to_csv('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob6_DF.csv')

#######################################
# Problem #6 graphics

Prob6_DF_plot=pd.read_csv("C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Prob6_DF.csv") 

fig, ax = plt.subplots()
ax.plot(Prob6_DF_plot.acc, '-b', label='acc')
ax.plot(Prob6_DF_plot.loss, '-g', label='loss')
plt.title('Problem #6 \nOptimized Model Accuracy & Loss vs Epochs')
plt.xlabel('Epochs')
plt.ylabel('Training Accuracy')
leg = ax.legend();

#########################################


##### Example - Retrieving saved model #####
new_model = tf.keras.models.load_model('C:/Users/Prodigy/Documents/GitRepositories/MSDS_7333_QTW/CaseStudy_12/Models/my_model3_2_2_relu_25epoch.h5')
new_model_history = new_model.history.history
new_model.summary()
loss, acc = new_model.evaluate(x_test, y_test)
print("Restored model, accuracy: {:5.2f}%".format(100*acc))
############################################



N=10500000. #Change this line adjust the number of rows. 
data=pd.read_csv("HIGGS.csv",nrows=N,header=None)
test_data=pd.read_csv("HIGGS.csv",nrows=500000,header=None,skiprows=10500000)

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


Southern Methodist University
MSDS 7333
Quantifying the World
Course Designer: Associate Professor Monnie McGee; Department of Statistical Science; Southern Methodist University, Dallas, Texas
Course Instructors: Alan Elliott, Daniel Engels, Eric Larson, Monnie McGee and Melvin Greer
Required Texts:
McKinley, Wes (2014, 2017). Python for Data Analysis. O’Reilly
Publishers, available in PDF download (McKinley).



Take a subset of the data and run the neural net presented in class:
N can be any number greater than 1 million, but less than 10.5 million (8GB Ram recommended for all the data).

data source: https://archive.ics.uci.edu/ml/machine-learning-databases/00280/

Work (10 points each)
1.	Pick 3 or more different architectures (add/subtract layers+neurons) and run the model + score. 
2.	With those same 3 architectures, run the SAME architecture but with 2 different (from sigmoid) activation functions.  Google the Keras documentation for a look at different available activations. 
3.	Take your best model from parts 1&2 and vary the batch size by at least 2 orders of magnitude
4.	Take your best model (score) from parts 1&2 and use 3 different kernel initializers. Use a reasonable batch size.
5.	Take your best results from #3 and try 3 different optimizers. (LMGTFY)
6.	Take all that you’ve learned so far and give your best shot at producing a score. 
Questions to be  answered (These are loaded questions—be warned they are there to test your understanding):
10 points - Q1: What was the effect of adding more layers/neurons.
10 points - Q2: Which parameters gave you the best result and why (in your opinion) did they work.
20 points Q3: For #6, how did you decide that your model was ‘done’


Abstract:
The purpose of this study is to explore neural networks and the effects parameter selections have on the accuracy of a model.   The study is an extension of work previously conducted by the authors of Searching for exotic particles in high-energy physics with deep learning (BSW_HIGGS), Baldi, P., P. Sadowski, and D. Whiteson [1][2].  BSW_HIGGS paper focused on identifying collisions at high-energy colliders the produce exotic particles (HIGGS bosons).  Finding these particles requires identification of signature signals through background noise, classification problem, utilizing machine-learning methodology. We will employ the same benchmark data sets used for the BSW_HIGGS article, which is publicly available through UC Irvine’s machine learning repository [3]. 
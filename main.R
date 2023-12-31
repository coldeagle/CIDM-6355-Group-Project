install.packages(c('e1071','nnet', 'party', 'NeuralNetTools', 'dplyr', 'reshape2'))
library('e1071')
library('nnet')
library('caret')
library('party')
library('NeuralNetTools')
library('dplyr')
library('reshape2')
# Setting the seed to be the same so RM and R have similar results when splitting
set.seed(1000)
source('helper.R')
# Using unfactored strings here so we can do som manipulation
data_cleaned <- do_clean_data(read.csv('data/BankChurners.csv', header = T, stringsAsFactors = F))
# Getting the data sample, we can re-use this on both factored and unfactored data since it's going by the index
data_sample <-sample(seq_len(nrow(data_cleaned)), size = floor(.7 * nrow(data_cleaned)))
# Creating the data sets
data_training <-data_cleaned[data_sample,]
data_testing <- data_cleaned[-data_sample,]

# Creating the logistical regression model and testing it
lrP_class <-do_lr_prediction(data_testing, data_training, Attrition_Flag ~.)
cm_lr <- caret::confusionMatrix(lrP_class, as.factor(data_testing[['Attrition_Flag']]))
cm_lr # Printing the confusion matrix

# Creating the naive bayes model and testing it
nb <- naiveBayes(Attrition_Flag ~., data = data_training)
nb_p <- predict(nb, data_testing)
cm_nb <- caret::confusionMatrix(nb_p,  as.factor(data_testing[['Attrition_Flag']]))
cm_nb # Printing the confusion matrix

# Creating a version of the data that factors the strings
data_cleaned_factored <- do_snip_data(read.csv('data/BankChurners.csv', header = T, stringsAsFactors = T))
data_training_factored <-data_cleaned_factored[data_sample,]
data_testing_factored <- data_cleaned_factored[-data_sample,]

# Creating a decision tree model and testing it
dt <-ctree(Attrition_Flag ~., data = data_training_factored)
dt_p <- predict(dt, data_testing_factored)
cm_dt <- caret::confusionMatrix(dt_p, data_testing_factored[['Attrition_Flag']])
cm_dt # Printing the confusion matrix

# Getting the data formatted for nn
nn_data <- get_nn_data(data_cleaned_factored)
# Re-sampling since we might have removed some data
nn_data_sample <-sample(seq_len(nrow(nn_data)), size = floor(.7 * nrow(nn_data)))
# Getting training and testing sets
nn_data_training_factored <-nn_data[nn_data_sample,]
nn_data_testing_factored <- nn_data[-nn_data_sample,]
nn <- nnet(Attrition_Flag ~., data = nn_data_training_factored, size = 8, maxit = 500) # Creating the model
nn_p <- predict(nn, nn_data_testing_factored, type = 'class') # Applying the model. The results all came out as "1"
# Getting the results as a confusion matrix
cm_nn <- caret::confusionMatrix(as.factor(nn_p), as.factor(nn_data_testing_factored[['Attrition_Flag']]))
cm_nn # Printing the results
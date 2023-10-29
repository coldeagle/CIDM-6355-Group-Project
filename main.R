install.packages(c('e1071','nnet', 'party'))
#library('party')
library('e1071')
library('nnet')
library('caret')
library('party')
set.seed(1000)
source('helper.R')
data_cleaned <- do_clean_data(read.csv('data/BankChurners.csv', header = T, stringsAsFactors = F))
data_sample <-sample(seq_len(nrow(data_cleaned)), size = floor(.7 * nrow(data_cleaned)))

data_training <-data_cleaned[data_sample,]
data_testing <- data_cleaned[-data_sample,]

lrP_class <-do_lr_prediction(data_testing, data_training, Attrition_Flag ~.)
cm_lr <- caret::confusionMatrix(lrP_class, as.factor(data_testing[['Attrition_Flag']]))

nb <- naiveBayes(Attrition_Flag ~., data = data_training)
nb_r <- predict(nb, data_testing)
cm_nb <- caret::confusionMatrix(nb_r,  as.factor(data_testing[['Attrition_Flag']]))

data_cleaned_factored <- do_snip_data(read.csv('data/BankChurners.csv', header = T, stringsAsFactors = T))
data_training_factored <-data_cleaned_factored[data_sample,]
data_testing_factored <- data_cleaned_factored[-data_sample,]

dt <-ctree(Attrition_Flag ~., data = data_training_factored)
dt_p <- predict(dt, data_testing_factored)
dt_p
cm_dt <- caret::confusionMatrix(dt_p, data_testing_factored[['Attrition_Flag']])
cm_dt

# NN isthe only onethat's giving me isseus
nn <- nnet(Attrition_Flag ~., data = data_training_factored, size = 8, maxit = 100000)
nn_p <- predict(nn, data_testing_factored)
nn_p
nn_pr <-as.factor(ifelse(nn_p>.5, 1,0))
#cm_nn<- caret::confusionMatrix(nn_p, data_testing_factored[['Attrition_Flag']])
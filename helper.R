# This funciton will clean the data in a way that the models can interpret
do_clean_data <- function(data_raw) {
  data_cleaned <- do_snip_data(data_raw)
  head(data_cleaned$Attrition_Flag)
  data_cleaned$Attrition_Flag[data_cleaned$Attrition_Flag=='Attrited Customer']<-1
  data_cleaned$Attrition_Flag[data_cleaned$Attrition_Flag=='Existing Customer']<-0
  data_cleaned$Attrition_Flag<-as.integer(as.character(data_cleaned$Attrition_Flag))

  return(data_cleaned)
}


# This function will snip the data that we want
do_snip_data <- function(data_raw) {
  data_cleaned <- data_raw[1:21] # Grabbing the first 21 columns
  rownames(data_cleaned) <- data_cleaned$CLIENTNUM
  # Removing Data that is unkown, this removes ~3k records
  data_cleaned <- subset(
    data_raw,
      data_cleaned$Education_Level != 'Unknown' &
      data_cleaned$Marital_Status != 'Unknown' &
      data_cleaned$Income_Category != 'Unknown',
    select = c(1:2,5:15,17:18,20:21), # Narrowing our column choices
  )
  return(data_cleaned)
}

# This function will get the sample data
do_get_data_sample <- function(data, factor) {
  return (sample(seq_len(nrow(data)), size = floor(factor * nrow(data))))
}

# This function will do the linear regression prediction
do_lr_prediction <- function(data_testing, data_training, model_args) {
  lrp <-get_lr_model_predictions(data_testing, data_training, model_args)

  lrPredict <- ifelse(lrp >.2, 1, 0)
  lrP_class <-as.factor(lrPredict)

  return(lrP_class)

}

# This function will get the linear regression model
get_lr_model_predictions <- function(data_testing, data_training, model_args) {
  lrModel <-glm(model_args, family = 'binomial', data_training)
  lrp <-predict(lrModel, data_testing, type = 'response')
  return(lrp)
}

# This function will manipulate the data for neural network modeling and predicting
get_nn_data <- function(nn_data) {
  nn_data[1] <- NULL
  # Getting the factored data to behave as numbers
  nn_data[sapply(nn_data, is.factor)] <- data.matrix(nn_data[sapply(nn_data, is.factor)])
  nn_cor <- as.matrix(cor(nn_data))
  nn_cor[!lower.tri(nn_cor)] <- 0
  return (nn_data[,!apply(nn_cor, 2, function (x) any(abs(x) > 0.8))])
}
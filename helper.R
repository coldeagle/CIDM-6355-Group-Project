do_snip_data <- function(data_raw) {
  data_cleaned <- data_raw[1:21] # Grabbing the first 21 columns
  # Removing Data that is unkown, this removes ~3k records
  data_cleaned <- subset(
    data_raw,
      data_cleaned$Education_Level != 'Unknown' &
      data_cleaned$Marital_Status != 'Unknown' &
      data_cleaned$Income_Category != 'Unknown', # This is probably the one we'll want to definately exclude unkown
    select = c(1:21),
  )
  return(data_cleaned)
}

do_clean_data <- function(data_raw) {
  data_cleaned <- do_snip_data(data_raw)
  head(data_cleaned$Attrition_Flag)
  data_cleaned$Attrition_Flag[data_cleaned$Attrition_Flag=='Attrited Customer']<-1
  data_cleaned$Attrition_Flag[data_cleaned$Attrition_Flag=='Existing Customer']<-0
  data_cleaned$Attrition_Flag<-as.integer(as.character(data_cleaned$Attrition_Flag))

  return(data_cleaned)
}

do_get_data_sample <- function(data, factor) {
  return (sample(seq_len(nrow(data)), size = floor(factor * nrow(data))))
}

do_lr_prediction <- function(data_testing, data_training, model_args) {
  lrModel <-glm(model_args, family = 'binomial', data_training)
  lrp <-predict(lrModel, data_testing, type = 'response')

  lrPredict <- ifelse(lrp >.2, 1, 0)
  lrP_class <-as.factor(lrPredict)

  return(lrP_class)

}


trainData_fn <- "C:\\Users\\Halvard\\Documents\\GitHub\\minimalist_example_lag_r\\input\\trainData.csv"
testData_fn <- "C:\\Users\\Halvard\\Documents\\GitHub\\minimalist_example_lag_r\\input\\futureClimateData.csv"
bin_fn <- "C:\\Users\\Halvard\\Documents\\GitHub\\minimalist_example_lag_r\\output\\model.bin"
pred_fn <- "C:\\Users\\Halvard\\Documents\\GitHub\\minimalist_example_lag_r\\output\\predict.csv"


df <- read.csv(trainData_fn)

model <- lm(disease_cases ~ rainfall + mean_temperature, data = df)
summary(model)


df2 <- read.csv(testData_fn)
X <- df2[, c("rainfall", "mean_temperature"), drop = FALSE]

y_pred <- predict(model, newdata = X)

#utils:
get_df_per_location <- function(csv_fn){
  full_df <- read.csv(csv_fn)
  locations <- split(full_df, full_df$location)
  return(locations)
}


#training:
train_chap <- function(csv_fn, model_fn) {
  dataframe_list <- get_df_per_location(csv_fn) #from utils
  
  models <- list()
  for (location in names(dataframe_list)){
    df <- dataframe_list[[location]]
    df$disease_cases[is.na(df$disease_cases)] <- 0 # set NaNs to zero (not a good solution, just for the example to work)
    model <- lm(disease_cases ~ rainfall + mean_temperature, data = df)
    models[[location]] <- model
  }
  saveRDS(models, file=model_fn)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  csv_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(csv_fn, model_fn)
}# else {
#  stop("Usage: Rscript train.R <csv_fn> <model_fn>")
#}

#test for train: SEEMS like its working
train_chap(trainData_fn, bin_fn)

#prediction:
predict_chap <- function(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn) {
  dataframe_list <- get_df_per_location(future_climatedata_fn)
  models <- readRDS(model_fn)  # Assumes the model was saved using saveRDS
  first_location <- TRUE
  for (location in names(dataframe_list)){
    df <- dataframe_list[[location]]
    X <- df[, c("rainfall", "mean_temperature"), drop = FALSE]
    model <- models[[location]]
    y_pred <- predict(model, newdata = X)
    df$sample_0 <- y_pred
    if (first_location){
      full_df <- df
      first_location <- FALSE
    }
    else {
      full_df <- rbind(full_df, df)
    }
    
    print(paste("Forecasted values:", paste(y_pred, collapse = ", ")))
  }
  write.csv(full_df, predictions_fn, row.names = FALSE)
}

#testing predict
predict_chap(bin_fn, "hist_fn", testData_fn, pred_fn)



#The simplest model:
#library(caret)  # For model training
#library(readr)  # For reading CSV files
#library(broom)  # For extracting model coefficients
#library(tools)  # For file operations like saving the model

train_chap <- function(csv_fn, model_fn) {
  df <- read_csv(csv_fn)
  df$disease_cases[is.na(df$disease_cases)] <- 0
  model <- lm(disease_cases ~ rainfall + mean_temperature, data = df)
  saveRDS(model, file=model_fn)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  csv_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(csv_fn, model_fn)
}# else {
#  stop("Usage: Rscript train.R <csv_fn> <model_fn>")
#}


predict_chap <- function(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn) {
  df <- read.csv(future_climatedata_fn)
  X <- df[, c("rainfall", "mean_temperature"), drop = FALSE]
  model <- readRDS(model_fn)  # Assumes the model was saved using saveRDS
  
  y_pred <- predict(model, newdata = X)
  df$sample_0 <- y_pred
  write.csv(df, predictions_fn, row.names = FALSE)
  
  print(paste("Forecasted values:", paste(y_pred, collapse = ", ")))
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 4) {
  model_fn <- args[1]
  historic_data_fn <- args[2]
  future_climatedata_fn <- args[3]
  predictions_fn <- args[4]
  
  predict_chap(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn)
}# else {
#  stop("Usage: Rscript predict.R <model_fn> <historic_data_fn> <future_climatedata_fn> <predictions_fn>")
#}

source("utils.R")

train_chap <- function(csv_fn, model_fn) {
  dataframe_list <- get_df_per_location(csv_fn) #from utils
  
  models <- list()
  for (location in names(dataframe_list)){
    model <- train_single_region(dataframe_list[[location]], location)
    models[[location]] <- model
  }
  saveRDS(models, file=model_fn)
}

train_single_region <- function(df, location){
  
}





#Multiregion model


source("utils.R")

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
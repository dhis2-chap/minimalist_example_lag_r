
source("utils.R")

train_chap <- function(csv_fn, model_fn) {
  dataframe_list <- get_df_per_location(csv_fn)
  
  models <- list()
  for (location in names(dataframe_list)){
    model <- train_single_region(dataframe_list[[location]], location)
    models[[location]] <- model
  }
  saveRDS(models, file=model_fn)
}

train_single_region <- function(df, location){
  df$disease_cases[is.na(df$disease_cases)] <- 0 # set NaNs to zero (not a good solution, just for the example to work)
  df <- create_lagged_feature(df, "mean_temperature", 1)
  df <- create_lagged_feature(df, "rainfall", 1)
  df <- create_lagged_feature(df, "disease_cases", 1)
  df <- cut_top_rows(df, 1)
  
  model <- lm(disease_cases ~ rainfall + rainfall_1 + mean_temperature + 
                mean_temperature_1 + disease_cases_1, data = df)
  
  print(paste0("Train - model coefficients for location ", location, ": "))
  print(model$coefficients)
  return(model)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  csv_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(csv_fn, model_fn)
}# else {
#  stop("Usage: Rscript train.R <csv_fn> <model_fn>")
#}
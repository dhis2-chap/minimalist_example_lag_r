



#Multiregion model




source("utils.R")

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

source("utils.R")

predict_chap <- function(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn) {
  future_per_location <- get_df_per_location(future_climatedata_fn)
  historic_per_location <- get_df_per_location(historic_data_fn)
  models <- readRDS(model_fn)  # Assumes the model was saved using saveRDS
  first_location <- TRUE
  
  for (location in names(future_per_location)){
    df <- future_per_location[[location]]
    historic_df <- historic_per_location[[location]]
    model <- models[[location]]

    df$disease_cases <- NA #makes the column so we can rowbind future and historic

    df_all <- rbind(historic_df, df)
    df_all <- create_lagged_feature(df_all, "mean_temperature", 1)
    df_all <- create_lagged_feature(df_all, "rainfall", 1)
    X <- df_all[(nrow(historic_df) + 1):nrow(df_all), c("rainfall", "rainfall_1", "mean_temperature", "mean_temperature_1"), drop = FALSE]
    
    last_disease_col <- get_lagged_col_name("disease_cases", 1)
    X[last_disease_col] <- NA
    df["sample_0"] <- NA
    
    prev_disease <- historic_df[nrow(historic_df), "disease_cases"]
    for(i in 1:nrow(X)){
      X[i, last_disease_col] <- prev_disease
      y_one_pred <- predict(model, newdata = X[i,])
      df[i, "sample_0"] <- y_one_pred
      
      prev_disease <- y_one_pred
    }
    if (first_location){
      full_df <- df
      first_location <- FALSE
    }
    else {
      full_df <- rbind(full_df, df)
    }
    print(paste("Forecasted values:", paste(df[, "sample_0", drop=TRUE], collapse = ", ")))
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
}





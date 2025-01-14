# Minimalist example of model integaration with multiple regions and lag in R
This document will explain how to expand the "minimalist_multiregion_r" model to include lag. 
We will use the same simple linear regression model, and keep the assumption of indepedent regions to keep the example simple.
The main change is then that we include lagged terms which affect both the traing and the prediction.
This introduces the need for some new helper functions which are defined in "utils.R" and briefly described below.

## Running the model without CHAP integration
The example can be run in isolation (e.g. from the command line) using the file isolated_run.R:
```
RScript isolated_run.R  
```

For details on code files and data, please consult the models in "minimalist_example_r" and "minimalist_multiregion_r".

## Helper functions in utils.R 
They are mostly the same as in "minimalist_example_lag", only translated to R. 
Explanations for the python code is in the repository "chap_model_dev_toolkit".
The only function which is changed from the python code is "create_lagged_feature" which in R removes the optional fourth argument for a "source_df" and added a boolean variable which determins if we make all lags up to the provided lag or only that lag. 

## Data for training and prediction
The training data now has an extra row for each location compared to the multiregion example. The future climate data is unchanged.


## Changes in the code
This example builds on the code from "minimalist_multiregion_r" and the main changes are:
* The training now uses lagged terms for all the covariates, including "disease_cases". The lagged covariates are made by "create_lagged_feature" before the top row is removed by "cut_top_rows" as the lagged features are undefined. This all takes place in the function "train_single_region" which is called iteratively for each region.
```
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
```

* When predicting for a location we start by row-binding the historic and future dataframes. We make a columns of NA's for disease_cases in the future dataframe so they match each other. Then we create the lagged coavriates as in the training and seperate the rows for the future time points into their own dataframe X, which now also has the lagged values. The lagged values for "disease_cases" need the previous prediction for the "disease_cases". Thus, the predictions are done one at a time and the current prediction is used as a covariate in the next prediction. This is shown in the code below:
```
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
}
```
* The remaining parts of "train.R" and "predict.R" are largely unchanged from "minimalist_multiregion_r", and "isolated_run.R" and "MLproject" are completly unchanged, except for the name in "MLproject".



get_df_per_location <- function(csv_fn){
  full_df <- read.csv(csv_fn)
  locations <- split(full_df, full_df$location)
  return(locations)
}

get_lagged_col_name <- function(feature, lag){
  #feature is a string and lag is an int
  return(paste0(feature, "_", lag))
}

#get_lagged_col_name("rainfall", 2)

shift <- function(v, lag){
  # v is a vector and lag is an int indicating how many places to shift
  stopifnot(length(v) > lag)
  v_shifted <- c(tail(v, lag), v[1:(length(v) - lag)])
  return(v_shifted) #can also just use v everywhere to reduce memory usage
}

#shift(c(1, 2, 3, 4, 5), 5)
#shift(df[["rainfall"]], 3)

create_lagged_feature <- function(df, feature, num_lags){
  lag_features <- c()
  for(lag in 1:(num_lags)){
    lag_features <- append(lag_features, get_lagged_col_name(feature, lag))
    df[tail(lag_features, 1)] <- shift(df[[feature]], lag) #use [[]] to get a vector for the given column
  }
  return(list(df = df, lag_features = lag_features))
}

#create_lagged_feature(df, "rainfall", 2)$df

cut_top_rows <- function(df, int_remove_rows){
  stopifnot(nrow(df) > int_remove_rows)
  return(df[(int_remove_rows + 1):nrow(df),])
}

#cut_top_rows(df, 2)

fill_top_rows_from_historic_last_rows <- function(feature, lag, future_df, historic_df){
  future_df[1:lag, get_lagged_col_name(feature, lag)] <- historic_df[(nrow(historic_df)-lag + 1):nrow(historic_df), feature]
  return(future_df)
}


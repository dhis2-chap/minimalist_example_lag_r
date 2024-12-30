
get_lagged_col_name <- function(feature, lag){
  #feature is a string and lag is an int
  return(paste0(feature, "_", lag))
}

#get_lagged_col_name("rainfall", 2)

create_lagged_feature <- function(df_to_change, feature, num_lags, source_df=NULL){
  lag_features <- c()
  for(lag in 1:(num_lags + 1)){
    lag_features <- append(lag_features, get_lagged_col_name(feature, lag))
    df_to_change[tail(lag_features, 1)] <- 
  }
  df_to_change["hei"] <- NA
  return(list(df <- df_to_change, lag_features <- lag_features))
}

df
create_lagged_feature(df, "a", "b")
df


#Multiregion model


get_df_per_location <- function(csv_fn){
  full_df <- read.csv(csv_fn)
  locations <- split(full_df, full_df$location)
  return(locations)
}

a <- c(1, 2)
b <- append(a, 4)
tail(b, 1)

# Function to forecast cumulative cases
forec_cases_cumsum <- function(data, n_ahead) {
  
  # Extract cumulative cases data as a time series
  data_ts <- ts(data[Cases_cumsum != 0, Cases_cumsum])
  
  # Check if the length of the time series is too low, and if so, replicate the first value
  if (length(data_ts) < 6) {
    data_ts <- ts(c(rep(data[Cases_cumsum != 0, Cases_cumsum][1], 6 - length(data_ts)),
                    data_ts))
  }
  
  # Perform exponential smoothing forecasting with multiple model choices
  pred <- es_old(data_ts,
                 model = c("MMN", "MMdN", "MAdN", "AMdN"), # Multiple choices of ETS model
                 ic = "BICc",                              # Information Criterion
                 h = n_ahead,                              # How many days ahead
                 loss = "MSE",                             # Mean Squared Error loss function
                 interval = "likelihood",                  # Use likelihood for prediction interval 
                 level = 0.90,                             # Use 90% PI
                 silent = "all"
  )
  
  # Handle cumulative ts, have to be increasing all the time
  if (pred$forecast[1] < data_ts[length(data_ts)]) {
    
    # Print a message indicating that only cases are used for forecasting
    print("using only cases!")
    
    # Extract non-zero cases data as a time series
    data_ts <- ts(data[Cases != 0, Cases])
    
    # Check if the length of the time series is too low and replicate the first value if needed
    if (length(data_ts) < 6) {
      data_ts <- ts(c(rep(data[Cases != 0, Cases][1], 6 - length(data_ts)),
                      data_ts))
    }
    
    # Perform exponential smoothing forecasting with different model choices
    pred <- es_old(data_ts,
                   model = c("MMN", "MMdN", "MAdN", "AMN", "AMdN"), # Multiple choices of ETS model
                   ic = "BICc",
                   h = n_ahead,
                   loss = "MSE",
                   interval = "likelihood",
                   level = 0.90,
                   silent = "all"
    )
    
    # Heuristic for Mean + Prediction Interval forecasts
    pred$forecast <- cumsum(pred$forecast) + data[.N, Cases_cumsum]
    pred$upper <- cumsum(pred$forecast) + data[.N, Cases_cumsum]
    pred$upper <- pred$upper + pred$upper*0.1
    
  }
  
  return(pred)
  
}

# Function to forecast cumulative deaths
forec_deaths_cumsum <- function(data, n_ahead) {
  
  # Extract cumulative deaths data as a time series
  data_ts <- ts(data[Deaths_cumsum != 0, Deaths_cumsum])
  
  # Check if the length of the time series is too low, and if so, replicate the first value
  if (length(data_ts) < 6) {
    data_ts <- ts(c(rep(data[Deaths_cumsum != 0, Deaths_cumsum][1], 6 - length(data_ts)),
                    data_ts))
  }
  
  # Perform exponential smoothing forecasting with specific model choices
  pred <- es_old(data_ts,
                 model = c("MMN", "MMdN"),
                 ic = "AICc",
                 h = n_ahead,
                 loss = "MSE",
                 interval = "likelihood",
                 level = 0.90,
                 silent = "all")
  
  return(pred)
  
}
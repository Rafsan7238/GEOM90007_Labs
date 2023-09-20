# Function to read COVID-19 testing data from the Worldometer source
read_data_worldometer <- function() {
  
  # Send an HTTP GET request to the Worldometer API
  data_res <- httr::GET(url = "https://covid19-server.chrismichael.now.sh/api/v1/AllReports")
  
  # Parse the JSON response from the API
  data_res_j <- jsonlite::fromJSON(rawToChar(data_res$content))
  
  # Extract the relevant table from the JSON data and convert it to a data table
  data_res_dt <- as.data.table(data_res_j$reports$table[[1]][[2]])
  
  # Convert TotalTests and Tests_1M_Pop columns from character to integer by removing commas
  data_res_dt[, TotalTests := as.integer(gsub(pattern = ",", replacement = "", TotalTests))]
  data_res_dt[, Tests_1M_Pop := as.integer(gsub(pattern = ",", replacement = "", Tests_1M_Pop))]
  
  # Handle specific country name changes
  data_res_dt[.("USA"), on = .(Country), Country := "US"]
  data_res_dt[.("UK"), on = .(Country), Country := "United Kingdom"]
  data_res_dt[.("S. Korea"), on = .(Country), Country := "Korea, South"]
  data_res_dt[.("UAE"), on = .(Country), Country := "United Arab Emirates"]
  data_res_dt[.("Taiwan"), on = .(Country), Country := "Taiwan*"]
  
  # Return a subset of the data that excludes rows with missing "TotalTests" values
  return(data_res_dt[!is.na(TotalTests), .(Country, TotalTests, Tests_1M_Pop)])
  
}
# Function to read COVID-19 data from the data source
read_data_covid19datahub <- function() {
  
  # Fetch COVID-19 data from the data source (covid19datahub) with verbosity turned off
  x <- covid19(verbose = FALSE)
  
  # Convert the fetched data to a data table
  x <- as.data.table(x)
  
  # Rename the "administrative_area_level_1" column to "Country"
  x[.("United States"), on = .(administrative_area_level_1), administrative_area_level_1 := "US"]
  setnames(x, "administrative_area_level_1", "Country")
  
  # Create a subset of the data containing only the latest date's information for each country
  x_max <- copy(x[, .SD[date == max(date)], by = .(Country)])
  
  # Rename the "tests" column to "TotalTests"
  setnames(x_max, "tests", "TotalTests")
  
  # Calculate and add a new column "Tests_1M_Pop" representing tests per 1 million population
  x_max[, Tests_1M_Pop := ceiling((TotalTests / population) * 1e6)]
  
  # Return a subset of the data that excludes rows with missing "TotalTests" values
  return(x_max[!is.na(TotalTests), .(Country, TotalTests, Tests_1M_Pop)])
  
}
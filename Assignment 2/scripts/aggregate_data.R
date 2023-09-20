# Define a function named "aggregate_data" that takes a data frame as input
aggregate_data <- function(data) {
  
  # Create a copy of the input data frame
  data_agg <- copy(data[, .(
    
    # Create a new column "Country" with the value "World"
    Country = "World",
    
    # Calculate the sum of the "Cases" column, ignoring missing values (NA)
    Cases = sum(Cases, na.rm = TRUE),
    
    # Calculate the sum of the "Deaths" column, ignoring missing values (NA)
    Deaths = sum(Deaths, na.rm = TRUE),
    
    # Calculate the sum of the "Cases_cumsum" column, ignoring missing values (NA)
    Cases_cumsum = sum(Cases_cumsum, na.rm = TRUE),
    
    # Calculate the sum of the "Deaths_cumsum" column, ignoring missing values (NA)
    Deaths_cumsum = sum(Deaths_cumsum, na.rm = TRUE),
    
    # Calculate the sum of the "Recovered_cumsum" column, ignoring missing values (NA)
    Recovered_cumsum = sum(Recovered_cumsum, na.rm = TRUE),
    
    # Calculate the sum of the "Active_cases_cumsum" column, ignoring missing values (NA)
    Active_cases_cumsum = sum(Active_cases_cumsum, na.rm = TRUE),
    
    # Calculate the sum of the "Population" column, ignoring missing values (NA)
    Population = sum(Population, na.rm = TRUE)
  ), 
  
  # Group the data by the "DateRep" column
  by = .(DateRep)])
  
  # Return the aggregated data frame
  return(data_agg)
}
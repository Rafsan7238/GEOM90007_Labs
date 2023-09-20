# Function to read population data from a CSV file
read_populations <- function() {
  
  # Read population data from a CSV file
  data_pop <- fread("data/country_population.csv")
  
  # Return the population data as a data table
  return(data_pop)
  
}
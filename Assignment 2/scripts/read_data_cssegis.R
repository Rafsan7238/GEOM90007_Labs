# Read data from JHU source CSSE ARCGIS -----
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
source("scripts/read_data_covid19datahub.R")

# Function to read COVID-19 data for confirmed cases
read_data_cases <- function() {
  
  # Read the CSV file containing confirmed cases data
  data_confirmed <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  # Rename the second column to "Country"
  setnames(data_confirmed, colnames(data_confirmed)[2], "Country")
  
  # Reshape the data by melting it
  data_confirmed_melt <- melt(data_confirmed,
                              id.vars = c(2),
                              measure.vars = 5:length(colnames(data_confirmed)),
                              variable.factor = FALSE,
                              variable.name = "DateRep",
                              value.name = "Cases_cumsum"
                              )
  
  # Convert the "DateRep" column to a proper date format
  data_confirmed_melt[, DateRep := lubridate::mdy(DateRep)]
  
  # Aggregate data by summing up cases for each country on each date
  data_confirmed_melt_agg <- copy(data_confirmed_melt[,
                                                      .(Cases_cumsum = sum(Cases_cumsum, na.rm = TRUE)),
                                                      by = .(Country, DateRep)])
  
  # Calculate new cases per day
  setorder(data_confirmed_melt_agg, Country, DateRep)
  data_confirmed_melt_agg[, Cases := c(.SD[1, Cases_cumsum],
                                       diff(Cases_cumsum, lag = 1, differences = 1)
                                      ),
                         by = .(Country)]
  
  # Ensure that negative cases are set to zero
  data_confirmed_melt_agg[Cases < 0, Cases := 0]
  
  return(data_confirmed_melt_agg)
  
}

# Function to read COVID-19 data for deaths
read_data_deaths <- function() {
  
  # Read the CSV file containing deaths data
  data_deaths <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  
  # Rename the second column to "Country"
  setnames(data_deaths, colnames(data_deaths)[2], "Country")
  
  # Reshape the data by melting it
  data_deaths_melt <- melt(data_deaths,
                           id.vars = c(2),
                           measure.vars = 5:length(colnames(data_deaths)),
                           variable.factor = FALSE,
                           variable.name = "DateRep",
                           value.name = "Deaths_cumsum"
                          )
  
  # Convert the "DateRep" column to a proper date format
  data_deaths_melt[, DateRep := lubridate::mdy(DateRep)]
  
  # Aggregate data by summing up deaths for each country on each date
  data_deaths_melt_agg <- copy(data_deaths_melt[,
                                                .(Deaths_cumsum = sum(Deaths_cumsum, na.rm = TRUE)),
                                                by = .(Country, DateRep)])
  
  # Calculate new deaths per day
  setorder(data_deaths_melt_agg, Country, DateRep)
  data_deaths_melt_agg[, Deaths := c(.SD[1, Deaths_cumsum],
                                     diff(Deaths_cumsum, lag = 1, differences = 1)
                                    ),
                       by = .(Country)]
  
  # Ensure that negative deaths are set to zero
  data_deaths_melt_agg[Deaths < 0, Deaths := 0]
  
  return(data_deaths_melt_agg)
  
}

# Function to read COVID-19 data for recoveries
read_data_recovered <- function() {
  
  # Read the CSV file containing recovered cases data
  data_recovered <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  
  # Rename the second column to "Country"
  setnames(data_recovered, colnames(data_recovered)[2], "Country")
  
  # Reshape the data by melting it
  data_recovered_melt <- melt(data_recovered,
                              id.vars = c(1, 2, 3, 4),
                              measure.vars = 5:length(colnames(data_recovered)),
                              variable.factor = FALSE,
                              variable.name = "DateRep",
                              value.name = "Recovered_cumsum",
                              na.rm = TRUE
                              )
  
  # Convert the "DateRep" column to a proper date format
  data_recovered_melt[, DateRep := lubridate::mdy(DateRep)]
  
  # Aggregate data by summing up recoveries for each country on each date
  data_recovered_melt_agg <- copy(data_recovered_melt[,
                                                      .(Recovered_cumsum = sum(Recovered_cumsum, na.rm = TRUE),
                                                        lat = data.table::first(Lat),
                                                        lon = data.table::first(Long)
                                                      ),
                                                      by = .(Country, DateRep)])
  
  # Calculate new recoveries per day
  setorder(data_recovered_melt_agg, Country, DateRep)
  data_recovered_melt_agg[, Recovered := c(.SD[1, Recovered_cumsum],
                                           diff(Recovered_cumsum, lag = 1, differences = 1)
                                          ),
                         by = .(Country)]
  
  # Ensure that negative recoveries are set to zero
  data_recovered_melt_agg[Recovered < 0, Recovered := 0]
  
  return(data_recovered_melt_agg)
  
}

# Function to join all COVID-19 data sources into one
join_all_corona_data <- function() {
  
  # Copy data for confirmed cases, deaths, recoveries, and tests
  data_all <- copy(read_data_cases())
  data_deaths <- copy(read_data_deaths())
  data_recov <- copy(read_data_recovered())
  data_tests <- copy(read_data_covid19datahub())
  
  # Join deaths data
  data_all[data_deaths,
           on = .(Country, DateRep),
           (c("Deaths_cumsum", "Deaths")) := .(i.Deaths_cumsum, i.Deaths)]
  
  # Join recoveries data
  data_all[data_recov,
           on = .(Country, DateRep),
           (c("Recovered_cumsum", "Recovered",
              "lat", "lon")) := .(i.Recovered_cumsum, i.Recovered,
                                  i.lat, i.lon)]
  
  # Handle specific country name changes
  data_all[.("Burma"), on = .(Country), Country := "Myanmar"]
  data_all[.("West Bank and Gaza"), on = .(Country), Country := "Palestine"]
  
  # Join tests data
  data_all[data_tests,
           on = .(Country),
           (c("TotalTests", "Tests_1M_Pop")) := 
             .(i.TotalTests, i.Tests_1M_Pop)]
  
  # Compute the number of active cases cumulatively
  data_all[, Active_cases_cumsum := Cases_cumsum - Deaths_cumsum - Recovered_cumsum]
  
  # Ensure that missing values are set to zero
  data_all[is.na(Recovered_cumsum), Recovered_cumsum := 0]
  data_all[is.na(Recovered), Recovered := 0]
  data_all[is.na(Active_cases_cumsum), Active_cases_cumsum := 0]
  
  return(data_all)
  
}

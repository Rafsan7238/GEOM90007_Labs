# Server function
function(input, output, session) {
  
  # Load scripts/modules
  # reading data
  source("scripts/read_data_cssegis.R")
  source("scripts/aggregate_data.R")
  source("scripts/read_populations.R")
  
  # forecasting
  source("scripts/forecasting.R")
  
  # clustering
  source("scripts/clustering_trajectories.R")
  
  # ---- Menu ----
 
  # Render the sidebar menu
  output$Side_dash <- renderMenu({
    sidebarMenu(
      id = "sideBar_Menu",  # Set the menu ID
      menuItem(
        "Forecasting",  # Menu item label
        icon = icon("chart-line"),  # Icon for the menu item
        tabName = "corTab",  # Tab name to navigate to
        startExpanded = FALSE,  # Whether the menu item is expanded by default
        selected = TRUE  # Whether the menu item is selected by default
      ),
      menuItem(
        "Comparisons", icon = icon("chart-bar"), tabName = "compareTab",
        startExpanded = FALSE, selected = FALSE
      ),
      menuItem(
        "Trajectory Clusters", icon = icon("diagram-project"), tabName = "trajectoryTab",
        startExpanded = FALSE, selected = FALSE
      ),
      menuItem(
        "Global Statistics", icon = icon("globe"), tabName = "worldTab",
        startExpanded = FALSE, selected = FALSE
      )
    )
  })
  
  observe({
    # Parse query parameters from the URL
    query <- parseQueryString(session$clientData$url_search)
    
    # Combine parameter names and values into a single string
    query1 <- paste(names(query), query, sep = "=", collapse = ", ")
    
    # Check the value of the 'tab' query parameter and update the selected tab accordingly
    if (query1 == "tab=compareTab") {
      updateTabItems(session, inputId = "sideBar_Menu", selected = "compareTab")
    } else if (query1 == "tab=worldTab") {
      updateTabItems(session, inputId = "sideBar_Menu", selected = "worldTab")
    } else if (query1 == "tab=trajectoryTab") {
      updateTabItems(session, inputId = "sideBar_Menu", selected = "trajectoryTab")
    }
  })
  
  # ---- Data Load ----
  
  # Read and process COVID-19 data
  data_corona <- reactive({
    
    # Join COVID-19 data from various sources
    data_res <- join_all_corona_data()
    
    # Read population data
    data_pop <- read_populations()
    
    # Merge population data with COVID-19 data based on the 'Country' column
    data_res[data_pop, on = .(Country), Population := i.Population]
    
    # Return the processed COVID-19 data
    data_res
    
  })
  
  # Render the date of the last data update as HTML
  output$text_date_update <- renderUI({
    
    # Find the maximum date from the COVID-19 data
    max_date <- max(data_corona()$DateRep)
    
    # Format the date as "9 March 2023"
    formatted_date <- format(max_date, format = "%e %B %Y")
    
    # Create HTML content to display the formatted date
    tags$html(tags$p(tags$b("Data last updated on: "), formatted_date))
    
  })
  
  # ---- Preference Selector ----
  
  # Country selector UI element
  output$selector_country <- renderUI({
    
    # Create a pickerInput widget
    pickerInput(
      inputId = "country",  # Input ID for the selector
      label = "Pick a country:",  # Label displayed above the selector
      choices = data_corona()[, unique(Country)],  # List of country choices
      selected = "US",  # Default selected country
      options = list(
        `live-search` = TRUE,  # Enable live search while typing
        style = "btn-info",  # Apply the "btn-info" Bootstrap style
        maxOptions = 7  # Maximum number of visible options
      )
    )
    
  })
  
  # N days forecast slider UI element
  output$slider_n_days_forec <- renderUI({
    
    # Create a sliderInput widget
    sliderInput(
      inputId = "n_days_forec",  # Input ID for the slider
      label = "Set how many days ahead to create a forecast for:",  # Label displayed above the slider
      min = 1,  # Minimum slider value
      max = 30,  # Maximum slider value
      value = 7  # Default slider value
    )
    
  })
  
  # ---- Data Table ----
  
  # Calculate the latest statistics for each country
  data_countries_stats <- reactive({
    
    # Make a copy of the COVID-19 data
    data_res <- copy(data_corona())
    
    # Create a subset of data for the latest date for each country
    data_res_latest <- copy(data_res[,
                                     .SD[DateRep == max(DateRep)],
                                     by = .(Country)]
    )
    
    # Order the data by Active_cases_cumsum in descending order
    setorder(data_res_latest, -Active_cases_cumsum)
    
    # Return the resulting data frame with the latest statistics
    data_res_latest
    
  })
  
  # Render a data table of statistics for the most infected countries
  output$dt_countries_cases <- renderDataTable({
    
    # Make a copy of the latest statistics data
    data_res_latest <- copy(data_countries_stats())
    
    # Create a data table using the selected columns
    DT::datatable(
      data_res_latest[, .(
        Country,
        'Total Cases' = Cases_cumsum,
        'Total Deaths' = Deaths_cumsum,
        'Active Cases' = Active_cases_cumsum,
        'New Cases' = Cases,
        'Active Cases/Million' = ceiling((Active_cases_cumsum / Population) * 1e6)
      )],
      selection = "single",  # Enable row selection
      class = "compact",  # Apply compact styling
      extensions = c('Buttons', 'Scroller'),  # Enable additional extensions
      options = list(
        pageLength = 5,  # Number of rows per page
        dom = 'Bfrtip',  # Layout options for DataTables
        deferRender = TRUE,  # Defer rendering for large datasets
        scrollY = 120,  # Vertical scrolling height
        scroller = TRUE,  # Enable DataTables Scroller extension
        buttons = c("csv", "excel"),  # Export buttons for CSV and Excel
        scrollX = TRUE  # Enable horizontal scrolling
      )
    )
    
  })
  
  # ---- Country Specific Stats ----
  
  # Subset the COVID-19 data by a selected country
  data_country <- reactive({
    
    shiny::req(input$country)  # Ensure that 'input$country' is available and not NULL
    
    # Make a copy of the COVID-19 data
    data_res <- copy(data_corona())
    
    # Subset the data based on the selected country using data.table syntax
    data_res <- data_res[.(input$country), on = .(Country)]
    
    # Return the resulting data frame with data for the selected country
    data_res
    
  })
  
  # Render value boxes displaying COVID-19 statistics
  
  # Value box for total confirmed cases
  output$valuebox_total_cases <- renderValueBox({
    valueBox(
      format(data_country()[.N, Cases_cumsum], nsmall = 1, big.mark = ","), # Use thousands separator
      "Total Confirmed Cases",  # Title describing the statistic
      icon = icon("ambulance"),  # Icon associated with the statistic
      color = "orange"  # Color of the value box
    )
  })
  
  # Value box for total confirmed deaths
  output$valuebox_total_deaths <- renderValueBox({
    valueBox(
      format(data_country()[.N, Deaths_cumsum], nsmall = 1, big.mark = ","),
      "Total Confirmed Deaths",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  # Value box for the death rate
  output$valuebox_death_rate <- renderValueBox({
    valueBox(
      paste0(
        round(data_country()[.N, Deaths_cumsum] / data_country()[.N, Cases_cumsum], digits = 4) * 100, "%"
      ),
      "Death Rate",  
      icon = icon("exclamation-triangle"),  
      color = "maroon"  
    )
  })
  
  # Value box for total confirmed active cases
  output$valuebox_total_active <- renderValueBox({
    valueBox(
      format(data_country()[.N, Active_cases_cumsum], nsmall = 1, big.mark = ","),
      "Total Confirmed Active Cases",
      icon = icon("hospital-alt"),
      color = "yellow"
    )
  })
  
  # Value box for total active cases per 1 million population
  output$valuebox_active_per_mil <- renderValueBox({
    valueBox(
      format(
        data_country()[.N, as.integer(ceiling((Active_cases_cumsum / Population) * 1e6))],
        nsmall = 1,
        big.mark = ","
      ),
      "Total Active Cases Per 1 Million Population", 
      icon = icon("male"),  
      color = "purple"  
    )
  })
  
  # ---- Chart Panel ----
  
  # Render Dygraph charts displaying COVID-19 data for the selected country
  
  # Render Dygraph chart for cumulative cases
  output$dygraph_country_cases <- renderDygraph({
    shiny::req(input$country)  # Ensure 'input$country' is available
    
    dygraph(data_country()[, .(DateRep, 'Cumulative Cases' = Cases_cumsum)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_country()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "orange", strokeColor = "#222d32") %>% # Choose color for slider
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE, # Don't use markers
                colors = c("orange")) %>% # Color for graph
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) # Legend settings
  })
  
  # Render Dygraph chart for new cases
  output$dygraph_country_new_cases <- renderDygraph({
    shiny::req(input$country) 
    
    dygraph(data_country()[, .(DateRep, Cases)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_country()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "orange", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("orange")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  })
  
  # Render Dygraph chart for new deaths
  output$dygraph_country_new_deaths <- renderDygraph({
    shiny::req(input$country)
    
    dygraph(data_country()[, .(DateRep, Deaths)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_country()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "red", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("red")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  })
  
  # Render Dygraph chart for cumulative deaths
  output$dygraph_country_deaths <- renderDygraph({
    shiny::req(input$country) 
    
    dygraph(data_country()[, .(DateRep, 'Cumulative Deaths' = Deaths_cumsum)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_country()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "red", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("red")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  })

  # ---- Country Specific Forecast ----
  
  # Define a reactive function to calculate cumulative case forecasts
  data_cases_cumsum_forec <- reactive({
    
    # Ensure that 'country' and 'n_days_forec' inputs are provided
    req(input$country, input$n_days_forec)
    
    # Create a copy of the 'data_country' dataset
    data_res <- copy(data_country())
    
    # Perform cumulative case forecasting based on the input 'n_days_forec'
    data_forec <- forec_cases_cumsum(data_res, input$n_days_forec)
    
    # Generate a new dataset that combines the original data and the forecasted values
    data_res <- rbindlist(list(
      data_res, # Original data
      data.table(
        DateRep = seq.Date(data_res[, max(DateRep) + 1],  # Generate a sequence of dates
                           data_res[, max(DateRep) + input$n_days_forec],
                           by = 1),
        Cases_cumsum_mean = round(data_forec$forecast, digits = 0),  # Rounded forecasted cases
        Cases_cumsum_lwr = floor(data_forec$forecast),  # Floor of forecasted cases
        Cases_cumsum_upr = data_forec$upper  # Upper limit of forecasted cases
      )
    ), fill = TRUE, use.names = TRUE)
    
    # Add a 'Model' column to the resulting dataset to store the forecasting model information
    data_res[, Model := data_forec$model]
    
    # Return the final dataset
    data_res
    
  })
  
  # Show forecasted cases of the selected country
  output$dygraph_country_cases_forecast <- renderDygraph({
    
    # Ensure that 'country' and 'n_days_forec' inputs are provided
    shiny::req(input$country, input$n_days_forec)
    
    # Create a copy of the 'data_cases_cumsum_forec' dataset
    data_res <- copy(data_cases_cumsum_forec())
    
    # Generate a Dygraph plot to visualize the forecasted cumulative cases
    dygraph(data_res[, .(DateRep, 'Cumulative Cases' = Cases_cumsum,
                         Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
            main = "") %>%
      dySeries('Cumulative Cases') %>%  # Plot the original cumulative cases
      dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
               label = "Forecasted Cumulative Cases") %>%  # Plot forecasted cases
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - input$n_days_forec - 10],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "orange", strokeColor = "#222d32") %>%  # Add range selector
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("orange", "green")) %>%  # Define Dygraph options
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%  # Add highlight effect
      dyEvent(data_res[is.na(Cases_cumsum_mean), max(DateRep)],
              "Forecasting Origin", labelLoc = "bottom") %>%  # Highlight the forecasting origin
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  # Customize legend
    
  })
  
  # ---- World Stats ----
  
  # Create a reactive expression to process and aggregate world COVID-19 data.
  data_world <- reactive({
    
    # Retrieve and process COVID-19 data using the aggregate_data function
    data_res <- aggregate_data(data_corona())
    
    # Return the processed data
    data_res
    
  })
  
  # Value boxes of world statistics
  output$valuebox_total_cases_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Cases_cumsum], nsmall=1, big.mark=","),
      "Total Confirmed Cases",
      icon = icon("ambulance"),
      color = "orange"
    )
    
  })
  
  output$valuebox_total_deaths_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Deaths_cumsum], nsmall=1, big.mark=","),
      "Total Confirmed Deaths",
      icon = icon("skull"),
      color = "red"
    )
    
  })
  
  output$valuebox_death_rate_world <- renderValueBox({
    
    valueBox(
      paste0(round(data_world()[.N, Deaths_cumsum]/data_world()[.N, Cases_cumsum], digits = 4)*100, "%"),
      "Death Rate",
      icon = icon("exclamation-triangle"),
      color = "maroon"
    )
    
  })
  
  output$valuebox_total_active_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N, Active_cases_cumsum], nsmall=1, big.mark=","),
      "Total Confirmed Active Cases",
      icon = icon("hospital-alt"),
      color = "yellow"
      )
    
  })
  
  output$valuebox_active_per_mil_world <- renderValueBox({
    
    valueBox(
      format(data_world()[.N,
                          as.integer(ceiling((Active_cases_cumsum / Population) * 1e6))],
             nsmall=1, big.mark=","),
      "Total Active Cases Per 1 Million Population",
      icon = icon("male"),
      color = "purple"
    )
    
  })
  
  # ---- World Graphs ----
  # Render dygraphs to display world COVID-19 data for cases and deaths.
  
  # Render dygraph for cumulative cases of the world
  output$dygraph_world_cases <- renderDygraph({
    
    dygraph(data_world()[, .(DateRep, 'Cumulative Cases' = Cases_cumsum)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_world()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "orange", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("orange")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Render dygraph for new cases of the world
  output$dygraph_world_new_cases <- renderDygraph({
    
    dygraph(data_world()[, .(DateRep, Cases)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_world()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "orange", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("orange")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Render dygraph for cumulative deaths of the world
  output$dygraph_world_deaths <- renderDygraph({
    
    dygraph(data_world()[, .(DateRep, 'Cumulative Deaths' = Deaths_cumsum)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_world()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "red", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("red")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # Render dygraph for new deaths of the world
  output$dygraph_world_new_deaths <- renderDygraph({
    
    dygraph(data_world()[, .(DateRep, Deaths)],
            main = "") %>%
      dyRangeSelector(dateWindow = c(data_world()[, min(DateRep)], data_country()[, max(DateRep) + 1]),
                      fillColor = "red", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = FALSE,
                colors = c("red")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # ---- World Forecast ----
  
  # Create a reactive expression to calculate and prepare data for world cumulative cases forecast.
  data_cases_cumsum_forec_world <- reactive({
    
    # Make a copy of the world data
    data_res <- copy(data_world())
    
    # Calculate cumulative cases forecast for the next 10 days
    data_forec <- forec_cases_cumsum(data_res, 10)
    
    # Add the forecasted data to the existing dataset
    data_res <- rbindlist(list(
      data_res,
      data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                    data_res[, max(DateRep) + 10],
                                    by = 1),
                 Cases_cumsum_mean = round(data_forec$forecast, digits = 0),
                 Cases_cumsum_lwr = floor(data_forec$forecast),
                 Cases_cumsum_upr = data_forec$upper
      )
    ), fill = TRUE, use.names = TRUE
    )
    
    # Add a 'Model' column to indicate the forecast model
    data_res[, Model := data_forec$model]
    
    # Return the prepared dataset
    data_res
    
  })
  
  # Render a dygraph to display world cumulative cases and forecast
  output$dygraph_world_cases_forecast <- renderDygraph({
    
    # Retrieve the prepared data for world cumulative cases and forecast
    data_res <- copy(data_cases_cumsum_forec_world())
    
    # Create a dygraph with specified settings
    dygraph(data_res[, .(DateRep, 'Cumulative Cases' = Cases_cumsum,
                         Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
            main = "") %>%
      dySeries('Cumulative Cases') %>%
      dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
               label = "Forecasted Cumulative Cases") %>%
      dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 20],
                                     data_res[, max(DateRep) + 1]),
                      fillColor = "orange", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = TRUE, fillAlpha = 0.4,
                drawPoints = TRUE, pointSize = 3,
                pointShape = "circle",
                colors = c("orange", "green")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyEvent(data_res[is.na(Cases_cumsum_mean), max(DateRep)],
              "Forecasting Origin", labelLoc = "bottom") %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
    
  })
  
  # ---- Country Comparisons ----
  
  # Country Selector
  output$picker_countries_selector <- renderUI({
    
    # Create a country selector using shinyWidgets::pickerInput
    shinyWidgets::pickerInput(
      inputId = "countries_selector",          # Specify the input ID for the selector
      label = NULL,                           # Set the label to NULL
      choices = data_corona()[, unique(Country)],  # Get unique country choices from data_corona()
      selected = c("US", "France", "United Kingdom", "India"),  # Set initial selected countries
      multiple = TRUE,                        # Allow multiple selections
      options = list(
        `actions-box` = TRUE,                 # Enable actions box for additional functionality
        style = "btn-info",                   # Set the style of the selector to btn-info
        `live-search` = TRUE,                 # Enable live search for quicker country selection
        size = 8                              # Set the size of the selector
      )
    )
    
  })
  
  # Comparison Statistic Selector
  data_corona_all_new_stats <- reactive({
    
    # Create a copy of the original data
    data_res <- copy(data_corona())
    
    # Calculate and add new statistics columns
    data_res[, ('Death Rate (%)') := round((Deaths_cumsum / Cases_cumsum) * 100, 2)]
    data_res[, ('Total Active Cases Per 1 Million Population') := ceiling((Active_cases_cumsum / Population) * 1e6)]
    data_res[, ('Total Deaths Per 1 Million Population') := ceiling((Deaths_cumsum / Population) * 1e6)]
    data_res[, ('Total Cases Per 1 Million Population') := ceiling((Cases_cumsum / Population) * 1e6)]
    data_res[, ('New Cases Per 1 Million Population') := ceiling((Cases / Population) * 1e6)]
    data_res[, ('New Deaths Per 1 Million Population') := ceiling((Deaths / Population) * 1e6)]

    # Remove unnecessary columns
    data_res[, c('TotalTests', 'Tests_1M_Pop', 'Population', 'lat', 'lon') := NULL]
    
    # Return the modified data
    data_res
    
  })
  
  # Statistic picker
  output$picker_stats_selector <- renderUI({
    
    # Set custom choices
    custom_choices <- c(
      "Cumulative Cases" = "Cases_cumsum",
      "New Cases" = "Cases",
      "Total Cases Per 1 Million Population",
      "New Cases Per 1 Million Population",
      
      "Cumulative Active Cases" = "Active_cases_cumsum",
      "Total Active Cases Per 1 Million Population",
      
      "Cumulative Deaths" = "Deaths_cumsum",
      "New Deaths" = "Deaths",
      "Death Rate (%)",
      "Total Deaths Per 1 Million Population",
      "New Deaths Per 1 Million Population"
    )
    
    # Create a copy of the modified data
    data_res <- copy(data_corona_all_new_stats())
    
    # Create a picker input for selecting statistics
    shinyWidgets::pickerInput(
      inputId = "stats_selector",              # Specify the input ID for the selector
      label = NULL,                           # Set the label to NULL
      choices = custom_choices,  # Set the choices
      selected = 'Cases_cumsum',  # Set the default selected statistic
      multiple = FALSE,                       # Allow only single selection
      options = list(
        style = "btn-danger",                 # Set the style of the selector to btn-danger
        `live-search` = TRUE,                 # Enable live search for quicker statistic selection
        size = 8                               # Set the size of the selector
      )
    )
    
  })
  
  # Log-scale switch
  output$switch_log_scale_compareTab <- renderUI({
    
    # Create a material switch input for toggling log scale on the Y-axis
    materialSwitch(
      inputId = "log_scale_compareTab",               # Specify the input ID for the switch
      label = "Use log scale on Y axis?",             # Set the label text
      value = FALSE,                                 # Set the initial switch value to "FALSE"
      status = "danger"                              # Set the status (color) of the switch to "danger"
    )
    
  })
  
  # Reactive function to filter and select data
  data_country_stat_selected <- reactive({
    
    # Ensure that both countries_selector and stats_selector inputs are available
    shiny::req(input$countries_selector, input$stats_selector)
    
    # Make a copy of the modified data
    data_res <- copy(data_corona_all_new_stats())
    
    # Filter and select specific columns based on user inputs
    data_picked <- copy(data_res[.(input$countries_selector),
                                 on = .(Country),
                                 .SD,
                                 .SDcols = c("Country",
                                             "DateRep",
                                             input$stats_selector)
    ])
    
    # Return the filtered and selected data
    data_picked
    
  })
  
  # ---- Comparison Graphs ----
  
  # Render a numeric input for selecting days since the first 'N' cases
  output$selector_cases_since_first_n <- renderUI({
    
    numericInput(
      inputId = "cases_since_first_n",               # Specify the input ID for the numeric input
      label = "How many cases before comparing?",  # Set the label text
      value = 100,                                   # Set the initial value to 100
      min = 1,                                       # Set the minimum allowed value to 1
      max = 1e6,                                     # Set the maximum allowed value to 1 million
      step = 2                                      # Set the step size for incrementing/decrementing
    )
    
  })
  
  # Render a numeric input for selecting days since the first 'N' deaths
  output$selector_deaths_since_first_n <- renderUI({
    
    numericInput(
      inputId = "deaths_since_first_n",              # Specify the input ID for the numeric input
      label = "How many deaths before comparing?", # Set the label text
      value = 20,                                   # Set the initial value to 20
      min = 1,                                      # Set the minimum allowed value to 1
      max = 1e6,                                    # Set the maximum allowed value to 1 million
      step = 2                                     # Set the step size for incrementing/decrementing
    )
    
  })
  
  # Cases
  data_country_stat_by_first_cases <- reactive({
    
    # Ensure that necessary inputs are available
    shiny::req(input$countries_selector, input$stats_selector, input$cases_since_first_n)
    
    # Make a copy of the modified data
    data_res <- copy(data_corona_all_new_stats())
    
    # Filter data for cases greater than or equal to the specified threshold
    data_res_cases <- copy(data_res[,
                                    .SD[DateRep >= .SD[Cases_cumsum >= input$cases_since_first_n,
                                                       min(DateRep, na.rm = TRUE)]],
                                    by = .(Country)
    ])
    
    # Order the filtered data
    setorder(data_res_cases, Country, DateRep)
    
    # Create a "Days Since First N Cases" column
    data_res_cases[, (paste0("Days Since First ", input$cases_since_first_n, " Cases")) := 1:.N, by = .(Country)]
    
    # Subset data based on selected parameters (Countries, Days, Statistic)
    data_res_cases <- copy(data_res_cases[.(input$countries_selector),
                                          on = .(Country),
                                          .SD,
                                          .SDcols = c("Country",
                                                      paste0("Days Since First ", input$cases_since_first_n, " Cases"),
                                                      input$stats_selector)
    ])
    
    # Return the filtered and subsetted data
    data_res_cases
    
  })
  
  # Deaths
  data_country_stat_by_first_deaths <- reactive({
    
    # Ensure that necessary inputs are available
    shiny::req(input$countries_selector, input$stats_selector, input$deaths_since_first_n)
    
    # Make a copy of the modified data
    data_res <- copy(data_corona_all_new_stats())
    
    # Filter data for deaths greater than or equal to the specified threshold
    data_res_deaths <- copy(data_res[,
                                     .SD[DateRep >= .SD[Deaths_cumsum >= input$deaths_since_first_n,
                                                        min(DateRep)]],
                                     by = .(Country)
    ])
    
    # Order the filtered data
    setorder(data_res_deaths, Country, DateRep)
    
    # Create a "Days Since First N Deaths" column
    data_res_deaths[, (paste0("Days Since First ", input$deaths_since_first_n, " Deaths")) := 1:.N, by = .(Country)]
    
    # Subset data based on selected parameters (Countries, Days, Statistic)
    data_res_deaths <- copy(data_res_deaths[.(input$countries_selector),
                                            on = .(Country),
                                            .SD,
                                            .SDcols = c("Country",
                                                        paste0("Days Since First ", input$deaths_since_first_n, " Deaths"),
                                                        input$stats_selector)
    ])
    
    # Return the filtered and subsetted data
    data_res_deaths
    
  })

  # Render a dygraph displaying statistics since the first occurrence for selected countries.
  output$dygraph_countries_stats_since_first <- renderDygraph({
    
    # Ensure that required input variables are provided
    shiny::req(input$countries_selector, input$stats_selector,
               input$cases_since_first_n, input$deaths_since_first_n)
    
    # Check if the selected statistic is related to cases
    if (grepl(pattern = "case", x = input$stats_selector) |
        grepl(pattern = "Case", x = input$stats_selector)) {
      
      # Reshape data for cases since the first occurrence
      data_res <- dcast(data_country_stat_by_first_cases(),
                        get(paste0("Days Since First ", input$cases_since_first_n, " Cases")) ~ Country,
                        value.var = input$stats_selector)
      
      # Rename the first column
      setnames(data_res, colnames(data_res)[1], paste0("Days Since First ", input$cases_since_first_n, " Cases"))
      
    } else if (grepl(pattern = "eath", x = input$stats_selector)) {
      
      # Reshape data for deaths since the first occurrence
      data_res <- dcast(data_country_stat_by_first_deaths(),
                        get(paste0("Days Since First ", input$deaths_since_first_n, " Deaths")) ~ Country,
                        value.var = input$stats_selector)
      
      # Rename the first column
      setnames(data_res, colnames(data_res)[1], paste0("Days Since First ", input$deaths_since_first_n, " Deaths"))
      
    }
    
    # Create the dygraph with specified settings
    dygraph(data_res,
            main = "") %>%
      dyRangeSelector(fillColor = "#5bc0de", strokeColor = "#222d32") %>%
      dyAxis("x", label = colnames(data_res)[1]) %>%
      dyOptions(
        fillGraph = TRUE, fillAlpha = 0.2,
        strokeWidth = 3,
        drawPoints = FALSE,
        logscale = if(input$log_scale_compareTab) {TRUE} else {NULL},
        colors = RColorBrewer::brewer.pal(nrow(data_res), "Dark2")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4)
      ) %>%
      dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  })
  
  # Render UI elements for providing information in the "Comparing Trajectories" tab.
  output$info_comparing_trajectories <- renderUI({
    
    # Create HTML tags for displaying information
    tags$html(
      tags$p("In this tab, you can", tags$b("compare countries' trajectories "), "based on a selected statistic."),
      tags$p("You can vary multiple settings such as:",
             tags$li("countries to compare"),
             tags$li("the statistic to base the compare on"),
             tags$li("how many cases or deaths should the countries have before comparing them")),
      tags$p(tags$b("Please note:"),
             tags$li("The cases parameter will only work if you select a statistic with the word 'case' in it"),
             tags$li("The deaths parameter will only work if you select a statistic with the word 'death' in it"))
    )
    
  })
  
  
  
  # ---- Trajectory Clusters ----

  # info
  # Render the UI for displaying information about clustering trajectories.
  output$info_clustering_trajectories <- renderUI({
    
    # Create an HTML structure with paragraphs and lists to provide information.
    tags$html(
      
      # Information about clustering countries' trajectories using HAC and DTW.
      tags$p("In this tab, you can", tags$b("cluster countries' trajectories"), 
             "using ", tags$b("Hierarchical Agglomerative Clustering"), 
             " method with ", tags$b("Dynamic Time Warping (DTW)"), 
             " distance measure."),
      
      # Explanation of various customizable settings.
      tags$p("You can vary multiple settings such as: ",
             tags$li("the statistic of COVID-19 spread"),
             tags$li("your preferred country"),
             tags$li("the starting points for trajectories"),
             tags$li("the number of clustered countries"),
             tags$li("the number of clusters"),
             tags$li("the order of trajectory smoothing"),
             tags$li("normalization of time series")),
      
      # Description of the cluster analysis results.
      tags$p("For cluster analysis results, there are 4 plots:",
             tags$li(tags$b("Grid of clusters"), " for viewing similar trajectories."),
             tags$li(tags$b("Focus plot of the selected cluster"), 
                     "for interactive country-wise analysis."),
             tags$li(tags$b("Dendrogram of hierarchical clustering"), 
                     "for detailed view into connections between countries."),
             tags$li(tags$b("Multidimensional Scaling (MDS) plot"), 
                     "for better imagination of countries' similarities in the lower dimensions.")),
      
      # Important notes for users.
      tags$p(tags$b("Please note:"),
             tags$li("The cases parameter will only work if you select a statistic with the word 'case' in it"),
             tags$li("The deaths parameter will only work if you select a statistic with the word 'death' in it"))
    )
    
  })
  
  # ---- Data for Clustering ----
  
  # Define a reactive function to preprocess the 'data_corona' dataset for analysis.
  data_corona_all_time_series <- reactive({
    
    # Make a copy of the 'data_corona' dataset.
    data_res <- copy(data_corona())
    
    # Filter out rows where population is less than 800,000.
    data_res <- copy(data_res[Population > 8e5])
    
    # Calculate and add a new column for 'New Cases Per 1 Million Population.'
    data_res[, ('New Cases Per 1 Million Population') := ceiling((Cases / Population) * 1e6)]
    
    # Calculate and add a new column for 'New Deaths Per 1 Million Population.'
    data_res[, ('New Deaths Per 1 Million Population') := ceiling((Deaths / Population) * 1e6)]
    
    # Calculate and add a new column for 'Death Rate (%).'
    data_res[, ('Death Rate (%)') := round((Deaths_cumsum / Cases_cumsum) * 100, 2)]
    
    # Calculate and add a new column for 'Total Active Cases Per 1 Million Population.'
    data_res[, ('Total Active Cases Per 1 Million Population') := ceiling((Active_cases_cumsum / Population) * 1e6)]
    
    # Calculate and add a new column for 'Total Deaths Per 1 Million Population.'
    data_res[, ('Total Deaths Per 1 Million Population') := ceiling((Deaths_cumsum / Population) * 1e6)]
    
    # Calculate and add a new column for 'Total Cases Per 1 Million Population.'
    data_res[, ('Total Cases Per 1 Million Population') := ceiling((Cases_cumsum / Population) * 1e6)]
    
    # Remove unnecessary columns from the dataset.
    data_res[, Population := NULL]
    data_res[, TotalTests := NULL]
    data_res[, Tests_1M_Pop := NULL]
    data_res[, lat := NULL]
    data_res[, lon := NULL]
    
    # Return the preprocessed dataset.
    data_res
  })
  
  # ---- Parameter Selection ----
  
  # Render the UI for selecting a statistic for clustering.
  output$picker_stat_selector_clust <- renderUI({
    
    # Make a copy of the preprocessed 'data_corona_all_time_series' dataset.
    data_res <- copy(data_corona_all_time_series())
    
    # Define custom choices for the statistic selection.
    custom_choices <- c(
      "Cumulative Cases" = "Cases_cumsum",
      "New Cases" = "Cases",
      "Total Cases Per 1 Million Population",
      "New Cases Per 1 Million Population",
      
      "Cumulative Active Cases" = "Active_cases_cumsum",
      "Total Active Cases Per 1 Million Population",
      
      "Cumulative Deaths" = "Deaths_cumsum",
      "New Deaths" = "Deaths",
      "Death Rate (%)",
      "Total Deaths Per 1 Million Population",
      "New Deaths Per 1 Million Population"
    )
    
    # Create a picker input widget for statistic selection.
    shinyWidgets::pickerInput(
      inputId = "stat_selector_clust",
      label = NULL, 
      choices = custom_choices,
      selected = 'Total Active Cases Per 1 Million Population',
      multiple = FALSE,  # Allow only single selection.
      options = list(
        style = "btn-info",
        `live-search` = TRUE,  # Enable live search for choices.
        size = 8  # Set the size of the widget.
      )
    )
  })
  
  # Render the UI for selecting the number of cases before clustering.
  output$selector_cases_since_first_n_clust <- renderUI({
    
    # Create a numeric input widget for selecting the number of cases.
    numericInput(inputId = "cases_since_first_n_clust",
                 label = "How many cases before clustering?",
                 value = 100,
                 min = 1,
                 max = 1e6,
                 step = 2
    )
    
  })
  
  # Render the UI for selecting the number of deaths before clustering.
  output$selector_deaths_since_first_n_clust <- renderUI({
    
    # Create a numeric input widget for selecting the number of deaths.
    numericInput(inputId = "deaths_since_first_n_clust",
                 label = "How many deaths before clustering?",
                 value = 20,
                 min = 1,
                 max = 1e6,
                 step = 2
    )
    
  })
  
  # Render the UI for selecting the top N countries from the selected statistic for clustering.
  output$selector_top_n_countries_clust <- renderUI({
    
    # Ensure that the 'stat_selector_clust' input is available before proceeding.
    shiny::req(input$stat_selector_clust)
    
    # Make a copy of the preprocessed 'data_corona_all_time_series' dataset.
    data_res <- copy(data_corona_all_time_series())
    
    # Create a numeric input widget for selecting the number of top countries to cluster.
    numericInput(inputId = "top_n_countries_clust",
                 label = paste0("Select number of top countries to cluster:"),
                 value = 60,
                 min = 1,
                 max = data_res[, uniqueN(Country)],  # Set the maximum value based on unique countries.
                 step = 2
    )
    
  })
  
  # Render the UI for selecting the number of clusters for DTW clustering.
  output$selector_n_clusters_dtw <- renderUI({
    
    # Create a numeric input widget for selecting the number of clusters.
    numericInput(inputId = "n_clusters_dtw",
                 label = "Select number of clusters:",
                 value = 9,
                 min = 2,
                 max = 40,
                 step = 1
    )
    
  })
  
  # ---- Data Selection for Cluster ----
  
  # Reactive function to process data for Cases based on user input.
  data_country_stat_by_first_cases_clust <- reactive({
    
    # Ensure that the required input values are available before proceeding.
    shiny::req(input$stat_selector_clust, input$top_n_countries_clust, input$cases_since_first_n_clust)
    
    # Make a copy of the preprocessed 'data_corona_all_time_series' dataset.
    data_res <- copy(data_corona_all_time_series())
    
    # Filter data for cases greater than or equal to the specified threshold.
    data_res_cases <- copy(data_res[, .SD[DateRep >= .SD[Cases_cumsum >= input$cases_since_first_n_clust,
                                                         min(DateRep, na.rm = TRUE)],
    ],
    by = .(Country)])
    
    # Set the order of the data frame by 'Country' and 'DateRep'.
    setorder(data_res_cases, Country, DateRep)
    
    # Create a 'Days Since First Cases' column for each country.
    data_res_cases[, (paste0("Days Since First ", input$cases_since_first_n_clust, " Cases")) := 1:.N,
                   by = .(Country)]
    
    # Select the top N countries based on the selected statistic.
    data_res_order <- copy(data_res_cases[, .SD[DateRep == max(DateRep),
                                                get(input$stat_selector_clust)],
                                          by = .(Country)])
    
    # Rename the 'V1' column to the selected statistic.
    setnames(data_res_order, "V1", input$stat_selector_clust)
    
    # Set the order of the data frame based on the selected statistic.
    setorderv(data_res_order, input$stat_selector_clust, -1)
    
    # Subset the data based on selected parameters (Countries, etc.).
    data_res_cases_sub <- copy(data_res_cases[.(data_res_order[1:input$top_n_countries_clust][!is.na(Country), Country]),
                                              on = .(Country),
                                              .SD,
                                              .SDcols = c("Country",
                                                          paste0("Days Since First ", input$cases_since_first_n_clust, " Cases"),
                                                          input$stat_selector_clust)
    ])
    
    # Check if "Slovakia" data is missing and add it if necessary.
    if (sum(grepl(pattern = "Slovakia", x = data_res_cases_sub[, unique(Country)])) == 0) {
      
      data_res_cases_sub <- rbindlist(list(data_res_cases_sub,
                                           data_res_cases[.("Slovakia"),
                                                          on = .(Country),
                                                          .SD,
                                                          .SDcols = c("Country",
                                                                      paste0("Days Since First ", input$cases_since_first_n_clust, " Cases"),
                                                                      input$stat_selector_clust)
                                           ])
      )
      
    }
    
    # Create time series data with same-length sequences for countries.
    data_res_trajectories <- dcast(data_res_cases_sub,
                                   get(paste0("Days Since First ", input$cases_since_first_n_clust, " Cases")) ~ Country,
                                   value.var = input$stat_selector_clust)
    
    # Rename the columns to match the 'Days Since First Cases' values.
    setnames(data_res_trajectories,
             colnames(data_res_trajectories)[1],
             paste0("Days Since First ", input$cases_since_first_n_clust, " Cases"))
    
    # Return the processed data.
    data_res_trajectories
    
  })
  
  # Reactive function to process data for Deaths based on user input.
  data_country_stat_by_first_deaths_clust <- reactive({
    
    # Ensure that the required input values are available before proceeding.
    shiny::req(input$stat_selector_clust, input$top_n_countries_clust, input$deaths_since_first_n_clust)
    
    # Make a copy of the preprocessed 'data_corona_all_time_series' dataset.
    data_res <- copy(data_corona_all_time_series())
    
    # Filter data for deaths greater than or equal to the specified threshold.
    data_res_deaths <- copy(data_res[, .SD[DateRep >= .SD[Deaths_cumsum >= input$deaths_since_first_n_clust,
                                                          min(DateRep)]],
                                     by = .(Country)])
    
    # Set the order of the data frame by 'Country' and 'DateRep'.
    setorder(data_res_deaths, Country, DateRep)
    
    # Create a 'Days Since First Deaths' column for each country.
    data_res_deaths[, (paste0("Days Since First ", input$deaths_since_first_n_clust, " Deaths")) := 1:.N,
                    by = .(Country)]
    
    # Select the top N countries based on the selected statistic.
    data_res_order <- copy(data_res_deaths[, .SD[DateRep == max(DateRep),
                                                 get(input$stat_selector_clust)],
                                           by = .(Country)
    ])
    
    # Rename the 'V1' column to the selected statistic.
    setnames(data_res_order, "V1", input$stat_selector_clust)
    
    # Set the order of the data frame based on the selected statistic.
    setorderv(data_res_order, input$stat_selector_clust, -1)
    
    # Subset the data based on selected parameters (Countries, etc.).
    data_res_deaths_sub <- copy(data_res_deaths[.(data_res_order[1:input$top_n_countries_clust][!is.na(Country), Country]),
                                                on = .(Country),
                                                .SD,
                                                .SDcols = c("Country",
                                                            paste0("Days Since First ", input$deaths_since_first_n_clust, " Deaths"),
                                                            input$stat_selector_clust)
    ])
    
    # Check if "Slovakia" data is missing and add it if necessary.
    if (sum(grepl(pattern = "Slovakia", x = data_res_deaths_sub[, unique(Country)])) == 0) {
      
      data_res_deaths_sub <- rbindlist(list(data_res_deaths_sub,
                                            data_res_deaths[.("Slovakia"),
                                                            on = .(Country),
                                                            .SD,
                                                            .SDcols = c("Country",
                                                                        paste0("Days Since First ", input$deaths_since_first_n_clust, " Deaths"),
                                                                        input$stat_selector_clust)
                                            ])
      )
      
    }
    
    # Create time series data with same-length sequences for countries.
    data_res_trajectories <- dcast(data_res_deaths_sub,
                                   get(paste0("Days Since First ", input$deaths_since_first_n_clust, " Deaths")) ~ Country,
                                   value.var = input$stat_selector_clust)
    
    # Rename the columns to match the 'Days Since First Deaths' values.
    setnames(data_res_trajectories,
             colnames(data_res_trajectories)[1],
             paste0("Days Since First ", input$deaths_since_first_n_clust, " Deaths")
    )
    
    # Return the processed data.
    data_res_trajectories
    
  })
  
  # ---- Fine Tuning ----
  
  # Render the UI for selecting the order of trajectory smoothing.
  output$selector_sma_order <- renderUI({
    
    # Create a numeric input widget for selecting the order of trajectory smoothing.
    numericInput(inputId = "sma_order",
                 label = "Select order of trajectory smoothing:",
                 value = 3,
                 min = 1,
                 max = 7,
                 step = 1
    )
    
  })
  
  # Render the UI for the normalization switch.
  output$switch_normalization <- renderUI({
    
    # Create a material switch for toggling normalization.
    materialSwitch(
      inputId = "normalization",
      label = "Normalize by z-score?", 
      value = TRUE,
      status = "info"
    )
    
  })
  
  # Render the UI for the log-scale switch.
  output$switch_log_scale <- renderUI({
    
    # Create a material switch for toggling log scale on the Y axis.
    materialSwitch(
      inputId = "log_scale",
      label = "Log scale on Y axis?",
      value = TRUE,
      status = "info"
    )
    
  })
  
  # ---- Data Preparation ----
  
  # Reactive function to prepare data for clustering trajectories.
  data_for_clustering_trajectories <- reactive({
    
    # Ensure that the required input values are available before proceeding.
    shiny::req(input$stat_selector_clust, input$sma_order)
    
    # Initialize 'data_res' based on the selected statistic (Cases or Deaths).
    if (grepl(pattern = "case", x = input$stat_selector_clust) |
        grepl(pattern = "Case", x = input$stat_selector_clust)) {
      
      data_res <- copy(data_country_stat_by_first_cases_clust())
      
    } else if (grepl(pattern = "eath", x = input$stat_selector_clust)) {
      
      data_res <- copy(data_country_stat_by_first_deaths_clust())
      
    }
    
    # Save the dimensions and NA counts for later use.
    n_col <- ncol(data_res)
    n_row <- nrow(data_res)
    n_row_na <- rowSums(data_res[, lapply(.SD, is.na)])
    n_col_na <- colSums(data_res[, lapply(.SD, is.na)])
    
    # Remove all NA rows and columns.
    if (length(which(n_row_na %in% n_col)) != 0) {
      data_res <- copy(data_res[-which(n_row_na == n_col)])
    }
    
    if (length(which(n_col_na %in% n_row)) != 0) {
      data_res <- copy(data_res[, -which(n_col_na %in% n_row), with = FALSE])
    }
    
    # Compute Simple Moving Average (SMA) for every Country.
    data_res[, (colnames(data_res)[-1]) := lapply(.SD, function(i) c(rep(NA, input$sma_order - 1),
                                                                     ceiling(repr_sma(i, input$sma_order))
    )
    ),
    .SDcols = colnames(data_res)[-1]]
    
    # Return the prepared data for clustering.
    data_res
    
  })
  
  # Reactive function for clustering trajectories.
  clustering_result_trajectories <- reactive({
    
    # Ensure that the number of clusters input is available before proceeding.
    shiny::req(input$n_clusters_dtw)
    
    # Retrieve the prepared data for clustering.
    data_res <- copy(data_for_clustering_trajectories())
    
    # Perform clustering on the trajectories data.
    clust_res <- cluster_trajectories(data_res, input$n_clusters_dtw, input$normalization)
    
    # Return the clustering results.
    clust_res
    
  })
  
  # ---- Country Selector for Focused Analysis ----
  
  # Render the UI for selecting a country to determine the cluster it belongs to.
  output$picker_country_clust <- renderUI({
    
    # Make a copy of the clustering results.
    clust_res <- copy(clustering_result_trajectories())
    
    # Create a data table to associate countries with their respective clusters.
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    # Create a picker input widget for selecting a country.
    shinyWidgets::pickerInput(
      inputId = "country_clust",
      label = "Choose your preferred country for focused analysis:",
      choices = data_clust_id[, Country],
      selected = 'Australia',  # Default selection.
      multiple = FALSE,  # Allow only single selection.
      options = list(
        style = "btn-info",
        `live-search` = TRUE,  # Enable live search for choices.
        size = 8  # Set the size of the widget.
      )
    )
    
  })
  
  # Render the UI for displaying information about the cluster that the selected country belongs to.
  output$text_inwhich_cluster_country <- renderUI({
    
    # Make a copy of the clustering results.
    clust_res <- copy(clustering_result_trajectories())
    
    # Create a data table to associate countries with their respective clusters.
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    # Ensure that the 'country_clust' input is available before proceeding.
    shiny::req(input$country_clust)
    
    # Display cluster information for the selected country.
    tags$html(tags$h4(tags$b(paste0("Cluster: ", data_clust_id[.(input$country_clust),
                                                               on = .(Country),
                                                               Cluster]))
    )
    )
    
  })
  
  # ---- Cluster Graphs ----
  
  # Reactive function to prepare data for plotting clustered trajectories.
  data_plot_clusters_trajectories <- reactive({
    
    # Retrieve the prepared data for clustering and clustering results.
    data_res <- copy(data_for_clustering_trajectories())
    clust_res <- clustering_result_trajectories()
    
    # Prepare time series data for plotting.
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    # Melt the data to a long format for plotting.
    data_plot <- melt(data_res,
                      id.vars = colnames(data_res)[1],
                      variable.name = "Country",
                      variable.factor = FALSE,
                      value.name = input$stat_selector_clust,
                      value.factor = FALSE
    )
    
    # Merge the data with cluster information based on country.
    data_plot <- copy(data_plot[.(data_clust_id$Country), on = .(Country)])
    
    # Assign cluster values to the data based on country.
    data_plot[data_clust_id,
              on = .(Country),
              Cluster := i.Cluster]
    
    # Initialize 'centers' for centroids and set cluster colors.
    if (!input$normalization) {
      
      centroids <- lapply(1:length(clust_res@centroids), function(i) c(rep(NA, input$sma_order - 1),
                                                                       clust_res@centroids[[i]])
      )
      
      # Prepare centroids data.
      centers <- as.data.table(reshape2::melt(centroids))
      setnames(centers, "L1", "Cluster")
      centers[, (colnames(data_res)[1]) := 1:.N,
              by = .(Cluster)]
      setnames(centers, "value", input$stat_selector_clust)
      centers[, Country := as.character(Cluster)]
      
    }
    
    # Set colors for clusters using rainbow_hcl.
    data_clust_colors <- data.table(Cluster = 1:max(clust_res@cluster),
                                    Color = colorspace::rainbow_hcl(max(clust_res@cluster), c = 90, l = 50)
    )
    
    # Return a list containing data, centers (if not normalized), and cluster colors.
    list(data = data_plot,
         centers = if (!input$normalization) { centers } else { NULL },
         colors = data_clust_colors
    )
    
  })
  
  # Render the plot of clusters' members.
  output$plot_clusters_trajectories <- renderPlot({
    
    # Retrieve the data for plotting clustered trajectories.
    data_clust_res <- data_plot_clusters_trajectories()
    
    # Define custom theme settings.
    theme_my <- theme(panel.border = element_rect(fill = NA,
                                                  colour = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.minor = element_line(colour = "grey85"),
                      panel.grid.major = element_line(colour = "grey85"),
                      panel.grid.major.x = element_line(colour = "grey85"),
                      axis.text = element_text(size = 13, face = "bold"),
                      axis.title = element_text(size = 16, face = "bold"),
                      strip.text = element_text(size = 16, face = "bold"),
                      strip.background = element_rect(colour = "black"),
                      legend.position = "none")
    
    # Create the plot based on user settings.
    if (!input$normalization) {
      
      # Plot clustered trajectories with centers.
      gg <- ggplot(data_clust_res$data,
                   aes(get(colnames(data_clust_res$data)[1]),
                       get(input$stat_selector_clust),
                       group = Country,
                       color = as.factor(Cluster))) +
        facet_wrap(~Cluster,
                   ncol = ceiling(data_clust_res$data[, sqrt(uniqueN(Cluster))]),
                   scales = "free") +
        geom_line(alpha = 0.6, size = 0.4) +
        scale_color_manual(values = data_clust_res$colors$Color) +
        labs(x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        guides(color = FALSE) +
        theme_my
      
      # Apply log scale on the Y-axis if selected.
      if (input$log_scale) gg <- gg + scale_y_continuous(trans = 'log10')
      
    } else {
      
      # Plot clustered trajectories without centers.
      gg <- ggplot(data_clust_res$data,
                   aes(get(colnames(data_clust_res$data)[1]),
                       get(input$stat_selector_clust),
                       group = Country,
                       color = as.factor(Cluster))) +
        facet_wrap(~Cluster,
                   ncol = ceiling(data_clust_res$data[, sqrt(uniqueN(Cluster))]),
                   scales = "free") +
        geom_line(alpha = 0.6, size = 0.4) +
        labs(x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        theme_my
      
      # Apply log scale on the Y-axis if selected.
      if (input$log_scale) gg <- gg + scale_y_continuous(trans = 'log10')
    }
    
    # Return the generated plot.
    gg
  })
  
  # ---- Focus Graphs ----
  
  # Generate UI element for cluster focus picker
  output$picker_cluster_focus <- renderUI({
    
    # Copy the clustering results
    clust_res <- copy(clustering_result_trajectories())
    
    # Create a data table with Cluster and Country information
    data_clust_id <- data.table(Cluster = clust_res@cluster,
                                Country = names(clust_res@cluster))
    
    # Require the number of clusters and selected country
    shiny::req(input$n_clusters_dtw, input$country_clust)
    
    # Create a picker input for cluster focus selection
    shinyWidgets::pickerInput(inputId = 'cluster_focus',
                              label = 'Choose cluster for visualization:',
                              choices = c(1:input$n_clusters_dtw),
                              selected = data_clust_id[.(input$country_clust),
                                                       on = .(Country),
                                                       Cluster],
                              options = list(`style` = "btn-primary",
                                             `live-search` = TRUE,
                                             size = 5)
    )
  })
  
  # Render a plotly plot for focused cluster visualization
  output$plotly_focus_cluster <- renderPlotly({
    
    # Require the selected cluster
    shiny::req(input$cluster_focus)
    
    # Retrieve the data for plotting clustered trajectories
    data_clust_res <- data_plot_clusters_trajectories()
    
    # Selected cluster
    k <- input$cluster_focus
    
    # Define custom theme settings
    theme_my <- theme(panel.border = element_rect(fill = NA,
                                                  colour = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.minor = element_line(colour = "grey85"),
                      panel.grid.major = element_line(colour = "grey85"),
                      panel.grid.major.x = element_line(colour = "grey85"),
                      axis.text = element_text(size = 10, face = "bold"),
                      axis.title = element_text(size = 11, face = "bold"),
                      plot.title = element_text(size = 14, face = "bold"),
                      legend.position = "none"
    )
    
    # Create the plot based on user settings.
    if (!input$normalization) {
      
      # Create the ggplot for the selected cluster
      gg_clust <- ggplot(data_clust_res$data[Cluster == k],
                         aes(get(colnames(data_clust_res$data)[1]),
                             get(input$stat_selector_clust),
                             group = Country,
                             color = Country,
                             text = paste('</br>', colnames(data_clust_res$data)[1], ": ", get(colnames(data_clust_res$data)[1]),
                                          '</br>', input$stat_selector_clust, ": ", round(get(input$stat_selector_clust), 2),
                                          '</br>Country: ', Country, sep = "")
                             )) +
        geom_line(alpha = 0.6, size = 0.6)

      # Set plot labels and theme
      gg_clust <- gg_clust +
        labs(x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        theme_my
      
      # Apply log scale on the Y-axis if selected
      if (input$log_scale) gg_clust <- gg_clust + scale_y_continuous(trans = 'log10')
      
    } else {
      
      # Create the ggplot for the selected cluster
      gg_clust <- ggplot(data_clust_res$data[Cluster == k],
                         aes(get(colnames(data_clust_res$data)[1]),
                             get(input$stat_selector_clust),
                             group = Country,
                             color = Country,
                             text = paste('</br>', colnames(data_clust_res$data)[1], ": ", get(colnames(data_clust_res$data)[1]),
                                          '</br>', input$stat_selector_clust, ": ", round(get(input$stat_selector_clust), 2),
                                          '</br>Country: ', Country, sep = "")
                         )) +
        geom_line(alpha = 0.6, size = 0.6)

      # Set plot labels and theme
      gg_clust <- gg_clust +
        labs(x = colnames(data_clust_res$data)[1],
             y = input$stat_selector_clust) +
        theme_my
      
      # Apply log scale on the Y-axis if selected
      if (input$log_scale) gg_clust <- gg_clust + scale_y_continuous(trans = 'log10')
      
    }
    
    # Convert the ggplot to plotly and specify tooltips
    ggplotly(gg_clust, tooltip = "text")
    
  })
  
  # ---- Dendrogram ----
  # Render dendrogram plot for trajectory clustering
  output$plot_clusters_trajectories_dendogram <- renderPlot({
    
    # Retrieve clustering results for trajectories
    clust_res <- clustering_result_trajectories()
    
    # Convert clustering results to a dendrogram
    dend <- as.dendrogram(clust_res)
    
    # Customize the dendrogram appearance
    dend <- dend %>%
      color_branches(k = input$n_clusters_dtw) %>%
      color_labels(k = input$n_clusters_dtw) %>%
      set("branches_lwd", 0.2) %>%
      set("labels_cex", 0.8)
    
    # Convert the dendrogram to a ggplot object
    ggd1 <- as.ggdend(dend)

    # Create a horizontal dendrogram plot using ggplot
    gg_dendo <- ggplot(ggd1, horiz = TRUE) 
    
    gg_dendo  # Return the ggplot object
    
  })
  
  # Render MDS (Multi-Dimensional Scaling) plot for DTW
  output$plot_scatter_mds_trajectories <- renderPlot({
    
    # Retrieve clustering results for trajectories
    clust_res <- clustering_result_trajectories()
    
    # Perform classical MDS
    mds_classical <- cmdscale(clust_res@distmat, eig = FALSE, k = 2)
    
    # Create a data table for plotting
    data_plot <- data.table(
      mds_classical,
      Country = row.names(mds_classical),
      Cluster = clust_res@cluster
    )
    
    # Create a scatter plot using ggplot
    gg_scatter <- ggplot(data_plot, aes(
      x = get("V1"),
      y = get("V2"),
      label = Country,
      color = as.factor(Cluster)
    )) +
      geom_label_repel(
        size = 5,
        alpha = 0.95,
        segment.alpha = 0.35,
        label.r = 0.1,
        box.padding = 0.25,
        label.padding = 0.3,
        label.size = 0.35,
        max.iter = 2500
      ) +
      scale_color_manual(
        values = colorspace::rainbow_hcl(
          input$n_clusters_dtw,
          c = 90,
          l = 50
        )
      ) +
      labs(
        x = NULL,
        y = NULL,
        color = NULL
      ) +
      guides(color = FALSE) +
      theme_bw()
    
    gg_scatter  # Return the ggplot object
    
  })
  
}
## Libraries ##
library(shiny)           # For creating interactive web applications
library(ggplot2)         # For data visualization
library(leaflet.extras)  # Enhancing Leaflet maps in Shiny apps
library(plotly)          # Creating interactive plots and charts
library(leaflet)         # For interactive maps in Shiny
library(dplyr)           # Data manipulation and filtering
library(highcharter)     # Creating Highcharts in Shiny apps
library(shinydashboard)  # Building dashboard-style Shiny apps
library(shinyWidgets)    # Adding custom widgets to Shiny apps
library(DT)              # Creating interactive data tables in Shiny
library(ggrepel)         # Controlling text labels in ggplot2 plots
library(jsonlite)        # Working with JSON data in R

# Read the data
# Import data and merge two datasets together to get the final hotel data
hotelData1 <- read.csv("Dataset/listings2.csv")   # Read data from "listings2.csv"
hotelData2 <- read.csv("Dataset/listings.csv")    # Read data from "listings.csv"
hotelData <- merge(hotelData2, hotelData1, by.x = "id", by.y = "id")  # Merge data from two sources based on the 'id' column

# Define the user interface (UI) for the Shiny app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    *{
    font-family:'Hiragino';
    }
    
    .shiny-numeric-input .up, .shiny-numeric-input .down {
      font-size: 50px;  /* Adjust size as needed */
    }
    .info-img {
      width: 350px;
      height: 350px;
      object-fit: cover;
      margin: auto;
      display: block;
    }
    
  "))
  ),
  
  fluidRow(
    # Hotels Filter box
    box(
      title = "Filter Your Choice", solidHeader = TRUE, width = 3, height = "450px",
      numericInput("numberOfHotelPeople", "Number of People:", value = 1, min = 1, max = 16, step = 1),  # Numeric input for the number of people
      sliderInput("hotelPriceRange", "Price Range", min = 0, max = 15000, value = c(200, 3000), step = 100, pre = "$", sep = ",", animate = FALSE),  # Slider input for price range
      selectInput(
        "roomType",
        label = "Room Type",
        choices = c(
          "All Types" = "All Type",
          "Entire Home/Apartment" = "Entire home/apt",
          "Private Room" = "Private room",
          "Shared Room" = "Shared room",
          "Hotel Room" = "Hotel room"
        ),
        selected = "All Type"
      )
    ),
    
    # Avaiblity analysis for hotel
    box(
      width = 4, height = "500px",
      title = "Price Analysis by Room Type", solidHeader = TRUE,
      plotlyOutput("availability_analysis")
    ),
    box(
      width = 5, height = "500px",
      title = "Price vs. Reviews", solidHeader = TRUE,
      plotlyOutput("price_review_scatter")
    )
  ),
  
  fluidRow(
    # Hotel map and table
    tabBox(
      title = NULL, width = 12, height = "600px",
      id = "map",
      tabPanel("AirBNB Map", icon = icon("location-dot"), leafletOutput("hotelMap", height = "550px")),  # Tab for displaying a map
      tabPanel("Top 10 Accommodations", icon = icon("circle-info"), dataTableOutput("hotelRate"))  # Tab for displaying the top 10 accommodations
    )
  ),
  fluidRow( # Info section
    # Room
    box(
      title = "Accommodation Information", solidHeader = TRUE, status = "primary", width = 6,
      htmlOutput("hotelRoomInfo")
    ),
    # Host 
    box(
      title = "Host Information", solidHeader = TRUE, status = "primary", width = 6,
      htmlOutput("hotelHostInfo")
    )
  )
)

# Define the server for the Shiny app
server <- function(input, output, session) {
  
  # Initialize the reactive value for hotel ID
  hotelIDrecord <- reactiveValues(x = 0)
  
  # Generate the filtered hotel data based on user input
  hotelDataFiltering <- reactive({
    filter(
      hotelData,
      if (input$roomType == "All Type") TRUE else room_type.x == input$roomType,
      input$hotelPriceRange[1] <= price.y & input$hotelPriceRange[2] >= price.y,
      input$numberOfHotelPeople >= (accommodates - 1) & input$numberOfHotelPeople <= (accommodates + 1)
    )
  })
  
  output$hotelHostInfo <- renderText({
    hotelHostData <- hotelData %>% filter(id == hotelIDrecord$x)
    
    if (nrow(hotelHostData) == 0) {
      return("Please select an accommodation from the map or table")
    }
    else {
      # Ensure NA or empty values are displayed as "None" or a placeholder
      host_url <- ifelse(is.na(hotelHostData$host_url) | hotelHostData$host_url == "", "URL not available", hotelHostData$host_url)
      host_picture_url <- ifelse(is.na(hotelHostData$host_picture_url) | hotelHostData$host_picture_url == "", "path/to/placeholder/image.jpg", hotelHostData$host_picture_url)
      host_name <- ifelse(is.na(hotelHostData$host_name.x) | hotelHostData$host_name.x == "", "None", hotelHostData$host_name.x)
      host_since <- ifelse(is.na(hotelHostData$host_since) | hotelHostData$host_since == "", "None", hotelHostData$host_since)
      host_location <- ifelse(is.na(hotelHostData$host_location) | hotelHostData$host_location == "", "None", hotelHostData$host_location)
      host_about <- ifelse(is.na(hotelHostData$host_about) | hotelHostData$host_about == "", "None", hotelHostData$host_about)
      host_response_time <- ifelse(is.na(hotelHostData$host_response_time) | hotelHostData$host_response_time == "", "None", hotelHostData$host_response_time)
      host_response_rate <- ifelse(is.na(hotelHostData$host_response_rate) | hotelHostData$host_response_rate == "", "None", hotelHostData$host_response_rate)
      host_acceptance_rate <- ifelse(is.na(hotelHostData$host_acceptance_rate) | hotelHostData$host_acceptance_rate == "", "None", hotelHostData$host_acceptance_rate)
      number_of_reviews <- ifelse(is.na(hotelHostData$number_of_reviews) | hotelHostData$number_of_reviews == "", "None", hotelHostData$number_of_reviews)
      review_scores_rating <- ifelse(is.na(hotelHostData$review_scores_rating) | hotelHostData$review_scores_rating == "", "None", hotelHostData$review_scores_rating)
      first_review <- ifelse(is.na(hotelHostData$first_review) | hotelHostData$first_review == "", "None", hotelHostData$first_review)
      last_review <- ifelse(is.na(hotelHostData$last_review) | hotelHostData$last_review == "", "None", hotelHostData$last_review)
      review_scores_cleanliness <- ifelse(is.na(hotelHostData$review_scores_cleanliness) | hotelHostData$review_scores_cleanliness == "", "None", hotelHostData$review_scores_cleanliness)
      review_scores_checkin <- ifelse(is.na(hotelHostData$review_scores_checkin) | hotelHostData$review_scores_checkin == "", "None", hotelHostData$review_scores_checkin)
      review_scores_communication <- ifelse(is.na(hotelHostData$review_scores_communication) | hotelHostData$review_scores_communication == "", "None", hotelHostData$review_scores_communication)
      review_scores_location <- ifelse(is.na(hotelHostData$review_scores_location) | hotelHostData$review_scores_location == "", "None", hotelHostData$review_scores_location)
      
      # HTML code to display the host info
      paste("
  <img src='", host_picture_url, "' class='info-img' alt='Image of the host'>
      <h4><a href='", host_url, "' target='_blank'>For more information, please visit the host profile on Airbnb</a></h4>
      <table class='table'>
        <tbody>
          <tr>
            <td><strong>Name</strong></td>
            <td>", host_name, "</td>
          </tr>
          <tr>
            <td><strong>Host Since</strong></td>
            <td>", host_since, "</td>
          </tr>
          <tr>
            <td><strong>Location</strong></td>
            <td>", host_location, "</td>
          </tr>
          <tr>
            <td><strong>Response Time</strong></td>
            <td>", host_response_time, "</td>
          </tr>
          <tr>
            <td><strong>Response Rate</strong></td>
            <td>", host_response_rate, "</td>
          </tr>
          <tr>
            <td><strong>Acceptance Rate</strong></td>
            <td>", host_acceptance_rate, "</td>
          </tr>
          <tr>
            <td><strong>Number of Reviews</strong></td>
            <td>", number_of_reviews, "</td>
          </tr>
          <tr>
            <td><strong>Review Scores Rating</strong></td>
            <td>", review_scores_rating, "</td>
          </tr>
          <tr>
            <td><strong>First Review</strong></td>
            <td>", first_review, "</td>
          </tr>
          <tr>
            <td><strong>Last Review</strong></td>
            <td>", last_review, "</td>
          </tr>
          <tr>
            <td><strong>Review Score (Cleanliness)</strong></td>
            <td>", review_scores_cleanliness, "</td>
          </tr>
          <tr>
            <td><strong>Review Score (Checkin)</strong></td>
            <td>", review_scores_checkin, "</td>
          </tr>
          <tr>
            <td><strong>Review Score (Communication)</strong></td>
            <td>", review_scores_communication, "</td>
          </tr>
          <tr>
            <td><strong>Review Score (Location)</strong></td>
            <td>", review_scores_location, "</td>
          </tr>
        </tbody>
      </table>"
      )
    }
  })
  
  # Create a text output to display the count of hotels found
  output$hotelCount <- renderText({
    hotelFilterData <- hotelDataFiltering()
    sumResult <- hotelFilterData %>%
      summarise(count = n(), .groups = "drop")
    paste("Find ", as.character(sumResult), " number of result")
  })
  
  # Create a reactive subset of data based on the number of people
  reactiveData <- reactive({
    subsetData <- hotelData[hotelData$accommodates == input$numberOfHotelPeople, ]
    return(subsetData)
  })
  
  # Bar graph for average price by room type
  output$availability_analysis <- renderPlotly({
    data_filtered <- hotelDataFiltering()
    
    # Group by room_type and calculate the average price
    avg_price_data <- data_filtered %>%
      group_by(room_type.x) %>%
      summarise(avg_price = mean(price.y, na.rm = TRUE))
    
    label <- function(){
      if(input$numberOfHotelPeople == 1){
        return("Person")
      }
      else{
        return("People")
      }
    }
    
    p <- ggplot(avg_price_data, aes(x = room_type.x, y = avg_price, fill = room_type.x)) +
      geom_bar(stat="identity") +
      labs(title = "",
           x = "Room Type",
           y = paste("Average Price for", input$numberOfHotelPeople, label(), "(AUD)")) +
      theme_minimal() +
      scale_fill_manual(values = c("Entire home/apt" = "#3498db", "Private room" = "#e74c3c", "Shared room" = "#9b59b6", "Hotel room" = "#f39c12", "Other" = "#2ecc71")) +
      theme(legend.title = element_blank(),
            legend.position = "none", # <- This line removes the legend
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank())
    
    p <- p + geom_text(aes(label = sprintf("$%.2f", avg_price)), nudge_y = 10, size = 3.5)
    p <- ggplotly(p, tooltip = "") %>%
      layout(
        font = list(family = "Hiragino")
      )
    
    return(p)
  })
  
  # Use proxy to listen to the hotel click marker event
  hotelLeafletProxy <- leafletProxy(mapId = "hotelMap", session)
  # When the mouse clicks the marker in the map, pass the ID to the reactive value
  observeEvent(input$hotelMap_marker_click, {
    clicked_point <- input$hotelMap_marker_click
    hotelIDrecord$x <- clicked_point$id
  })
  
  # Use observer event to listen to the table click event
  observeEvent(input$hotelRate_rows_selected, {
    # Get the clicked row number
    clicked_point <- input$hotelRate_rows_selected
    # Guard program
    if (is.null(clicked_point)) {
      return()
    }
    # Set the row's house ID to reactive value
    hotelRatedData <- head(select(arrange(hotelDataFiltering(), desc(reviews_per_month.y)), id, name.x, price.x, reviews_per_month.y), 10)
    hotelIDrecord$x <- hotelRatedData$id[clicked_point]
  })
  
  # DT to display the top 10 places in Melbourne
  output$hotelRate <- DT::renderDataTable({
    # Get the top 10 places based on reviews per month
    top_hotels <- head(arrange(hotelDataFiltering(), desc(reviews_per_month.y)), 10)
    
    # Select relevant columns for display
    selected_data <- select(top_hotels,name.x, price.x, reviews_per_month.y)
    
    # Define custom column headers
    column_headers <- c("Name", "Price (AUD)", "Avg. Reviews Per Month")
    
    # Create the DataTable with custom headers
    datatable(selected_data, 
              selection = "single",
              options = list(pageLength = 10, searching = FALSE, paging = FALSE, info = FALSE),
              colnames = column_headers)
  })
  
  # HTML to write the house information, and get the information detail by reading the reactive value
  output$hotelRoomInfo <- renderText({
    hotelRoomData <- hotelData %>% filter(id == hotelIDrecord$x)
    if (nrow(hotelRoomData) == 0) {
      paste("Please select an accommodation from the map or table")
    } else {
      amenities_string <- paste(as.character(unlist(hotelRoomData$amenities)), collapse = ", ")
      amenities_list <- fromJSON(amenities_string)
      amenities_string <- paste(paste0("- ", amenities_list), collapse = "<br>")
      
      paste("
  <img src='", hotelRoomData$picture_url, "' class='info-img' alt='Image of the house'>
<h4><a href=", hotelRoomData$listing_url, ">For more information, please visit the Airbnb page</a></h4>
      <table class='table'>
  <tbody>
  <tr>
    <td><b>Name</b></td>
    <td>", hotelRoomData$name.x, "</td>
  </tr>
  <tr>
    <td><b>Price Per Night (AUD)</b></td>
    <td>", hotelRoomData$price.x, "</td>
  </tr>
  <tr>
    <td><b>Room Type</b></td>
    <td>", hotelRoomData$room_type.x, "</td>
  </tr>
  <tr>
    <td><b>Accommodates</b></td>
    <td>", hotelRoomData$accommodates, "</td>
  </tr>
  <tr>
    <td><b>Bathrooms</b></td>
    <td>", hotelRoomData$bathrooms, "</td>
  </tr>
  <tr>
    <td><b>Bedrooms</b></td>
    <td>", hotelRoomData$bedrooms, "</td>
  </tr>
  <tr>
    <td><b>No. of Beds</b></td>
    <td>", hotelRoomData$beds, "</td>
  </tr>
  <tr>
    <td><b>Available Services</b></td>
    <td>", amenities_string, "</td>
  </tr>
</tbody>
</table>")
    }
  })
  
  # Output the map to contain the house location as markers
  output$hotelMap <- renderLeaflet({
    # Get the filtering data
    hotelFilterData <- hotelDataFiltering()
    # Set the label when the mouse goes to the marker
    labels <- sprintf(
      "<strong>%s</strong><br/><strong>Host Name:</strong> %s <br/><strong>Accommodates:</strong> %g <br/><strong>Bedroom:</strong> %g <br/><strong>Bathroom:</strong> %s <br/><strong>Price(AUD):</strong> %g  ",
      hotelFilterData$name.x, hotelFilterData$host_name.x, hotelFilterData$accommodates, hotelFilterData$bedrooms, hotelFilterData$bathrooms, hotelFilterData$price.y
    ) %>% lapply(htmltools::HTML)
    # Guard the program when there is no data
    if (nrow(hotelFilterData) == 0) {
      return()
    }
    # Use leaflet to generate the map
    m <- leaflet(hotelFilterData) %>%
      addProviderTiles(providers$OpenStreetMap) %>% # Use OpenStreetMap
      addAwesomeMarkers(~longitude.x, ~latitude.x,
                        layerId = ~id, group = "House",
                        icon = ~ awesomeIcons(
                          icon = "ios-home",
                          library = "ion",
                          markerColor = "darkred",
                          iconColor = "#fff"
                        ),
                        label = labels,
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "15px",
                          direction = "auto"
                        ),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction = JS("
                          function(cluster) {
                            var childCount = cluster.getChildCount();
                            var c = ' marker-cluster-';
                            if (childCount < 10) {
                              c += 'large';
                            } else if (childCount < 100) {
                              c += 'medium';
                            } else {
                              c += 'small';
                            }
                            return new L.DivIcon({
                              html: '<div style=\"\"><span>' + childCount + '</span></div>',
                              className: 'marker-cluster' + c,
                              iconSize: new L.Point(40, 40)
                            });
                          }
                        ")
                        )
      ) %>%
      setView(lng = 145, lat = -37.8136, zoom = 9.5)
    
    return(m)
  })
  
  # Scatter plot for Price vs Review
  output$price_review_scatter <- renderPlotly({
    data_filtered <- hotelDataFiltering()
    
    p <- ggplot(data_filtered, aes(x = review_scores_rating, y = price.y, color = room_type.x, text = name.x)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "",
           x = "Review Scores",
           y = "Price",
           color = "Room Type:") +
      theme_minimal() +
      scale_color_manual(values = c("Entire home/apt" = "#3498db", "Private room" = "#e74c3c", "Shared room" = "#9b59b6", "Hotel room" = "#f39c12", "Other" = "#2ecc71")) +
      theme(legend.title = element_text(face = "bold"),
            legend.position = "bottom",
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank()) 
    p <- ggplotly(p, tooltip = "") %>%
      layout(
        font = list(family = "Hiragino"),
        legend = list(font = list(size = 14))
      )
    
    return(p)
  })
  
}

shinyApp(ui, server)
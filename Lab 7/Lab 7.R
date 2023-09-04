library("shiny")
library("ggiraph")
library('leaflet')
library("ggplot2")
library("tidyr")
library("stringr")

auBirth <- read.csv("Births_summary_with_id.csv", stringsAsFactors = FALSE)

# Sort the data by Births in descending order
auBirth <- auBirth[order(auBirth$Region),]

hosp_data <- read.csv('Hospitals in Australia with childbirth stats-1.csv')

##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Births',
  h2('Births in Australia'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId='year',
        label='Year',
        choices=c('2017'='X2017',
                  '2018'='X2018',
                  '2019'='X2019',
                  '2020'='X2020'),
        selected='X2020'
      )
    ),
    mainPanel(
      girafeOutput('plot_births')
    )
  )
)

trends_tab <- tabPanel(
  title='Trends',
  h2('Births in Australia over the Years'),
  plotOutput('plot_trends')
)

hospitals_tab <- tabPanel(
  title='Hospitals',
  leafletOutput('map_hospitals', height=600)
)

ui <- navbarPage(
  title='Population Growth in Australia',
  births_tab,
  trends_tab,
  hospitals_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  output$plot_births <- renderGirafe({
    
    dynamic_title <- paste("Births by State in", gsub("X", "", input$year))
    
    p <- ggplot(auBirth, aes(x = Region, y = .data[[input$year]], fill = "#9325be", tooltip = paste(Region,': ',.data[[input$year]], sep = ""), data_id=Region)) +
    scale_fill_manual(values = "#9325be") +
    geom_bar_interactive(stat = "identity") +
    labs(title = dynamic_title,
          x = "State",
          y = "") +
    theme_minimal() +
    theme(axis.title.x = element_text(margin = margin(t = 20)),
          axis.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          panel.grid.major.x = element_blank(),
          legend.position = "none") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    girafe(ggobj=p, height_svg=4)
  })
  
  output$plot_trends <- renderPlot({
    # Create a custom color palette with 8 colors
    my_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                            "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
                            
    # Plot a line chart with additional customizations
    ggplot(auBirth_long_new, aes(x = Year, y = Births, group = id, color = Region)) +
      geom_line(linewidth = 1.5) +
      scale_color_manual(values = my_colors) +
      labs(title = "Birth Trends by Region in Australia (2017-2020)",
           x = "Year",
           y = "Number of Births") +
      scale_x_discrete(labels = function(x) as.numeric(gsub("X", "", x))) +  # Remove "X" prefix
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.background = element_rect(fill = "transparent"),  # Remove legend background
            legend.box.background = element_rect(color = "black"),  # Add border to legend box
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.background = element_rect(fill = "#f0f0f0"),  # Set panel background color
            panel.grid.major = element_line(color = "white"),  # Customize grid lines
            panel.grid.minor = element_line(color = "white")) +
      guides(color = guide_legend(override.aes = list(size = 2)))  # Adjust legend symbol size
  })
  
  output$map_hospitals <- renderLeaflet({
      filtered_hosp_data <- hosp_data[hosp_data$Childbirths.total != 0, ]
      
      leaflet(filtered_hosp_data) %>%
        addProviderTiles(providers$CartoDB) %>%
        addAwesomeMarkers(lng=~Longitude, lat=~Latitude,
                          icon=awesomeIcons(library='fa',
                                            icon='plus-square',
                                            markerColor='red',
                                            iconColor='#ffffff'), 
                          label=~Name)
  })
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)
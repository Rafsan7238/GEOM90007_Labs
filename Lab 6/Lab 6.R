library("shiny")

auBirth <- read.csv("Births_summary_with_id.csv", stringsAsFactors = FALSE)

# Sort the data by Births in descending order
auBirth <- auBirth[order(auBirth$Region),]


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
      plotOutput('plot_births')
    )
  )
)

ui <- navbarPage(
  title='Population Growth in Australia',
  births_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  output$plot_births <- renderPlot({
    
    dynamic_title <- paste("Births by State in", input$year)
    
    ggplot(auBirth, aes(x = Region, y = .data[[input$year]], fill = "#9325be")) +
    scale_fill_manual(values = "#9325be") +
    geom_bar(stat = "identity") +
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
  })
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)
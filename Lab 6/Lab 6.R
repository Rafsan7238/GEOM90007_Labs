library("shiny")

auBirth <- read.csv("Births_summary_with_id.csv", stringsAsFactors = FALSE)

# Convert data to long format for multiple data series (2017-2020)
auBirth_long <- pivot_longer(auBirth, cols = starts_with("X"), names_to = "Year", values_to = "Births")

# Remove X before years
auBirth_long$Year <- gsub("X", "", auBirth_long$Year)

# Sort the data by Births in descending order
auBirth_long <- auBirth_long[order(auBirth_long$Region),]


##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Births',
  h2('Births in Australia'),
  plotOutput('plot_births')
)

ui <- navbarPage(
  title='Population Growth in Australia',
  births_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)
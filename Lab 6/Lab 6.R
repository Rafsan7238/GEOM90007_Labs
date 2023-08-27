library("shiny")

##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Births',
  h2('Births in Australia'),
  plotOutput('plot_births')
)

ui <- navbarPage(
  title='Population growth in Australia',
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
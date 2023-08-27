library("shiny")

##################
# USER INTERFACE #
##################

births_tab <- tabPanel(
  title='Births',
  h2('Births in Australia'),
  plotOutput('plot_births')
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
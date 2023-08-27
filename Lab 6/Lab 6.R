library("shiny")

##################
# USER INTERFACE #
##################

ui <- 'Hello world!'

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)
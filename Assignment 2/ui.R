# Creating a dashboard sidebar using the `dashboardSidebar` function
sidebar <- dashboardSidebar(
  sidebarMenuOutput("Side_dash")
)

# Creating a dashboard header with a title that includes an icon
header <- dashboardHeader(title = span(icon("diagnoses"), "COVID-19 Tracker"))

# Defining custom CSS styles to hide shiny output errors
css <- ".shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }
       "

# Create the dashboard body using the `dashboardBody` function
body <- dashboardBody(
  
  useShinyjs(),  # Use the Shinyjs library for dynamic JavaScript functionality
  
  # Add a link to the favicon in the HTML head section
  tags$head(tags$link(rel = "shortcut icon", href = "favicon_trans.png")),
  
  # Define custom CSS styles using the `tags$style` function
  tags$style(
    make_css(
      list(
        '.dygraph-legend',               # Target the elements with class 'dygraph-legend'
        c('left', 'background-color'),  # Set the CSS properties 'left' and 'background-color'
        c('70px !important', 'transparent !important')  # Apply custom values with '!important'
      )
    )
  ),
  
  # Add custom CSS styles to the HTML head section
  tags$head(
    tags$style(
      HTML(
        ".checkboxgroup-inline {  # Target elements with class 'checkboxgroup-inline'
        margin-left: 0px;      # Set left margin to 0px
        margin-right: 10px;    # Set right margin to 10px
      }
      .checkboxgroup-inline + .checkboxgroup-inline {  # Target adjacent elements with class 'checkboxgroup-inline'
        margin-left: 0px;      # Set left margin to 0px
        margin-right: 10px;    # Set right margin to 10px
      }
      "
      )
    )
  ),
  
  # Add JavaScript code to the HTML head section using `tags$script`
  tags$script(
    HTML(
      "
    var openTab = function(tabName){
      $('a', $('.sidebar')).each(function() {
        if(this.getAttribute('data-value') == tabName) {
          this.click()
        };
      });
    }
    "
    )
  ),
  
  # Apply shadows to elements using shinyEffects
  shinyEffects::setShadow(class = "box"),  # Apply shadow to elements with class "box"
  shinyEffects::setShadow(id = "my-progress"),  # Apply shadow to the element with id "my-progress"
  
  # Use SweetAlert for custom alert dialogs
  shinyWidgets::useSweetAlert(),
  
  # Choose a slider skin with `chooseSliderSkin`
  chooseSliderSkin(skin = "Shiny", color = "DeepSkyBlue"),
  
  # Add custom CSS styles to the HTML head section using `tags$style`
  tags$style(type = "text/css", css),  # The `css` variable contains the custom CSS styles
  
  # Create a list of tab items
  tabItems(
    # Create a tab for the forecasting
    tabItem(tabName = "corTab",
            class = 'active',
            
            fluidRow(
              # Preference Selector
              column(width = 5,
                     box(title = span(icon("magic"), " Select Your Preferences"),
                         solidHeader = FALSE,  # Disable solid header
                         status = "info",  # Box status color
                         collapsible = TRUE,  # Make the box collapsible
                         width = NULL,  # Use default width
                         uiOutput("selector_country"),  # Country Selector
                         uiOutput("slider_n_days_forec"),  # Days Slider
                         htmlOutput("text_date_update")  # Date Update
                     ),
              ),
              # Data Table
              box(title = span(icon("table"), " Data Table"),
                  status = "info",  # Box status color
                  solidHeader = FALSE,  # Disable solid header
                  collapsible = TRUE,  # Make the box collapsible
                  width = 7,  # Set custom width
                  collapsed = FALSE,  # Box is initially expanded
                  DTOutput("dt_countries_cases")  # Output DataTable
              )
            ),
            
            fluidRow(
              # Statistics box
              box(title = span(icon("table"), " Statistics"),
                  solidHeader = FALSE,  # Disable solid header
                  status = "warning",  # Box status color
                  collapsible = TRUE,  # Make the box collapsible
                  width = 12,  # Full-width box
                  # ValueBoxes with spinners for loading animation
                  valueBoxOutput("valuebox_total_cases") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_deaths") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_death_rate") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_total_active") %>% withSpinner(color = "#5bc0de"),
                  valueBoxOutput("valuebox_active_per_mil") %>% withSpinner(color = "#5bc0de")
              )
            ),
            
            fluidRow(
              # TabBox containing multiple charts
              tabBox(
                id = "tabset1",
                # New Cases Panel
                tabPanel("New Cases",
                         dygraphOutput("dygraph_country_new_cases") %>% withSpinner(color = "#5bc0de")),
                # Cumulative Cases Panel
                tabPanel("Cumulative Cases",
                         dygraphOutput("dygraph_country_cases") %>% withSpinner(color = "#5bc0de")),
                # New Deaths Panel
                tabPanel("New Deaths",
                         dygraphOutput("dygraph_country_new_deaths") %>% withSpinner(color = "#5bc0de")),
                # Cumulative Deaths Panel
                tabPanel("Cumulative Deaths",
                         dygraphOutput("dygraph_country_deaths") %>% withSpinner(color = "#5bc0de"))
              ),
              # Forecasted Cumulative Cases + 90% Prediction Interval
              box(title = span(icon("chart-line"), " Forecasted Cumulative Cases + 90% Prediction Interval"),
                  solidHeader = FALSE,  # Disable solid header
                  status = "primary",  # Box status color
                  collapsible = TRUE,  # Make the box collapsible
                  width = 6,  # Custom width
                  dygraphOutput("dygraph_country_cases_forecast") %>% withSpinner(color = "#5bc0de") # Forecast Chart
              )
            )
    ),
    
    # Create a tab for comparison
    tabItem(tabName = "compareTab",
            
            fluidRow(
              # Box for giving usage information
              box(title = span(icon("info-circle"), " Information About Trajectory Comaprison"),
                  solidHeader = T, status = "info",
                  collapsible = T, width = 12,
                  htmlOutput("info_comparing_trajectories")
              )
            ),
            
            fluidRow(
              # Box for selecting countries to compare
              box(
                title = span(icon("flag"), " Select Countries to Compare"),
                solidHeader = FALSE,  # Remove header background color
                status = "info",      # Set the box status color to info
                collapsible = FALSE,  # Disable collapsing
                width = 4,            # Set the box width
                uiOutput("picker_countries_selector")  # Render the country selector UI
              ),
              # Box for selecting comparison statistic
              box(
                title = span(icon("table"), " Select Comparison Statistic"),
                solidHeader = FALSE,  # Remove header background color
                status = "danger",    # Set the box status color to danger
                collapsible = FALSE,  # Disable collapsing
                width = 4,            # Set the box width
                uiOutput("picker_stats_selector"),   # Render the statistic selector UI
                uiOutput("switch_log_scale_compareTab")  # Render the log scale switch UI
              ),
              # Box for selecting comparison parameters
              box(
                title = span(icon("angle-double-up"), " Select Comparison Parameters"),
                solidHeader = FALSE,  # Remove header background color
                status = "info",      # Set the box status color to info
                collapsible = FALSE,  # Disable collapsing
                width = 4,            # Set the box width
                uiOutput("selector_cases_since_first_n"),   # Render cases since first 'N' UI
                uiOutput("selector_deaths_since_first_n")  # Render deaths since first 'N' UI
              )
            ),
            
            fluidRow(
              # Box for displaying comparison charts
              box(
                title = span(icon("chart-line"), " Comparison of Trajectories Across Countries for the Selected Statistic"),
                solidHeader = FALSE,  # Remove header background color
                status = "primary",   # Set the box status color to primary
                collapsible = TRUE,   # Allow collapsing
                width = 12,           # Set the box width
                dygraphOutput("dygraph_countries_stats_since_first") %>% withSpinner(color = "#5bc0de")  # Render the dygraph with a spinner
              )
            )
    ),
    
    # Create a tab for clustering trajectories
    tabItem(tabName = "trajectoryTab",
            # Information Box
            fluidRow(
              box(title = span(icon("info-circle"), " Information About Trajectory Clustering"),
                  solidHeader = TRUE, status = "info",
                  collapsible = TRUE, width = 12,
                  htmlOutput("info_clustering_trajectories")
              )
            ),
            # Clustering Statistic Selection, Trajectory Starting Points, and Cluster Parameters Boxes
            fluidRow(
              # Clustering Statistic Selection
              box(title = span(icon("table"), " Select Clustering Statistic"),
                  solidHeader = FALSE, status = "info",
                  collapsible = FALSE, width = 4,
                  uiOutput("picker_stat_selector_clust"),
                  uiOutput("picker_country_clust"),
                  htmlOutput("text_inwhich_cluster_country")
              ),
              # Trajectory Starting Points Selection
              box(title = span(icon("angle-double-up"), " Select Trajectory Starting Points"),
                  solidHeader = FALSE, status = "info",
                  collapsible = FALSE, width = 4,
                  uiOutput("selector_cases_since_first_n_clust"),
                  uiOutput("selector_deaths_since_first_n_clust")
              ),
              # Cluster Parameters Selection
              box(title = span(icon("sliders-h"), " Select Cluster Parameters"),
                  solidHeader = FALSE, status = "info",
                  collapsible = FALSE, width = 4,
                  uiOutput("selector_top_n_countries_clust"),
                  uiOutput("selector_n_clusters_dtw")
              )
            ),
            # Trajectories of Clusters and Focus Plot Boxes
            fluidRow(
              # Trajectories of Clusters
              box(title = span(icon("chart-line"), " Trajectories of the Clusters for the Selected Statistic and Parameters"),
                  solidHeader = FALSE, status = "info",
                  collapsible = TRUE, width = 7,
                  fluidRow(
                    column(width = 6, uiOutput("selector_sma_order")),
                    column(width = 3, uiOutput("switch_normalization")),
                    column(width = 3, uiOutput("switch_log_scale"))
                  ),
                  plotOutput("plot_clusters_trajectories", height = "70vh") %>% withSpinner(color = "#5bc0de")
              ),
              # Focus Plot of Selected Cluster
              box(title = span(icon("crosshairs"), " Focus Plot of the Selected Cluster"),
                  solidHeader = FALSE, status = "primary",
                  collapsible = TRUE, width = 5,
                  uiOutput("picker_cluster_focus"),
                  plotlyOutput("plotly_focus_cluster", height = "70vh") %>% withSpinner(color = "#5bc0de")
              )
            ),
            # Dendrogram and MDS Plot Boxes
            fluidRow(
              # Dendrogram of Hierarchical Clustering
              box(title = span(icon("tree"), " Dendrogram of Hierarchical Clustering for Selected Statistic"),
                  solidHeader = FALSE, status = "success",
                  collapsible = FALSE, width = 7,
                  plotOutput("plot_clusters_trajectories_dendogram", height = "95vh") %>% withSpinner(color = "#5bc0de")
              ),
              # MDS Plot of Clustered Countries
              box(title = span(icon("chart-area"), " MDS Plot of Clustered Countries for Selected Statistic"),
                  solidHeader = FALSE, status = "info",
                  collapsible = FALSE, width = 5,
                  plotOutput("plot_scatter_mds_trajectories", height = "95vh") %>% withSpinner(color = "#5bc0de")
              )
            )
    ),
    
    # Create a tab for world stats
    tabItem(
      tabName = "worldTab",
      
      fluidRow(
        # Create a box with Global Statistics
        box(
          title = span(icon("globe"), " Global Statistics"),  # Box title with an icon
          solidHeader = F,  # Disable solid header
          status = "warning",  # Set the box status to warning
          collapsible = TRUE,  # Allow collapsing the box
          width = 12,  # Set the box width to 12 columns
          
          # Display value boxes with spinners
          valueBoxOutput("valuebox_total_cases_world") %>% withSpinner(color = "#5bc0de"),
          valueBoxOutput("valuebox_total_deaths_world") %>% withSpinner(color = "#5bc0de"),
          valueBoxOutput("valuebox_death_rate_world") %>% withSpinner(color = "#5bc0de"),
          valueBoxOutput("valuebox_total_active_world") %>% withSpinner(color = "#5bc0de"),
          valueBoxOutput("valuebox_active_per_mil_world") %>% withSpinner(color = "#5bc0de")
        )
      ),
      
      fluidRow(
        tabBox(
          id = "tabset3",
          
          # Create individual tab panels
          tabPanel("New Cases",
                   dygraphOutput("dygraph_world_new_cases") %>% withSpinner(color = "#5bc0de")
          ),
          tabPanel("Cumulative Cases",
                   dygraphOutput("dygraph_world_cases") %>% withSpinner(color = "#5bc0de")
          ),
          tabPanel("New Deaths",
                   dygraphOutput("dygraph_world_new_deaths") %>% withSpinner(color = "#5bc0de")
          ),
          tabPanel("Cumulative Deaths",
                   dygraphOutput("dygraph_world_deaths") %>% withSpinner(color = "#5bc0de")
          )
        ),
        # Create a separate box for forecasted cumulative cases
        box(
          title = span(icon("chart-line"), " Forecasted Cumulative Cases + 90% Prediction Interval"),  # Box title with an icon
          solidHeader = F,  # Disable solid header
          status = "primary",  # Set the box status to primary
          collapsible = TRUE,  # Allow collapsing the box
          width = 6,  # Set the box width to 6 columns
          
          # Display a dygraph with a spinner for forecasted cumulative cases
          dygraphOutput("dygraph_world_cases_forecast") %>% withSpinner(color = "#5bc0de")
        )
      )
    )
  )
)

# Define the UI (User Interface) function
ui <- function(req) {
  
  # Create a dashboard page using the `dashboardPage` function
  dashboardPage(
    title = "COVID-19 Tracker",   # Set the title of the dashboard page
    sidebar = sidebar,            # Assign the previously defined sidebar
    header = header,              # Assign the previously defined header
    body = body,                  # Assign the previously defined body (not shown in the provided code)
    skin = "blue",                # Apply a blue skin to the dashboard
    preloader = list(             # Define a preloader for the dashboard
      html = tagList(             # Use HTML tags for the preloader
        div(spin_1()),            # Display a spinning loader
        div("Loading the screen...", style = "font-size: 20px;")  # Display a loading message
      ),
      color = "black"             # Set the preloader color to black
    )
  )
  
}

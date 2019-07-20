# Draft of the dashboard UI for SHALE GAS case study (ms, June 2019)

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# based on https://github.com/mammykins/App-cherry_picker 
# This app should be used to develop your own dashboard with map as a Shiny app
# It provides a good skeleton to start from.

library(shiny)
library(leaflet) # required to render maps


# TABBED DASHBOARD STYLE -------------------------------------------------------------

ui <- navbarPage("PA Well picker", id = "nav",

   # 1st TAB: Shows the map and a floating panel to filter wells’ data
       tabPanel("Interactive map", 
                    
                    # div(class = "outer",
     
                        tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css")
                        ),
                       leafletOutput("mymap"), 
                       br(),
                      p("Imagine you wanted to plot two continuous variables associated with around eighteen thousand wells in PA, 
                      here we provide an app to demonstrate one solution."),
                     p("We zoom in on a selected County and then show all the data for wells in that county
                      filtered by configuration {Horizontal|Vertical}."),
                     p("Importantly ", strong("all this data comes from a dummy file uploaded to MAGIC-NIS repository (supplied by Cristina M. on May 2019);"),
                      " any apparently significant relationship among variables could be due to chance."),
                    
	         # A PANEL holding some controls for filtering data
                      # Shiny versions prior to 0.11 should use class="modal" instead.

                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 180, bottom = "auto",
                                      width = 330,
                                      height = "auto",
                                      h4("User input"),
                                      selectInput(inputId = "county_of_interest", label = "PA county",
                                                  choices = regions_code_list_sorted, selected = "ALLEGHENY"),
                                      selectInput(inputId = "phase", label = "Well configuration",
                                                  choices = c("Horizontal", "Vertical")),
                                      plotOutput("hist_depth", height = 200),
                                      plotOutput("hist_gas", height = 200),
                                      p("Here we provide depht and gas quantity data distributions for all counties for wells of the same configuration in PA. 
                                       To facilitate comparison the selected county relevant datum is represented by a blue tick below the horizontal axis.")
                       )
       ),
   # 2nd TAB: allows exploring/selecting and downloading data records  
   tabPanel("Data explorer", div(h3("Wells' details for the county of ", textOutput("county_name"))),
            br(),
            DT::dataTableOutput("fruit_table_data"),
            br(),
            verbatimTextOutput('fruit_table_data_rows_selected'),
            h4("Table variables explained"),
            
            p("The variables are...",
              #        	   	The ", strong("apples"), "is simulated by the ", strong("uniform"), "distribution.",
              #           	"The ", strong("pears"), " are also drawn from a uniform distribution.",
              #           	"The ", strong("cherry_status"),  " is calculated by the ", strong("apples"),
              #           	" multiplied by the ", strong("pears"), ".",
              "This statistic (", strong("cherry_status") ,
              " provides an indication of the amount of spurious correlation between the two variables.
                     One could cherry pick those wells with a ", strong("cherry_status"),
              " to provide evidence for a spurious claim (e.g. drilling deeper causes wells to produce more gas).",
              style = "font-family: 'times'; font-si16pt"),
            
            downloadButton("download_data", "Download"),
            br(),
            p("ISSUE: Following file download you may have to restart the app.", 
              style = "font-family: 'times'; font-si16pt")
   ),
   
   # 3rd TAB: shows the correlation between depth and gas production for the selected wells 
   tabPanel("Gas limit",
            h4("Wells selected from Data explorer tab"),
            DT::dataTableOutput("gas_wells"),
            plotOutput("scatter_gas", height = 200),
            h4("How deep wells are required to feed the ES with enough gas?"),
            p("This app was developed to showcase a Shiny app in R using the Leaflet package. Use your imagination, make an interactive dashboard with maps!",
              style = "font-family: 'times'; font-si16pt")
   ),
   
   # 4th TAB: describes and links to Data and methods
   tabPanel("Data and methods",
            h3("Data origin"),
            p("This app combines data from various sources, we could join data from distinct csv files using well permit num.",
              "Inspect the App data folder for the .csv files and the related NIS repository",
              a(href ="https://nextcloud.data.magic-nexus.eu/apps/files/NIS_internal/WP6/CS6_4_Shale_gas_extraction/docs", "folder."),
              br(), 
              "The original (PA_dummy) well dataset including their location was provided by ",
              a(href = "http://iaste.info/cristina-madrid-lopez/", "Cristina M."),
              "The data was downloaded from the NIS repository on May 2019.",
              "The PA counties polygon data can be found on the ArcGis website (",
              a(href = "https://www.arcgis.com/home/item.html?id=04e3f70b4b7f401faafd431da9355ab4", "PA counties"), 
              ") or in the data.gov ", a(href="http://catalog.data.gov", "catalog"),
              ".",
              style = "font-family: 'times'; font-si16pt"), 
            br(),
            h4("Mapping"),
            p("The linking of further/secondary data to original Well data could be achieved using their Well_Permit_Num (WPN).",
              style = "font-family: 'times'; font-si16pt"),
            br()
            # h4("Apples"),
            # p("Generated in Excel using RAND(). Often one receives data as a csv. ",
            #   "You can put some extra information here and share any relevant methodology with hyperlinks.",
            #   style = "font-family: 'times'; font-si16pt"),
            # br(),
            # h4("Pears"),
            # p("Generated in Excel using RAND(). Often one receives data as a csv.",
            #   style = "font-family: 'times'; font-si16pt"),
            # br(),
            # h4("Complete cases"),
            # p("Only complete cases were used,
            #   i.e. where a School had corresponding apples, pears and mapping data.",
            #   style = "font-family: 'times'; font-si16pt"),
            # br()
   ),
   ###
   tags$a(img(src = "magic_logo.png", height = 109, width = 83),
          href = "http://www.magic-nexus.eu"),
   br(),
   tags$div(id = "cite",
            'App developed by ',
            a(href = "www.stad.unina.it/2016", "ms"), ", June 2019."
   ), 
   tags$blockquote("Correlation does not imply causation.", cite = "Anon.")
   
           
   


)

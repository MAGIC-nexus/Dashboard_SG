# Shale gas dashboard prototype (ms, June 2019)

# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
# EXTRACT PA counties POLYGON ------------------------------------------------------
  # We need the polygon data only for the county of interest
  polygon_county <- reactive({
    PA_counties[match(input$county_of_interest[[1]], toupper(PA_counties@data$NAME)),]
       #  select row 
  })
  
  output$county_details <- renderTable({
    polygon_county()@data %>%
    xtable()
  })
  
  output$county_name <- renderText({
    input$county_of_interest
  })
  
# DATA FOR WELLS IN THE COUNTY --------------------------------------
# Join data and filter for County and configuration by user input
# (filtering variables could be changed/added)
  
  wells_to_map <- reactive({
    dplyr::filter(fruits,
                  well_locations$Well_County == input$county_of_interest[[1]], # filter by county
                  #well_locations$Well_Configuration == (input$phase)                # and by configuration
                  #la_number == as.numeric(sapply(strsplit(input$county_of_interest[[1]], split = " "), "[[", 1))
                  ) %>%
      left_join(well_locations) %>%
      dplyr::filter(well_locations[well_locations$Well_County == input$county_of_interest[[1]],"Well_Configuration"] == paste(input$phase[[1]],"Well")) %>% # filter by configuration
      mutate(lat = Well_Latitude, long = Well_Longitude) %>% # create columns for lat-long
      select(-Well_Latitude, -Well_Longitude) %>%
      na.omit()
  })

  output$fruit_table_data <- DT::renderDataTable({
    expr = datatable(
      wells_to_map() %>%  #  Notice the parentheses! ()
      select("Well_Permit_Num", "GrossExtraction_TJ", "Depth"
             ) 
      # %>%
      # mutate(apples = round(apples, 2), pears = round(pears, 2), cherry_status = round((apples + pears) / 2, 2)) %>%
      # rename(School_Name = school_name, URN = urn)
      ,
      selection =  list(mode = 'multiple', selected = 1, target = 'row')  #  preselection, ?datatable
      ) %>%
      #  http://rstudio.github.io/DT/functions.html
      formatStyle(
        c("GrossExtraction_TJ", "Depth"),
        color = styleInterval(0.5, c('red', 'blue'))  #  colour table font based on rule
      ) 
      # %>%
      #formatCurrency(c('apples', 'pears'),  #  add currency symbols and round
      #               '\U00A3',
      #               digits = 2)  #%>%
      #  # formatStyle(
      #  #   'cherry_status',
      #  #   background = styleColorBar(wells_to_map()$cherry_status, 'steelblue'),
      # #   backgroundSize = '100% 90%',
      # #   backgroundRepeat = 'no-repeat',
      # #   backgroundPosition = 'center')
  })
  
  # download selected records
  output$download_data <- downloadHandler(
    filename = function() { paste("shale_gas_app_", input$county_of_interest, '.csv', sep = '') },
    content = function(file) {
      write.csv(wells_to_map(), file)
    }
  )
  
  # NATIONAL DISTRIBUTIONS for COUNTY comparison --------------------------------
  
  # Provide variable distributions to aid comparison to rest of the country
  output$hist_depth <- renderPlot({
    hist(fruits$Depth,  # Note this draw an histogram for all wells in the country with the same configuration
         main = "Depth of the wells",  #  ¿Note how we call our pre-filtered data assigned in global!
         xlab = "m", col = "salmon", border = 'white', xlim = range(fruits$Depth))
    rug(wells_to_map()$Depth, ticksize = -0.2, lwd = 3, col = "blue")  #  For the rug we use our reactive dataframes
  })
  
  output$hist_gas <- renderPlot({
    plot(density(fruits$GrossExtraction_TJ), 
         main = "Gross extraction (TJ)",
         xlab = "TJ", col = '#00DD00', xlim = range(fruits$GrossExtraction_TJ))
    rug(wells_to_map()$GrossExtraction_TJ, ticksize = -0.15, lwd = 3, col = "blue")
  })
  
  output$fruit_table_data_rows_selected = renderPrint(input$fruit_table_data_rows_selected)
  
  output$scatter_gas <- renderPlot({
    ggplot(fruits, aes(x = Depth, y = GrossExtraction_TJ, col = "red")) +
      geom_bin2d() +
      geom_point(alpha = 0.2) +
      xlim(0, 6000) + ylim(0, 1000) +
      geom_point(data = slice(wells_to_map(),
                              input$fruit_table_data_rows_selected), #  Add our selected wells from the previous tab's table
                 mapping = aes(x = Depth, y = GrossExtraction_TJ,
                               shape = "circle", colour = "blue",
                               size = 4.5)) +
      # annotate( "rect", xmin = 0.8 , xmax = 1.0, ymin = 0.8, ymax = 1.0,
      #           alpha = 0.01, colour = "pink") +  #  Capture data points that are ripe for picking!
      # annotate("text", x = 0.9, y = 0.9,
      #          label = "Cherry picking region", col = "black") +
      ggthemes::theme_tufte()

  })
  
# ANOTHER TAB ----------------------------------------------------------
  # Here we can use another tab to display some furter analysis or statistics
# https://rstudio.github.io/DT/shiny.html
  # row selection
  output$green_grocers <- DT::renderDataTable(datatable(
    slice(wells_to_map() %>%
            select(school_name, apples, pears, urn) %>%
            arrange(desc(apples)) %>%
            rename(School_Name = school_name, URN = urn), #  creates identical table to slice from, see fruit_table_data
          input$fruit_table_data_rows_selected) %>%  
      mutate(made_up_statistic = (apples + pears) * (if_else(input$phase == "Secondary",
                                              3,  #  Secondary school children need more fruit!?
                                              1) * 30),
             cherry_status = round((apples + pears) / 2, 2)
    ) %>%  #  we refine the datatable here and prettify
      select(School_Name, cherry_status, made_up_statistic)
  ) %>%  #  and prettify
    formatRound(c("School_Name", "made_up_statistic"),
                0)
  )
  
  # # MAPPING SETUP -----------------------------------------------------------

  # Variables for holding the coordinate system types (see: # http://www.epsg.org/ for details)
  # The CRS is retrieved from the geographical layer in global.R
  # PA_counties_epsg <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # ukgrid <- "+init=epsg:27700"
  # latlong <- "+init=epsg:4326"
  

  # Create coordinates variable, first argument
  # Create the SpatialPointsDataFrame, note coords and data are distinct slots in S4 object
  # Vestigial name from condainment app
  wells_sp_ll <- reactive({
    #  we use x as a placeholder just within this reactive bit, helps with the last renaming step
    x <- spTransform(
      sp::SpatialPointsDataFrame(dplyr::select(wells_to_map(), lat, long),
                                 data = dplyr::select(wells_to_map(), -lat, -long),
                                 proj4string = CRS(PA_counties_epsg)),
                                 CRS(PA_counties_epsg)
    )
    # Convert from Eastings and Northings to Latitude and Longitude and rename columns
    colnames(x@coords)[colnames(x@coords) == "long"] <- "longitude"
    colnames(x@coords)[colnames(x@coords) == "lat"] <- "latitude"
    
    x
  })
  
    # LEAFLET -----------------------------------------------------------------
  
  output$mymap <- renderLeaflet({
    pal11 <- colorNumeric(palette = "PuRd",
                          wells_sp_ll()@data$Depth)
    pal12 <- colorNumeric(palette = "PuBuGn",
                          wells_sp_ll()@data$GrossExtraction_TJ) 
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)
    
    m11 <- leaflet(data = wells_sp_ll()@data) %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
      addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      # Apples
      addCircles(lng = wells_sp_ll()@coords[, "longitude"],
                 lat = wells_sp_ll()@coords[, "latitude"],
                 color = "black",
                 opacity = 0.8,
                 weight = 0.5,
                 radius = 2000,  #  Radius could be assigned to (default 200) another variable
                 fillOpacity = 0.5,
                 fillColor = pal11(wells_sp_ll()@data$Depth),
                 popup = NULL, group = "Depth") %>%
      addLegend("bottomright", pal = pal11,
                values = wells_sp_ll()@data$Depth,
                title = "Depth",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.5, layerId = "Depth") %>%
      # Oranges
      addCircles(lng = wells_sp_ll()@coords[, "longitude"],
                 lat = wells_sp_ll()@coords[, "latitude"],
                 color = "black",
                 opacity = 1, radius = 2000, weight = 1,
                 fillOpacity = 0.3,
                 fillColor = pal12(wells_sp_ll()@data$GrossExtraction_TJ),
                 popup = NULL, group = "Gross Extraction") %>%
      addLegend("bottomleft", pal = pal12,
                values = wells_sp_ll()@data$GrossExtraction_TJ,
                title = "Extraction",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.5, layerId = "GrossExtraction") %>%
      # Extraction outline
      addCircles(lng = wells_sp_ll()@coords[, "longitude"],
                 lat = wells_sp_ll()@coords[, "latitude"],
                 color = pal12(wells_sp_ll()@data$GrossExtraction_TJ),
                 opacity = 1,
                 radius = 2010,
                 weight = 5,
                 fillOpacity = 0,
                 fillColor = pal12(wells_sp_ll()@data$GrossExtraction_TJ),
                 popup = NULL, group = "Gross Extraction Outline") %>%
      # marker
      addMarkers(lng = wells_sp_ll()@coords[, "longitude"],
                 lat = wells_sp_ll()@coords[, "latitude"],
                 popup = as.character(paste(wells_sp_ll()@data$Well_Permit_Num, 
                                            "has a gross production of",
                                            round(wells_sp_ll()@data$GrossExtraction_TJ),
                                            "TJ per year.",
                                            sep = "\n"
                 )),
                 options = popupOptions(closeButton = TRUE),
                 group = "Info") %>%
      ### County polygons
      addPolygons(data = polygon_county(), 
                  stroke = TRUE, fillOpacity = 0, smoothFactor = 0.2, 
                  color = "black", weight = 1,
                  group = "County boundary") %>%
      ### Groups
      hideGroup("Info") %>%
      hideGroup("Gross Extraction") %>%
      hideGroup("Gross Extraction Outline") %>%
      showGroup("Depth") %>%
      hideGroup("Terrain") %>%
      showGroup("OSM (B & W)") %>%
      showGroup("County boundary") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Terrain", "OSM (B & W)"),
        overlayGroups = c("Depth", "Gross Extraction Outline",
                          "Gross Extraction",
                          "Info"
                          ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    m11
  })
  
}



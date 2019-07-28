# Shale gas dashboard prototype (ms, June 2019)
# SERVER functions, revised by MS, July 2019

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
  
  # EXTRACT PA counties POLYGONS ------------------------------------------------------
  # We need the polygon data only for the county of interest
  polygon_county <- reactive({
    PA_counties[match(input$county_of_interest[[1]], toupper(PA_counties@data$NAME)),]
    #  select the row of interest in the PA_counties df 
  })

  # output of potential use in the UI: currently NOT used
  output$county_details <- renderTable({
    polygon_county()@data %>%
      xtable()
  })
  
  # output of potential use in the UI: currently NOT used
  output$county_name <- renderText({
    input$county_of_interest
  })
  
  # DATA FOR WELLS IN THE COUNTY --------------------------------------
  # Join data and filter for County and configuration by user input
  # (filtering variables could be changed/added)
  
  wells <- reactiveValues(to_map = NULL, sel = NULL) # Instantiate a reactive container for current wells data
  
  observeEvent(c(input$county_of_interest, input$phase), # update current wells' selection based on the filters
               {
                 wells$to_map <- dplyr::filter(fruits,
                               well_locations$Well_County == input$county_of_interest[[1]] # filter by county
                               #well_locations$Well_Configuration == (input$phase)                # and by configuration
                               #la_number == as.numeric(sapply(strsplit(input$county_of_interest[[1]], split = " "), "[[", 1))
                               ) %>%
                                left_join(well_locations) %>%
                                dplyr::filter(well_locations[well_locations$Well_County == input$county_of_interest[[1]],"Well_Configuration"] == paste(input$phase[[1]],"Well")) %>% # filter by configuration
                                mutate(lat = Well_Latitude, long = Well_Longitude) %>% # create columns for lat-long
                                select(-Well_Latitude, -Well_Longitude) %>%
                                na.omit() # drop rows with NA
               },
               ignoreNULL = FALSE ) # perform the update also on start-up
  
  observeEvent(wells$to_map,
               {
                 wells$sel <- vector(mode = 'logical', length = length(wells$to_map$Well_Permit_Num)) # init the sel vector to the default no well
               })
              
  
  # wells_to_map <- reactive({
  #   dplyr::filter(fruits,
  #                 well_locations$Well_County == input$county_of_interest[[1]], # filter by county
  #                 #well_locations$Well_Configuration == (input$phase)                # and by configuration
  #                 #la_number == as.numeric(sapply(strsplit(input$county_of_interest[[1]], split = " "), "[[", 1))
  #   ) %>%
  #     left_join(well_locations) %>%
  #     dplyr::filter(well_locations[well_locations$Well_County == input$county_of_interest[[1]],"Well_Configuration"] == paste(input$phase[[1]],"Well")) %>% # filter by configuration
  #     mutate(lat = Well_Latitude, long = Well_Longitude) %>% # create columns for lat-long
  #     select(-Well_Latitude, -Well_Longitude) %>%
  #     na.omit() %>%
  #     mutate(vec_sel = FALSE) # add a column to store selection flag for the wells (default FALSE)
  #     # %>% mutate(vec_sel = ifelse( Well_Permit_Num == input$mymap_marker_click$id, 1 - isolate(vec_sel), isolate(vec_sel) ) ) # react to click on the map
  # })
  
  # glob <- reactiveValues(sel = NULL)
  # 
  # observer(glob$sel <- wells_to_map()$vec_sel) # Each time vec_sel changes its status is copied to glob$sel
  # 
  # newsel <- reactive({ # Updated statuts for selection is calculated after user click
  #   ii <- match(input$mymap_marker_click$id, wells_to_map()$Well_Permit_Num)[1]
  #   sel <- glob$sel() # access and store locally current selection status
  #   lii <- vector(mode = "logical", length = length(sel)) # create a logical vector for updating selection step 1
  #   lii[ii] <- TRUE # step 2
  #   xor(as.logical(sel),lii) # return the updated vector
  # })
  
  observeEvent(input$mymap_marker_click$id, { # Observe clicks on any marker in the map to
      sel <- wells$sel
      ii <- match(input$mymap_marker_click$id, wells$to_map$Well_Permit_Num)[1]
      lii <- vector(mode = "logical", length = length(sel))
      lii[ii] <- TRUE
      newsel <- xor(as.logical(sel),lii)
               #print(c("pri", sel)) # debugging
               # print(c("pos", newsel)) # debugging
      isolate(wells$sel <- newsel)     # set the new status to global reactive variable wells$sel
               #wells_to_map()$vec_se <- 1-(wells_to_map()$vec_sel[match(wells_to_map()$Well_Permit_Num, input$mymap_marker_click$id)])
  })

 # output selected wells
   output$wells_selected <- renderText(wells$to_map$Well_Permit_Num[wells$to_map$vec_sel])
  
  
  # otuput with the table of the wells data in the chosen county
   output$fruit_table_data <- DT::renderDataTable({
    expr = datatable(
      wells$to_map %>%  #  Notice the parentheses! ()
        select("Well_Permit_Num", "GrossExtraction_TJ", "Depth"),
      selection =  list(mode = 'multiple', selected = which(wells$sel), target = 'row')  #  selection comes from map clicks
    ) %>%
      #  see http://rstudio.github.io/DT/functions.html
      formatStyle(
        c("GrossExtraction_TJ", "Depth"),
        color = styleInterval(0.5, c('red', 'blue'))  #  colour table font based on rule
      ) 
    # %>%
    #formatCurrency(c('dollars', 'euros'),  #  add currency symbols and round IN CASE
    #               '\U00A3',
    #               digits = 2)  #%>%
    #  # formatStyle(
    # #   backgroundSize = '100% 90%',
    # #   backgroundRepeat = 'no-repeat',
    # #   backgroundPosition = 'center')
  })
  
  # download selected records
  output$download_data <- downloadHandler(
    filename = function() { paste("shale_gas_app_", input$county_of_interest, '.csv', sep = '') },
    content = function(file) {
      write.csv(wells$to_map[input$fruit_table_data_rows_selected,], file)
    }
  )
  
  output$analitycs <- renderText(
    paste("Total production:",sum(wells$to_map$GrossExtraction_TJ[input$fruit_table_data_rows_selected]))
    )
  
  # NATIONAL DISTRIBUTIONS for COUNTY comparison --------------------------------
  
  # Provide variable distributions to aid comparison to rest of the country
  output$hist_depth <- renderPlot({
    hist(fruits$Depth,  # Note this draw an histogram for all wells in the country with the same configuration
         main = "Depth of the wells",  #  ¿Note how we call our pre-filtered data assigned in global!
         xlab = "m", col = "salmon", border = 'white', xlim = range(fruits$Depth))
    rug(wells$to_map$Depth, ticksize = -0.2, lwd = 3, col = "blue")  #  For the rug we use our reactive dataframes
  })
  
  output$hist_gas <- renderPlot({
    plot(density(fruits$GrossExtraction_TJ), 
         main = "Gross extraction (TJ)",
         xlab = "TJ", col = '#00DD00', xlim = range(fruits$GrossExtraction_TJ))
    rug(wells$to_map$GrossExtraction_TJ, ticksize = -0.15, lwd = 3, col = "blue")
  })
  
  output$fruit_table_data_rows_selected = renderPrint(input$fruit_table_data_rows_selected)
  
  output$scatter_gas <- renderPlot({
    ggplot(fruits, aes(x = Depth, y = GrossExtraction_TJ, col = "red")) +
      # geom_bin2d() + This would add a 2-dimensional binning of points
      geom_point(alpha = 0.2) +
      xlim(0, 6000) + ylim(0, 1000) +
      geom_point(data = slice(wells$to_map,
                              input$fruit_table_data_rows_selected), #  Add our selected wells from the previous tab's table
                 mapping = aes(x = Depth, y = GrossExtraction_TJ,
                               shape = "circle", colour = "blue",
                               size = 4.5)) # +
      # annotate( "rect", xmin = 0.8 , xmax = 1.0, ymin = 0.8, ymax = 1.0,
      #           alpha = 0.01, colour = "pink") +  #  Capture data points that are ripe for picking!
      # annotate("text", x = 0.9, y = 0.9,
      #          label = "Cherry picking region", col = "black") +
      # ggthemes::theme_tufte()
    
  })
  
  # ANOTHER TAB ---------------------------------------------------------------
  # # Here we can use another tab to display some furter analysis or statistics
  # # https://rstudio.github.io/DT/shiny.html
  # # row selection
  # output$green_grocers <- DT::renderDataTable(datatable(
  #   slice(wells$to_map %>%
  #           select(school_name, apples, pears, urn) %>%
  #           arrange(desc(apples)) %>%
  #           rename(School_Name = school_name, URN = urn), #  creates identical table to slice from, see fruit_table_data
  #         input$fruit_table_data_rows_selected) %>%  
  #     mutate(made_up_statistic = (apples + pears) * (if_else(input$phase == "Secondary",
  #                                                            3,  #  Secondary school children need more fruit!?
  #                                                            1) * 30),
  #            cherry_status = round((apples + pears) / 2, 2)
  #     ) %>%  #  we refine the datatable here and prettify
  #     select(School_Name, cherry_status, made_up_statistic)
  # ) %>%  #  and prettify
  #   formatRound(c("School_Name", "made_up_statistic"),
  #               0)
  # )
  
  # # MAPPING SETUP -----------------------------------------------------------
  
  # Variables for holding the coordinate system types (see: # http://www.epsg.org/ for details)
  # The CRS is retrieved from the geographical layer in global.R
  # PA_counties_epsg <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  

 # A) Wells and geographical layers to be displayed
  # Create coordinates variable, first argument
  # Create the SpatialPointsDataFrame, note coords and data are distinct slots in S4 object
  wells_sp_ll <- reactive({
    #  we use x as a placeholder just within this reactive bit, helps with the last renaming step
    x <- spTransform(
      sp::SpatialPointsDataFrame(dplyr::select(wells$to_map, lat, long),
                                 data = dplyr::select(wells$to_map, -lat, -long),
                                 proj4string = CRS(PA_counties_epsg)),
                                 CRS(PA_counties_epsg)
    )
    # rename Latitude and Longitude columns
    colnames(x@coords)[colnames(x@coords) == "long"] <- "longitude"
    colnames(x@coords)[colnames(x@coords) == "lat"] <- "latitude"
    
    x
  })
  
 # B) render the LEAFLET map -----------------------------------------------------------------
  output$mymap <- renderLeaflet({
    
    # define the base map
    m11 <- leaflet(
      # base data to be drawn (georeferenced data in a spatial dataframe)
      data = wells_sp_ll()@data) %>%
      # declare optional layers for the tiles
      addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
      addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)")  %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>% # NOT USED
    ### County polygons
    addPolygons(data = polygon_county(), 
                stroke = TRUE, fillOpacity = 0, smoothFactor = 0.2, 
                color = "black", weight = 1,
                group = "County boundary")  # %>% setView(mean(data$latitude), mean(data$longitude), zoom = 4)
    
    m11 # render the map
  })
  
  observe({
    # Define color palettes for legends
    pal11 <- colorNumeric(palette = "PuRd",
                          wells_sp_ll()@data$Depth)
    pal12 <- colorNumeric(palette = "PuBuGn",
                          wells_sp_ll()@data$GrossExtraction_TJ) 
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       wells_sp_ll()@data$xxx) # not used
    
    # define custom markers for wells  
    ### I edited an iconfinder file and created my own marker.
    ### (generated two png with two different colors)
    WellIcons <- iconList("blue" = makeIcon("www/Well_uns.png", iconWidth = 24, iconHeight =32),
                          "orange" = makeIcon("www/Well_sel.png", iconWidth = 24, iconHeight =32))
    
    leafletProxy("mymap") %>%
      clearShapes() %>%
      # declare layer simbols for visaulizing data features
      # wells depth
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
      # wells gross extraction
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
      # wells extraction outline
      addCircles(lng = wells_sp_ll()@coords[, "longitude"],
                 lat = wells_sp_ll()@coords[, "latitude"],
                 color = pal12(wells_sp_ll()@data$GrossExtraction_TJ),
                 opacity = 1,
                 radius = 2010,
                 weight = 5,
                 fillOpacity = 0,
                 fillColor = pal12(wells_sp_ll()@data$GrossExtraction_TJ),
                 popup = NULL, group = "Gross Extraction Outline") %>%
      ### Groups initial view
      showGroup("Info") %>%
      hideGroup("Gross Extraction") %>%
      hideGroup("Gross Extraction Outline") %>%
      hideGroup("Depth") %>%
      hideGroup("Terrain") %>%
      showGroup("OSM (B & W)") %>%
      showGroup("County boundary") %>%
      # well marker
      addMarkers(lng = wells_sp_ll()@coords[, "longitude"],
                 lat = wells_sp_ll()@coords[, "latitude"],
                 layerId = wells_sp_ll()@data$Well_Permit_Num, # Use the WPN as Id to check for selection by clicking makers
                 icon = WellIcons[ifelse(wells$sel, "orange","blue")], # ~WellIcons[ifelse(wells_sp_ll()@data$vec_sel, "orange","blue")], # TODO Set icon marker color depending on the selection status
                 popup = as.character(paste(wells_sp_ll()@data$Well_Permit_Num,
                                            "has a gross production of",
                                            round(wells_sp_ll()@data$GrossExtraction_TJ),
                                            "TJ per year.",
                                            sep = "\n")),
                 options = popupOptions(closeButton = TRUE),
                 group = "Info" ) %>% 
      addLayersControl(
        baseGroups = c("Terrain", "OSM (B & W)"),
        overlayGroups = c("Depth", "Gross Extraction Outline",
                          "Gross Extraction",
                          "Info"),
        options = layersControlOptions(collapsed = FALSE)
    )
      # and so on in a similar fashion for the rest of your shapes
  }) # raised priority
 

 # # HANDLE click on markers in the map for selecting wells
 #  # observe the marker click info and print to console when it is changed.
 #  observeEvent(input$mymap_marker_click,{
 #    print(paste("observed mymap_marker_click: ",input$mymap_marker_click$id))
 #    # print(input$mymap_marker_click$id)
 #    a <- input$mymap_marker_click$id
 #    ii <- match(a,wells_to_map()$Well_Permit_Num)
 #    print(ii)
 #    # params$tmp =rep(FALSE, ii) # Testing example
 #    # print(params$tmp) # used only for testing
 #    sel = wells_to_map()$vec_sel[ii]
 #    wells_to_map()$vec_sel[ii] <- 1-sel # TODO toggle selection status
 #    # marker name = ifelse(sel, "orange", "blue")
 #    print(head(wells_to_map()$vec_sel, 15))
 #    # the old approach (keeping a list of the selected wells WPNs)
 #    # wells_to_map()[match(a,well_locations$Well_Permit_Num),"Sel"] <- ifelse(wells_to_map()[match(a,well_locations$Well_Permit_Num),"Sel"], FALSE, TRUE)
 #    # if (a %in% vec_sel)
 #    #   { print("remove")
 #    #   vec_sel <<- vec_sel[- match(a,vec_sel)] } # remove the WPN from the lost of selected wells
 #    # else
 #    #   { print("add")
 #    #   vec_sel<<-append(a,vec_sel)} # add the WPN to the lost of selected wells
 #    # 
 #    # print(vec_sel)
 #    # 
 #    #data$clickedMarker <- input$mymap_marker_click
 #    #print(data$clickedMarker)
 #    output$myTable <- renderTable({
 #      return( NULL #sel
 #         # subset(well_locations, well_locations$Well_Permit_Num == data$clickedMarker, select = Well_Permit_Num, drop = FALSE)  
 #      )
 #    })
 #  })
# end of server function
}

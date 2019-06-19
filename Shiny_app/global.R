### Draft of the dashboard UI for SHALE GAS case study (ms, June 2019)
###### Based on CherryPicker app by Matt Gregory
######              http://twitter.com/mammykins_
###### published at https://github.com/mammykins/App-cherry_picker

# What is global.R? -------------------------------------------------------
# a script executed before app launches
# the objects generated here can be used both in user and sever
# these are actions that can be done once per session
# such as library calls, data source loading and custom function sourcing


# PACKAGES ---------------------------------------------------------------

library(leaflet)
library(RColorBrewer)
library(sp)
library(tidyverse)
library(rgdal)
library(testthat)
library(xtable)
library(DT)
# library(checkpoint)
# checkpoint("2016-12-28")  # ymd

# DATA INPUT -----------------------------------------------------------------

# PA POLYGON --------------------------------------------------------------

# Get polygon for PA of interest boundary
## load PA Counties shapefile (to identify county boundaries)
PA_counties <- readOGR("PAdata/PA_Counties_clip_shp/.", "PA_Counties_clip", verbose = FALSE)
## not needed converting PA shapefile coordinates to lat/long
PA_counties_epsg <- proj4string(PA_counties)

#PA_counties <- spTransform(PA_counties, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# projection string of the original PA County boundaries: "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"



# QA ----------------------------------------------------------------------
#expect_equal(length(la_s_ll@data$LEA_NAME), 152,
#             info = "there are 152 LA, do we have data for all?")

#  We can add some tests into our code using the testthat package
#  This provides quality assurance for our inputs and outputs
#  http://www.machinegurning.com/rstats/test-driven-development/

# WELL COORD data ----------------------------------
well_locations <- read_rds("PAdata/well_locations.rds")
well_locations$Well_Permit_Num <-as_factor(well_locations$Well_Permit_Num) # unique key for joins

# ANY additional data related to wells (to be joined by means of unique key: Well_Permit_Num)

apples <- readRDS("PAdata/apples.rds") %>% mutate(Depth = - floor(Depth) )  #Depth w sign changed # read_rds("data/apples_data.rds")
oranges <- readRDS("PAdata/oranges.rds") #GrossExtraction_TJ # read_rds("data/oranges_data.rds")

# JOIN DATA ---------------------------------------------------------------
fruits <- dplyr::left_join(apples, oranges) # Join by Well_Permit_Num
fruits$Well_Permit_Num <- as.factor(fruits$Well_Permit_Num) # convert Well_Permit_Num to factor

map_points <- well_locations

# MAPPING SETUP -----------------------------------------------------------

# Variables for holding the coordinate system types (see: # http://www.epsg.org/ for details) 
#ukgrid <- "+init=epsg:27700" 
#latlong <- "+init=epsg:4326"

# Create coordinates variable
coords <- dplyr::select(map_points, Well_Latitude, Well_Longitude)
# Create the SpatialPointsDataFrame, note coords and data are distinct slots in S4 object 
# TODO check projection: epsg:7572

#AreaOfUse [USA - Pennsylvania] Code: EPSG::1407 Name: USA - Pennsylvania
#                                  Description: United States (USA) - Pennsylvania. SEE ALSO: North PA -> EPSG::32028 
wells_sp <- sp::SpatialPointsDataFrame(coords,
                                         data = dplyr::select(map_points, -Well_Latitude, -Well_Longitude),
                                         proj4string = CRS(PA_counties_epsg)) # set the same CRS that the county boudaries hold

# CONVERT TO LONG & LAT ---------------------------------------------------
# Convert from Eastings and Northings to Latitude and Longitude
# surplus_sp_ll <- spTransform(surplus_sp, CRS(latlong))
# we ONLY need to rename the columns
colnames(wells_sp@coords)[colnames(wells_sp@coords) == "Well_Longitude"] <- "longitude" 
colnames(wells_sp@coords)[colnames(wells_sp@coords) == "Well_Latitude"] <- "latitude"

# With the data in place, the user, via the app, can select the appropriate PA region of interest to filter for


# EFFICIENCY --------------------------------------------------------------
regions_code_list_sorted <- sort(unique(well_locations$Well_County))
# USER Friendly labels for drop down list
# PA_user_friendly_list <- unique(select(well_locations, la_number, la)) %>%
#  mutate(la_combined = paste(la_number, la, sep = " - ")) %>%
#  select(la_combined) %>%
#  as.vector()


# POPUP DIAGNOSIS ---------------------------------------------------------
# source("make_popup_vector_from_numeric.R")

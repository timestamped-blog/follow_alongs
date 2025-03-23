library(tidyverse)
library(sf)

main_project_directory <- paste0("... main project directory ...")
objects_directory <- paste0(main_project_directory,"objects/")
data_sets <- paste0(main_project_directory,"data/")

# Read in your locations data.
# If you do not have a file to plot, I have a copy of my locations on github,
# available in .csv and .rds format: https://github.com/timestamped-blog/follow_alongs
# This script will use the .rds file.
early_xty_locations <- readRDS(paste0(objects_directory,"early_xty_locations.rds"))

# Read in your world shapefile. downloaded from https://www.naturalearthdata.com/
# Have it unzipped to the main folder where your code lives.
world <- st_read(paste0(main_project_directory,"/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"))  


# Make the locations sf object for mapping.
locations_sf <- locations_master %>% 
  rename("lon" = longi) %>% 
  dplyr::filter(!is.na(lat)) %>% 
  dplyr::select(locationID, lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(st_crs(world))

# Min/max of my location data, use for cropping to the area that holds all points.
locations_master %>% 
  dplyr::filter(!is.na(lat)) %>% 
  summarize(min_lon = min(longi),
            max_lon = max(longi),
            min_lat = min(lat),
            max_lat = max(lat))

# Add the min/max coordinates.
# Round down on the min-values, and up on the max-values, regardless of the decimal value.
# This gives the points on the edges of the map some breathing room.
# (Note: x = longitude, y = latitude)
world_cropped <- st_crop(world,
                         xmin = -10, xmax = 76, 
                         ymin = 7, ymax = 57)

# Plot
ggplot() +
  geom_sf(data = world_cropped,
          fill = "navajowhite") +
  geom_sf(data = locations_sf,
          alpha = 0.5)

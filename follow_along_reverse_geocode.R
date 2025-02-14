
# Setup -------------------------------------------------------------------
library(tibble)
library(tidyverse)

test_locations <- tibble(
  location_id = c("9XFRII5PX9C1",	"01IGIIIBZKAT",	"046L3BBRTMG7",	"04MTL0M2YFGU",	"04TW230ZUDPN",	"054YS5GI5VC6",	"066W6FZESWTH",	"080S9STZ4P34",	"0888MXLQB4BA",	"08NJN72IRRDR"),
  ancient_city = c("Cornwall",	"Carini",	"Caere - Cerveteri - Centocelle",	"Saqqara",	"Nevers",	"Bologna",	"Seleucia",	"Oloron",	"Artaz - Mardistan - Erzurum",	"Heraclea"),
  ancient_region = c("Britain",	"Italy",	"Etruria",	"Egypt",	"Gaul",	"Italy",	"Pisidia",	"France",	"Armenia",	"Thracia Prima"),
  lat_long = c("50.40000000, -4.9000",	"38.15814418656147, 13.174123126985677",	"42.091179, 11.79681",	"29.871111, 31.216389",	"47.08333333, 3.50000000",	"44.4945737, 11.3455467",	"37.900000, 30.617900",	"43.18801197461766, -0.6163607797619663",	"39.908611, 41.276944",	"40.97083333, 27.95444444"),
  lat = c("50.40000000",	"38.15814419",	"42.091179",	"29.871111",	"47.08333333",	"44.4945737",	"37.9",	"43.18801197",	"39.908611",	"40.97083333"),
  long = c("-4.9000",	"13.17412313",	"11.79681",	"31.216389",	"3.5",	"11.3455467",	"30.6179",	"-0.61636078",	"41.276944",	"27.95444444"),
  modern_country = c("Britain",	"Italy",	"Italy",	"Egypt",	"France",	"Italy",	"Turkey",	"France",	"Turkey",	"Turkey"))



# Perform reverse_geocode() -----------------------------------------------

# Use reverse_geocode() to locate the current location information from the lat/long coordinates.

# Walk-thru of script:
# Filter out any incomplete coordinates or blank coordinates - the lookup will fail otherwise.
# For lat = point to your latitude column, long = to your longitude column.
# method = is the mapping method you wish to use. Here I use Open Street Map ("osm"), but there are others. 
# More details here: https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html 
# full_results = T returns the entire wide set produced by reverse_geocode.
# OSM will return many place names in their native languages. custom_query = returns results in US English. 
rev_geo <- test_locations %>%
  filter(!is.na(lat_long)) %>% 
  reverse_geocode(
    lat = lat,
    long = long,
    method = "osm",
    full_results = T,
    custom_query  = list("accept-language"="en-US")
  )

# Check countries that don't match what was brought over by the reverse_geocode(). Some mismatches will
# likely happen, but they might still be the correct country information.
rev_geo %>% 
  select(location_id, 
         address, 
         ancient_city, 
         ancient_city, 
         modern_country, 
         country) %>% 
  mutate(countries_check = if_else(modern_country == country, T, F)) %>% 
  filter(countries_check == F)
  



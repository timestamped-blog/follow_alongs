# continue ----------------------------------------------------------------
# This script piggy backs onto the script generated in this walkt hrough:
# https://www.time-stamped.blog/2025/02/26/finding-location-information-with-fuzzyjoin/
library(tidyverse)
library(tibble)
library(fuzzyjoin)

# Directory setup.
main_project_directory <- paste0("... main project directory ...")
objects_directory <- paste0(main_project_directory,"objects/")
data_sets <- paste0(main_project_directory,"data/")

# Load the list of Pleiades data sets created in the last script:
pleiades_from_json_cleaned_tidied <- readRDS(paste0(
  objects_directory, "pleiades_from_json_cleaned_tidied.rds"))

# Load full json file - just in case
pleiades_full_json <- readRDS(paste0(objects_directory, "pleiades_full_json.rds"))

# Trismegistos data dump.
# This .csv file can be downloaded here: https://www.trismegistos.org/dataservices/tabledump/
trismegistos_all_export_geo <- read.csv(paste0(data_sets,"trismegistos_all_export_geo.csv"))

# Load previous objects

# Matches within 5 km of original coordinates. Covered on:
# https://www.time-stamped.blog/2025/02/26/finding-location-information-with-fuzzyjoin/
locations_5km <- readRDS(paste0(objects_directory,"locations_5km.rds"))

# Load original list of locations. More info here: 
# https://www.time-stamped.blog/2025/02/14/using-reverse_geocode-to-verify-place-information/
locations_master <- readRDS(paste0(objects_directory,"locations_master.rds"))

# Created here:
# https://www.time-stamped.blog/2025/02/26/finding-location-information-with-fuzzyjoin/
no_matches <- readRDS(paste0(objects_directory,"locations_master.rds"))


# Clean, tidy Trismegistos Data -------------------------------------------

# Start big cleanup of tris data
tris_data_long_clean <- trismegistos_all_export_geo %>% 
  as_tibble() %>% 
  select(id,
         provincia,
         country,
         name_latin,
         name_standard,
         full_name) %>% 
  
  # delicate with the full name...break out carefully
  # By " - "
  separate_wider_delim(
    full_name,
    delim = " - ",
    names = c("country_region_ext","city_ext"),
    too_many = "merge",
    too_few = "align_start") %>% 
  
  separate_wider_delim(
    country_region_ext,
    delim = ",",
    names = c("country_ext","region_ext"),
    too_many = "merge",
    too_few = "align_start") %>%
  
  # Render the "[" in the broken-out city to a "(" to separate again
  mutate(city_ext = str_replace_all(city_ext, " \\[", "(")) %>% 
  
  separate_wider_delim(
    city_ext,
    delim = "(",
    names = c("city_ext_1","city_ext_2"),
    too_many = "merge",
    too_few = "align_start") %>%

  separate_wider_delim(
    name_latin,
    delim = " - ",
    names = paste0("tris_latin_", 1:25),
    too_many = "merge",
    too_few = "align_start") %>% 
  select(where(~ !all(is.na(.))),
         -country_ext) %>% 

  pivot_longer(!c(id, provincia, region_ext, country),
               names_to = "source_seq",
               values_to = "name") %>% 
  mutate(name = str_trim(str_to_lower(name)),
         region_ext = str_trim(region_ext),
         name = if_else(name == "", NA, name),
         country = str_to_lower(country),
         name = str_replace_all(name, "\\)", "")) %>% 
  filter(!is.na(name)) %>% 
  arrange(id) %>% 
  select(-source_seq) %>% 
  distinct() %>% 
  relocate(name, .after = id)


# Save Point --------------------------------------------------------------

saveRDS(tris_data_long_clean, paste0(objects_directory,"tris_data_long_clean.rds"))


# Move onto the no_matches.
tris_matches_2_no_matches <- no_matches %>% 
  mutate(ancientcity = str_to_lower(ancientcity),
         country = str_to_lower(country)) %>%
  inner_join(
    tris_data_long_clean,
    by = join_by(ancientcity == name,
                 country == country)) 


# Joining all sets together -----------------------------------------------

# Start Script
# Block 1
# Bind the 2 data sets together by rows.
locations_plei_and_tris <- locations_5km %>% 
  rename("plei_id" = id) %>% 
  select(-long,
         -lat) %>% 
  bind_rows(
    tris_matches_2_no_matches %>% 
      rename("orig_lat" = lat,
             "orig_long" = longi,
             "tris_id" = id)) %>% 
  
  # Block 2
  left_join(
    pleiades_from_json_cleaned_tidied$pleiades_references %>% 
      filter(grepl("trismegistos", accessURI, ignore.case = T)) %>% 
      mutate(tris_id_extract = str_extract(accessURI, "[0-9]+")) %>% 
      select(id, tris_id_extract),
    by = join_by(plei_id == id)) %>% 
  
  # Block 3
  left_join(
    tris_data_long_clean %>% 
      select(id, 
             provincia, 
             country) %>% 
      mutate(id = as.character(id)) %>% 
      distinct(),
    by = join_by(tris_id_extract == id)
  ) %>% 
  # Block 4
  mutate(trismegistos_id = coalesce(as.character(tris_id), tris_id_extract),
         trismegistos_provincia = coalesce(provincia.x, provincia.y)) %>% 
  rename("country" = country.x) %>% 
  select(-provincia.x,
         -region_ext,
         -tris_id_extract,
         -provincia.y,
         -country.y,
         -tris_id) 


# Final Tidy --------------------------------------------------------------
locations_cited <- locations_master %>% 
  rename("orig_region" = ancientregion,
         "orig_country" = country) %>% 
  left_join(
    locations_plei_and_tris %>% 
    select(-ancientcity,
           -ancientregion,
           -country,
           -orig_lat,
           -orig_long),
    by = join_by(locationID == locationID))  


# Check which records didn't carry over.
locations_cited %>% 
  filter(is.na(plei_id) &
           is.na(trismegistos_id)) %>% 
  slice_sample(n = 10)


# Save Point --------------------------------------------------------------
saveRDS(locations_cited, paste0(objects_directory,"locations_cited.rds"))
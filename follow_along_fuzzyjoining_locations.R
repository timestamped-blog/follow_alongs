# continue ----------------------------------------------------------------
# This script piggy backs onto the script generated in this walkthrough:
# https://www.time-stamped.blog/2025/02/20/wrangling-json-data/
library(tidyverse)
library(tibble)
library(fuzzyjoin)

main_project_directory <- paste0("... setup main project directory ...")
objects_directory <- paste0(main_project_directory,"objects/")
data_sets <- paste0(main_project_directory,"data/")

# Load the list of Pleiades data sets created in the last script:
pleiades_from_json_cleaned_tidied <- readRDS(paste0(objects_directory, "pleiades_from_json_cleaned_tidied.rds"))

# Load the data set where we performed the reverse_geocode() in this post:
# https://www.time-stamped.blog/2025/02/14/using-reverse_geocode-to-verify-place-information/
locations_reversed_joined <- readRDS(paste0(objects_directory, "locations_reversed_joined.rds"))

# Load full json file 
pleiades_full_json <- readRDS(paste0(objects_directory, "pleiades_full_json.rds"))

# fuzzyjoin ---------------------------------------------------------------

# Trim down the locations_reversed_joined to the relevant columns.
locations_master <-locations_reversed_joined %>% 
  select(locationID,
         ancientcity,
         ancientregion,
         country,
         lat,
         longi)

# Use geo_inner_join to find attestations based on lat/long - 
# this will take several seconds.
locations_join <- locations_master %>% 
  filter(!is.na(lat)) %>% 
  rename("orig_lat" = lat,
         "orig_long" = longi) %>% 
  geo_inner_join(
    pleiades_from_json$pleiades_locations %>% 
      filter(!is.na(long)),
    by = c("orig_long" = "long",
           "orig_lat" = "lat"),
    unit = c("km"),
    max_dist = 2,
    distance_col = "distance_from_original")

# Annoying that there aren't any names. Let's pull them over.
# As before:
plei_ids <- pleiades_full_json$`@graph`$id

# For now just grab the titles.
glimpse(pleiades_full_json$`@graph`$title)
plei_titles <- pleiades_full_json$`@graph`$title

# Pull the 2 sets together.
pleiades_titles <- tibble(
  id = plei_ids,
  plei_title = plei_titles
) 

# Join the titles to the fuzzy join so we can see what the names of matched places are.
locations_full_join_w_titles <- locations_join %>% 
  left_join(
    pleiades_titles,
    by = join_by(id == id))

# Add these to the big list real quick.
pleiades_from_json_cleaned_tidied[["pleiades_titles"]] <- pleiades_titles

# Save the list locally.
saveRDS(pleiades_from_json_cleaned_tidied, paste0(objects_directory, "pleiades_from_json_cleaned_tidied.rds"))


# Exploring the Data ------------------------------------------------------

# Who has the most citations?
locations_full_join_w_titles %>% 
  group_by(
    ancientcity,
    country,
    orig_lat,
    orig_long
  ) %>% 
  summarize(n_plei_records = n_distinct(id)) %>% 
  arrange(desc(n_plei_records))

# Very cool. This is a good starting point to start research. But I feel like some obvious locations should be
# in these top records. Like Rome or Constantinople. I'm going to widen the distance of the inner join.
locations_5km <- locations_master %>% 
  filter(!is.na(lat)) %>% 
  rename("orig_lat" = lat,
         "orig_long" = longi) %>% 
  geo_inner_join(
    pleiades_from_json$pleiades_locations %>% 
      filter(!is.na(long)),
    by = c("orig_long" = "long",
           "orig_lat" = "lat"),
    unit = c("km"),
    max_dist = 5,
    distance_col = "distance_from_original"
  )

# Check totals again.
locations_5km %>% 
group_by(
  ancientcity,
  country,
  orig_lat,
  orig_long
) %>% 
  summarize(n_plei_records = n_distinct(id)) %>% 
  arrange(desc(n_plei_records))

# Check at 10km. This time we perform all the steps at once.
locations_master %>% 
  filter(!is.na(lat)) %>% 
  rename("orig_lat" = lat,
         "orig_long" = longi) %>% 
  geo_inner_join(
    pleiades_from_json$pleiades_locations %>% 
      filter(!is.na(long)),
    by = c("orig_long" = "long",
           "orig_lat" = "lat"),
    unit = c("km"),
    max_dist = 10,
    distance_col = "distance_from_original"
  ) %>% 
  
  group_by(
    ancientcity,
    country,
    orig_lat,
    orig_long
  ) %>% 
  summarize(n_plei_records = n_distinct(id)) %>% 
  arrange(desc(n_plei_records))

# Which ID's did not get matches?
no_matches <- locations_master %>% 
  left_join(locations_5km %>% 
    group_by(
      locationID) %>% 
      summarize(n_plei_records = n_distinct(id),
                .groups = "drop"),
    by = join_by(locationID == locationID)) %>% 
  filter(is.na(n_plei_records))

# Save Point --------------------------------------------------------------

saveRDS(locations_5km, paste0(objects_directory,"locations_5km.rds"))
saveRDS(locations_master, paste0(objects_directory,"locations_master.rds"))
saveRDS(no_matches, paste0(objects_directory,"no_matches.rds")) 

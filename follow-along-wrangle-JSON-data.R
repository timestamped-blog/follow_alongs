
# Setup -------------------------------------------------------------------
library(jsonlite)
library(tidyverse)
library(tibble)

main_project_directory <- paste0("F:/Everything/art/Early Xty/R/")
objects_directory <- paste0(main_project_directory,"objects/")
data_sets <- paste0(main_project_directory,"data/")

# Read in the json file. It is a large file and will take awhile ~10mins
# If you have not, you can download the latest JSON data here: 
# https://atlantides.org/downloads/pleiades/json/
pleiades_full_json <- read_json("F:/Everything/art/Early Xty/R/data/pleiades-places.json",
                       flatten = T,
                       simplifyDataFrame = T)



# Mining References -------------------------------------------------------

# Grab all the IDs in the JSON.
plei_ids <- pleiades_full_json$`@graph`$id 

# Check out the data structure. Observe the top level elements and the
# lists nested underneath. You can also click the list object in the 
# Global Environment or use View()
glimpse(pleiades_full_json$`@graph`)
View(pleiades_full_json)

# Pull out the the id values and the corresponding references
# into their own objects.
plei_ids <- pleiades_full_json$`@graph`$id 
plei_refs <- pleiades_full_json$`@graph`$references 


# Use a tibble to combine the 2 sets. Use unnest to expand 
# the references lists.

pleiades_references <- tibble(
  id = plei_ids,
  references = plei_refs
) %>% 
  unnest(references)

# Do a spot check on 3 random ID's and verify the refs are correct.
spot_check_ids <- pleiades_references %>% 
  select(id) %>% 
  distinct() %>% 
  slice_sample(n = 3)

pleiades_references %>% 
  filter(id %in% spot_check_ids$id)


# Mining Locations --------------------------------------------------------

# Like before, grab the set and store it as an object.
plei_rep_locs <- pleiades_full_json$`@graph`$reprPoint

# Examine the data.
glimpse(pleiades_full_json$`@graph`$reprPoint)


# These are lists of 2 numerical values, which is pretty straightforward,
# but requires some manipulation and cleaning to make it usable going
# forward. All of the following can be done in one-step.

pleiades_locations_id_match <- tibble(
  id = plei_ids,
  location_data = plei_rep_locs
) %>% 
  unnest_wider(location_data,
               names_sep = "_") %>% 
  rename("long" = location_data_1,
         "lat" = location_data_2)


# Save Point --------------------------------------------------------------

# Save my sets to a single list for easy use in the future.

# Create a blank list.
pleiades_from_json_cleaned_tidied <- list()

# Add to the list.
pleiades_from_json_cleaned_tidied[["pleiades_locations"]] <- pleiades_locations_id_match
pleiades_from_json_cleaned_tidied[["pleiades_references"]] <- pleiades_references

# Save the list.
saveRDS(pleiades_from_json_cleaned_tidied, paste0(objects_directory, "pleiades_from_json_cleaned_tidied.rds"))






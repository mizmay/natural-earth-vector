library(sf)
library(WikidataR)
library(tidyverse)

setwd("~/Repos/natural-earth-vector/10m_cultural/")

# Helper functions

not.defined = function(x) {
  is.null(x) || is.na(x) || is.nan(x) || x == ''
}

get_locals <- function(wd_id, iso_codes) {
  items <- get_item(wd_id)
  locals <- t(sapply(items,function(x) {
    sapply(iso_codes, function(iso) {
      item <- NA
      item <- x$labels[[iso]]$value
      ifelse(not.defined(item), NA, item)
    })
  }))
  data.frame(locals)
}

compare_labels <- function(ne_label,wiki_label) {
  ifelse(as.character(ne_label) != as.character(wiki_label) 
         || is.na(as.character(ne_label)), 
         as.character(wiki_label), NA)
}

## IMPORT NE DATA

# Read in ne_10m_populated_places shapefile
ne_10m_populated_places <- st_read('ne_10m_populated_places.shp')

# Grab all the field names in the ne_10m_populated_places attribute table
# that contain place names and thus could be used as label fields (case insensitive)
ne_10m_label_fields <- names(ne_10m_populated_places %>% 
                               st_drop_geometry() %>% 
                               select(contains("name")))

# Keep only:
# "NAME"      "NAMEPAR"   "NAMEALT"   "NAMEASCII" "MEGANAME"  "LS_NAME" 
# and localizations
ne_10m_label_fields <- ne_10m_label_fields[c(1:4,10,11, grep("name_", ne_10m_label_fields))]

# Extract the two-digit ISO codes from the fields containing localized names,
# these are formatted as "name_en", name_es", etc.

ne_10m_label_fields.localizations <- sub("name_", "", grep("name_", ne_10m_label_fields, value = TRUE))

## INPUT NAMES FROM GITHUB ISSUES

# Build a table of revision suggestions from Github issues
issue <- c("430","430","428","413","399","398","381","380","374","369","366","336","335","329","329","329")
NAME <- c("Shijianzhuang","Xiangfan", "Savetskaya Gavan", "Astana","Puebla","Ujungpandang","Rangoon","Sevastapol","AmundseniScott South Pole Station","Lupanshui","Akureyi","Bandar Lampung","Kailua-Kona","Chittagong","Jessore","Barisal")
suggested <- c("Shijiazhuang","Xiangyang","Sovetskaya Gavan", "Nur-Sultan","Puebla","Makassar","Yangon","Sevastopol","Amundsen-Scott South Pole Station","Liupanshui","Akureyri","Bandar Lampung","Kailua-Kona","Chattogram","Jashore","Barishal")
ne_names_issues = data.frame(issue,NAME,suggested)

# LOOK UP WIKIDATA

# Look up and append wikidata localizations to github issues
wikidata_lookup <- ne_10m_populated_places %>%
  st_drop_geometry() %>%
  right_join(ne_names_issues, by = "NAME") %>%
  select(NAME,issue,suggested,wikidataid) %>%
  filter(!is.na(wikidataid)) %>%
  distinct(wikidataid, .keep_all = TRUE)

# Filter to the localizations included in Natural Earth
wikidata_localizations <- get_locals(wikidata_lookup$wikidataid,ne_10m_label_fields.localizations)
wikidata_results <- data.frame(wikidata_lookup, wikidata_localizations)

# COMPARE WIKIDATA AND NATURAL EARTH

# Compare Wikidata labels to Natural Earth "name" fields
wikidata_concordance <- ne_10m_populated_places %>%
  st_drop_geometry() %>%
  right_join(wikidata_results, by = c("wikidataid","NAME")) %>%
  select(wikidataid, any_of(ne_10m_label_fields), any_of(ne_10m_label_fields.localizations)) %>%
  distinct(wikidataid, .keep_all = TRUE) %>%
  pivot_longer(cols = !wikidataid, names_to = "name_field", values_to = "place_name") %>%
  mutate(
    source = if_else(nchar(name_field) == 2, "wikidata","ne"),
    name_field = str_replace(name_field, "name_", "")
  ) %>%
  arrange(wikidataid, name_field, source) %>% 
  drop_na() %>% 
  pivot_wider(names_from = "source", values_from = "place_name") %>%
  left_join(wikidata_results[c("wikidataid","en")], by = "wikidataid")

# For "name" fields that are not localizations, use "en" value
# (tidyverse solutions aren't working)
wikidata_concordance$wikidata[is.na(wikidata_concordance$wikidata)] <- wikidata_concordance$en[is.na(wikidata_concordance$wikidata)]

wikidata_concordance <- wikidata_concordance %>%
  rowwise() %>%
  mutate(
    recommend_update = compare_labels(ne,wikidata))

# Reformat so that each label field is it's own column
export_data <- wikidata_concordance %>%
  select(wikidataid,name_field,recommend_update) %>%
  mutate( 
    name_field = if_else(nchar(name_field) == 2, 
                     paste("name_", name_field, sep =""),
                     name_field) ) %>%
  pivot_wider(id_cols = "wikidataid", names_from = "name_field", values_from = "recommend_update") %>%
  relocate(any_of(ne_10m_label_fields))

# Build a table of the recommended changes only
export_data_csv <- ne_10m_populated_places %>%
  st_drop_geometry() %>%
  select(wikidataid, LATITUDE, LONGITUDE) %>%
  right_join(export_data, by = "wikidataid")

write_csv(export_data_csv, "ne_10m_populated_places_wikidata_updates", na="")
write_csv(wikidata_results, "ne_names_issues_compare_wikidata", na="")

# Build a 
# export_data_shapefile <- ne_10m_populated_places %>%
#   select(-one_of(names(export_data)[2:length(export_data)])) %>%
#   right_join(export_data, by = 'wikidataid')

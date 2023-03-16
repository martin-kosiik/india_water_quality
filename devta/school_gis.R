library(tidyverse)
library(here)
library(sf)
library(mapview)
library(ggspatial)
library(DBI)
library(RSQLite)

setwd(here::here())




read_shapefile <- function(zipfile){
  temp <- tempfile()
  unzip(zipfile = str_c( "data/india_2001_census_map/", zipfile), exdir = temp)
  SHP_file <-list.files(temp, pattern = ".shp$",full.names=TRUE)
  village_map <- sf::read_sf(SHP_file)
  return(village_map)
}

village_map <- read_shapefile('india-india-village-level-geospatial-socio-econ-1991-2001-up-2001-shp.zip')

source('devta/merge_census_map_with_shrug.R')


devta_vill <- village_map %>% 
  filter(pc01_state_name == "uttar pradesh") %>% 
  filter(pc01_district_name %in% c("lucknow", "rae bareli", "unnao", "kanpur dehat", "kanpur nagar", "hardoi",
                                   "kheri", "sitapur"))

rm(village_map)
gc()

udise_schools <- read_csv("data/UDISEplus/udise_schools.csv")

con <- dbConnect(drv=RSQLite::SQLite(), dbname="data/DISE database/pythonsqlite.db")
dbListTables(con)
test_results <- dbGetQuery(con, 'SELECT * FROM joined_table')
basic_data <- dbGetQuery(con, "SELECT * FROM basic_data WHERE state_name='Uttar Pradesh' AND AC_YEAR='2017-18'")

dbDisconnect(con)


udise_up <- udise_schools %>% 
  filter(stname == "Uttar Pradesh")


# You can check e.g., SCHOOL_CODE (or schcd) "9071001809" to see that they are the same school across the datasets


basic_data <- basic_data %>% 
  mutate(SCHOOL_CODE_num = as.numeric(SCHOOL_CODE))

merged_df <- basic_data %>% 
  semi_join(udise_up, by = c('SCHOOL_CODE_num' = 'schcd'))

merged_df <- basic_data %>% 
  left_join(udise_up, by = c('SCHOOL_CODE_num' = 'schcd'))



c("lucknow", "rae bareli", "unnao", "kanpur dehat", "kanpur nagar", "hardoi",
  "kheri", "sitapur")

devts_schools_merged <- merged_df %>% 
  filter(DISTNAME %in% c('LUCKNOW', 'RAE BARELI', 'UNNAO', 'KANPUR DEHAT', 'KANPUR NAGAR',
                         'HARDOI', 'KHERI', 'SITAPUR'))
  

devta_schools_merged_sf <- devts_schools_merged %>% 
  filter(!is.na(lon), !is.na(lat))

sum(is.na(devts_schools_merged$id))/nrow(devts_schools_merged)


devta_schools_merged_sf <- st_as_sf(devta_schools_merged_sf, coords = c('lon', 'lat'), crs = 4326)
devta_schools_merged_sf <- st_set_crs(devta_schools_merged_sf,  4326)



mapview(devta_schools_merged_sf)



#########################################
# Fuzzy string matching of village names

library(reticulate)

source_python('indian_names_string_matching_edited.py')

matched_df <- many_to_many_match(input_df_1, input_df_2)
matched_df


census_dist_names <- tibble('group_id' = '1', 'name' = devta_vill$pc01_district_name) %>% 
  distinct()

census_village_names <- tibble('group_id' = devta_vill$pc01_district_name, 'name' = devta_vill$NAME)

dise_dist_names <- tibble('group_id' = '1', 'name' = basic_data$DISTNAME) %>% 
  distinct()

matched_dist <- many_to_many_match(census_dist_names, dise_dist_names) %>% 
  left_join_in_min_lev_dist()


left_join_in_min_lev_dist <- function(matched_df){
  output_df <- matched_df %>% 
    group_by(group_id, name_1) %>% 
    mutate(min_lev_dist = min(lev_dist) == lev_dist) %>% 
    ungroup() %>% 
    filter(min_lev_dist) %>% 
    dplyr::select(-min_lev_dist)
  
  return(output_df)
}


census_village_names_unnao <- census_village_names %>% 
  filter(group_id == "unnao")

census_village_names_unnao

dise_village_names_unnao <- tibble('group_id' = basic_data$DISTNAME %>% str_to_lower(), 'name' = basic_data$VILLAGE_NAME) %>% 
  distinct() %>% 
  filter(group_id == "unnao")


matched_villages_unnao <- many_to_many_match(census_village_names_unnao, dise_village_names_unnao)


matched_villages_unnao_best <- matched_villages_unnao %>% 
  left_join_in_min_lev_dist()


matched_villages_unnao_best %>% 
  filter(lev_dist < 1)

matched_villages_unnao %>% 
  filter(lev_dist < 1)


basic_data

census_dist_names


# Try to find correspondence between udise village, cluster and block codes and those from local gov directory


names(udise_up)
# 2011 census codes: dtcode11,  

udise_up_dist_codes <- udise_up %>% 
  distinct(dtcode11, .keep_all = TRUE)

udise_up_dist_codes <- udise_up %>% 
  distinct(dtcode11, dtname_1, udise_dtco, dtname, .keep_all = F)

udise_up_dist_codes <- udise_up %>% 
  count(dtcode11, dtname_1, udise_dtco, dtname)


str(test_results)

village_directory <- read_delim("data/village-directory.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(`Local  Government DirectoryState Code` = col_integer(), 
                                                                                     `Census  2011 Code` = col_integer(), 
                                                                                     `District Code` = col_integer(), 
                                                                                     `Census 2011 Code...6` = col_integer(), 
                                                                                     `Sub district Code` = col_integer(), 
                                                                                     `Village Code` = col_integer(), `Local Body Code` = col_integer(), 
                                                                                     `Local Body Type Code` = col_integer(), 
                                                                                     `Block Code` = col_integer()), trim_ws = TRUE)
village_directory$vill_name <- str_to_lower(village_directory$`Village Name (In English)`)

names(village_directory)

village_directory$`Census 2011 Code...6` <- as.integer(village_directory$`Census 2011 Code...6`)
village_directory$`Census  2011 Code` <- as.integer(village_directory$`Census  2011 Code` )


udise_schools_merged <- udise_schools %>% 
  mutate(vill_name = str_to_lower(vilname),
         stcode11 = as.integer(stcode11),
         dtcode11 = as.integer(dtcode11) )%>% 
  inner_join(village_directory , by = c('stcode11' = 'Census  2011 Code', 'dtcode11' = 'Census 2011 Code...6', 'vill_name'))


udise_schools_merged <- udise_schools_merged %>% 
  add_count(id)


udise_schools_merged %>% 
  count(n)

udise_schools_merged <- udise_schools_merged %>% 
  filter(n == 1)


village_directory



test_results <- test_results %>% 
  mutate(SCHOOL_CODE_num = as.numeric(SCHCD))


test_results_merged <- test_results %>% 
  semi_join(udise_schools_merged, by = c('SCHOOL_CODE_num' = 'schcd'))
nrow(test_results_merged)

test_results_merged <- test_results %>% 
  inner_join(udise_schools_merged, by = c('SCHOOL_CODE_num' = 'schcd'))

test_results_merged$`Local Body Name (In English)`

names(test_results_merged)

by_gp <- test_results_merged %>% 
  group_by(AC_YEAR, `Local  Government DirectoryState Code`, `State Name (In English)`, `District Code`, `District Name (In English)`,
           `Sub district Code`, `Subdistrict Name (In English)`, `Local Body Code`, `Local Body Name (In English)`, `Local Body Type Name`, ) %>% 
  summarize(tot_enrol_boys = sum(C1_TOTB, na.rm = T), tot_enrol_girls = sum(C1_TOTG, na.rm = T), n_schools = n())


by_gp %>% 
  write_csv('data/grade_1_enrol_by_gp_and_year.csv')





con_2 <- dbConnect(drv=RSQLite::SQLite(), dbname="C:\\Users\\marti\\Downloads\\india-local-government-directory.sqlite3")
dbListTables(con_2)
village_codes <- dbGetQuery(con_2, 'SELECT * FROM villages')






by_gp <- read_csv('data/grade_1_enrol_by_gp_and_year.csv')

by_gp

sbm_panch <- read_csv('data/targ_vs_achieved.csv')


sbm_panch <- sbm_panch %>% 
  mutate(total_pre_treat_toilet = total_hh_entered - no_toilet_alp_blp,
         pre_toilet_share = total_pre_treat_toilet/total_hh_entered,
         entered_2012 = pre_toilet_share)

sbm_panch_long <- sbm_panch %>% 
  pivot_longer(starts_with('entered_'), names_to = 'year', values_to = 'entered')

sbm_panch_long <- sbm_panch_long %>% 
  mutate(
    entered_pct = entered/total_hh_entered,
    entered_pct_progress = entered/hh_covered_after_bls,
    year = as.numeric(str_remove(year, 'entered_')))

sbm_panch_long <- sbm_panch_long %>% 
  arrange(data_level, state_name, district_name, block_name, panch_name, year)

sbm_panch_long <- sbm_panch_long %>% 
  group_by(data_level, state_name, district_name, block_name, panch_name)%>% 
  mutate(cum_toilet_coverage_pct = cumsum(entered_pct_progress))

options(scipen = 999)

sbm_panch_long <- sbm_panch_long %>% 
  mutate(state_name = str_to_lower(str_sub(state_name, start = 13)),
         district_name = str_to_lower(str_sub(district_name, start = 16)),
         panch_name = str_to_lower(panch_name))


by_gp <- by_gp %>% 
  mutate(year = as.integer(str_sub(AC_YEAR, end = -4))) %>% 
  mutate_at(c('State Name (In English)', 'District Name (In English)', 'Local Body Name (In English)'), str_to_lower)


"Village Panchayat"
by_gp$`Local Body Type Name`

by_gp <- by_gp %>% 
  filter(by_gp$`Local Body Type Name` == 'Village Panchayat')
  


merged_df <- sbm_panch_long %>% 
  inner_join(by_gp, by = c('state_name' = 'State Name (In English)', 'district_name' = 'District Name (In English)',
                          'panch_name' = 'Local Body Name (In English)', 'year'))


names(by_gp)

library(tidyverse)
library(here)
library(janitor)
library(sf)
library(mapview)
library(ggspatial)
setwd(here::here())

#install.packages('mapview')

#village_map <- sf::read_sf("data\\india_2001_census_map\\all_india\\all_india_village_level_map.shp")

read_shapefile <- function(zipfile){
  temp <- tempfile()
  unzip(zipfile = str_c( "data/india_2001_census_map/", zipfile), exdir = temp)
  SHP_file <-list.files(temp, pattern = ".shp$",full.names=TRUE)
  village_map <- sf::read_sf(SHP_file)
  return(village_map)
}


village_map <- read_shapefile('india-india-village-level-geospatial-socio-econ-1991-2001-up-2001-shp.zip')

up_2011_village_map <- sf::read_sf("data\\official_india_2011_village_boundary_polygons-master\\up\\villages_09.shp")
up_2011_missing_name <- sf::read_sf("data\\official_india_2011_village_boundary_polygons-master\\up\\missing_name_UTTARPRADESH.shp")


unnao_village_map <- up_2011_village_map %>% 
  filter(n_dt2011_0 %in% c('156'))

plot(unnao_village_map$geometry)

######################################################
# Merging with SRUG 
###############################
library(readr)
shrug_pc01r_keys <- read_csv("C:/Users/marti/Dropbox/research_projects_prelim/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01r_key.csv") 

shrug_pc01r_keys <- shrug_pc01r_keys %>%
  mutate(pc01_state_id  = as.numeric(pc01_state_id),
         pc01_district_id  = as.numeric(pc01_district_id),
         pc01_subdistrict_id  = as.numeric(pc01_subdistrict_id),
         pc01_village_id  = as.numeric(pc01_village_id),)

#shrug_secc <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-secc-csv/shrug_secc.csv")

village_map <- village_map %>% 
  left_join(shrug_pc01r_keys, by = c('STATE' = 'pc01_state_id', 'DISTRICT' = 'pc01_district_id', 
                                     'SUB_DIST' = 'pc01_subdistrict_id', 'TOWN_VILL' = 'pc01_village_id'))


names(list_of_maps)[1:19]

#census_shrid <- village_map #%>% dplyr::select(1:19, shrid)



shrug_pc01subdistrict_keys <- read_csv("C:/Users/marti/Dropbox/research_projects_prelim/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01_subdistrict_key.csv") 
shrug_pc01subdistrict_keys_unique <- shrug_pc01subdistrict_keys %>% 
  distinct(pc01_state_id, pc01_state_name, pc01_district_id, pc01_district_name, pc01_subdistrict_id, pc01_subdistrict_name)

shrug_pc01subdistrict_keys_unique <- shrug_pc01subdistrict_keys_unique %>%
  mutate(pc01_state_id  = as.numeric(pc01_state_id),
         pc01_district_id  = as.numeric(pc01_district_id),
         pc01_subdistrict_id  = as.numeric(pc01_subdistrict_id))



village_map <- village_map %>% 
  left_join(shrug_pc01subdistrict_keys_unique, by = c('STATE' = 'pc01_state_id', 'DISTRICT' = 'pc01_district_id', 
                                                      'SUB_DIST' = 'pc01_subdistrict_id'))

rm(list = c('shrug_pc01r_keys', 'shrug_pc01subdistrict_keys', 'shrug_pc01subdistrict_keys_unique'))
gc()


unique(village_map$pc01_state_name)

unique(village_map$pc01_district_name)


village_map %>% 
  distinct(pc01_district_name)

names(village_map)

devta_vill <- village_map %>% 
  filter(pc01_state_name == "uttar pradesh")

unique(devta_vill$pc01_district_name)

devta_vill <- devta_vill %>% 
  filter(pc01_district_name %in% c("lucknow", "rae bareli", "unnao", "kanpur dehat", "kanpur nagar", "hardoi",
                                   "kheri", "sitapur"))

class(devta_vill$CWC)

plot(devta_vill$geometry)


devta_vill %>% 
  mutate(number_of_awc = as.factor(CWC)) %>%
  ggplot() +
  #annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = number_of_awc)) +
  ggtitle(label = "Fluoride contamination of drinking water")+
  #annotation_scale() +
  labs(fill = "Fluoride in water (mg/L)")
#  scale_fill_gradient2(low = "black", mid = "yellow",high = "red", midpoint = 1.5#, limits = c(0,0.15), oob = scales::squish  )
#♦scale_fill_viridis_c(option = "magma")

devta_vill %>% 
  mutate(number_of_awc = as.factor(MCW_CNTR)) %>%
  ggplot() +
  #annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = number_of_awc)) +
  ggtitle(label = "Fluoride contamination of drinking water")+
  #annotation_scale() +
  labs(fill = "Fluoride in water (mg/L)")

table(devta_vill$MCW_CNTR)
table(devta_vill$M_HOME)
table(devta_vill$N_HOME)
table(devta_vill$CWC)
table(devta_vill$FWC_CNTR)
table(devta_vill$CHW)

table(devta_vill$PHS_CNT)
table(devta_vill$PH_CNTR)


#ggsave('outputs/figures/wb_districts_fluoride_water_contamination_map.pdf', scale = 1.5)


devta_vill %>% 
  mutate(number_of_awc = as.factor(FWC_CNTR)) %>%
  ggplot() +
  #annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = number_of_awc)) +
  ggtitle(label = "Fluoride contamination of drinking water")+
  #annotation_scale() +
  labs(fill = "Fluoride in water (mg/L)")


devta_vill %>% 
  mutate(number_of_awc = as.factor(N_HOME)) %>%
  ggplot() +
  #annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = number_of_awc)) +
  ggtitle(label = "Fluoride contamination of drinking water")+
  #annotation_scale() +
  labs(fill = "Fluoride in water (mg/L)")

# Make map of villages and spiillovers

names(devta_vill)


devta_blocks <- devta_vill %>% 
 # st_geometry(NULL) %>% 
  #filter(LEVEL != "TOWN") %>% 
  group_by(DIST_CODE, THSIL_CODE, BLOCK_CODE) %>% 
  summarise(total_pop = sum(TOT_P, na.rm = T))

devta_blocks %>% 
  ungroup() %>% 
  summarise(mean_pop = mean(total_pop, na.rm = T))

# 198418


devta_blocks <- devta_vill %>% 
  #filter(LEVEL != "TOWN") %>% 
  group_by(DIST_CODE, THSIL_CODE, BLOCK_CODE) %>% 
  summarise()

plot(devta_blocks$geometry)

devta_dist <- devta_vill %>% 
  group_by(DIST_CODE) %>% 
  summarise()

devta_dist <- devta_vill %>% 
  #filter(LEVEL != "TOWN") %>% 
  group_by(DISTRICT) %>% 
  summarise()


table(devta_vill$LEVEL)


ggplot(devta_vill) +
  geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = devta_blocks) +
  theme_void() +
  coord_sf(ndiscr = F)



ggplot(devta_vill) +
  geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = devta_blocks) +
  geom_sf(fill = "transparent", color = "white", size = 0, 
          data = devta_vill_with_zero_dist) +
  theme_void() +
  coord_sf(ndiscr = F)


devta_vill_with_zero_dist <- devta_vill %>% filter(DIST_CODE == 0)

devta_towns <- devta_vill %>% filter(DIST_CODE == 0, LEVEL == "TOWN")

devta_zero_dist_non_towns <- devta_vill %>% filter(DIST_CODE == 0, LEVEL != "TOWN")

plot(devta_zero_dist_non_towns$geometry)

plot(devta_vill_with_zero_dist$geometry)


devta_vill %>% 
  dplyr::select(1:17, 73:80) %>% 
  mutate(CENSUS_COD = as.character(CENSUS_COD)) %>% 
  View()


devta_vill %>% 
  mutate(not_town = case_when(DIST_CODE == 0 & LEVEL != "TOWN" ~ 'Other',
                              TRUE ~ 'Normal')) %>% 
  dplyr::select(2:17, 73:80, not_town) %>% 
  mapview(zcol = "not_town")


# NAME: MALAON, C_CODE01 == '0926000203488100' or  NAME: BARWA, C_CODE01 == '0926000203488200'   then
# ST_CODE	9
# DIST_CODE	26
# THSIL_CODE	2
# BLOCK_CODE 298


# NAME: ASAIPUR, C_CODE01 == '0928000703832100' or  NAME: RAIPUR MAHEWA, C_CODE01 == '0928000703832000'   then
# ST_CODE	9
# DIST_CODE	28
# THSIL_CODE	7
# BLOCK_CODE 335

str(devta_vill)
devta_vill <- devta_vill %>% 
  mutate(DIST_CODE = case_when(C_CODE01 == '0926000203488100' | C_CODE01 == '0926000203488200' ~ 26,
                               C_CODE01 == '0928000703832100' | C_CODE01 == '0928000703832000' ~ 28,
                               TRUE ~ DIST_CODE),
         THSIL_CODE = case_when(C_CODE01 == '0926000203488100' | C_CODE01 == '0926000203488200' ~ 2,
                               C_CODE01 == '0928000703832100' | C_CODE01 == '0928000703832000' ~ 7,
                               TRUE ~ THSIL_CODE),
         BLOCK_CODE = case_when(C_CODE01 == '0926000203488100' | C_CODE01 == '0926000203488200' ~ 298,
                                C_CODE01 == '0928000703832100' | C_CODE01 == '0928000703832000' ~ 335,
                                TRUE ~ BLOCK_CODE))


ggplot(devta_vill) +
 # geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = devta_blocks) +
#  geom_sf(fill = "transparent", color = "white", size = 1.5,  data = devta_vill_with_zero_dist) +
  theme_void() +
  coord_sf(ndiscr = F)


ggplot(devta_vill) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = devta_blocks) +
  #  geom_sf(fill = "transparent", color = "white", size = 1.5,  data = devta_vill_with_zero_dist) +
  geom_sf(fill = "yellow", color = "transparent", size = 1.5,  data = devta_vill_with_zero_dist) +
#  geom_sf(fill = "transparent", color = "red", size = 1.2, data = devta_dist) +
    theme_void() +
  annotation_scale() +
  annotation_north_arrow(location = 'tl')+
  coord_sf(ndiscr = F)

ggsave('devta/figures/blocks_with_towns.pdf')

ggplot(devta_vill) +
  geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
  geom_sf(fill = "transparent", color = "gray20", size = 1, 
          data = devta_blocks) +
  #  geom_sf(fill = "transparent", color = "white", size = 1.5,  data = devta_vill_with_zero_dist) +
  geom_sf(fill = "yellow", color = "transparent", size = 1.5,  data = devta_vill_with_zero_dist) +
  theme_void() +
  annotation_scale() +
  annotation_north_arrow(location = 'tl')+
  coord_sf(ndiscr = F)

ggsave('devta/figures/villages_and_blocks_with_towns.pdf', scale = 2)






#################################################

# Spillovers
##############################################
# think if you are using the right projection for calculation of the centroids

devta_vill$centroid <- st_centroid(devta_vill$geometry)

plot(devta_vill$centroid)

devta_blocks_filter <- devta_blocks %>% 
  filter(!BLOCK_CODE %in% c(0, 8888)) %>% 
  ungroup()

plot(devta_blocks$geometry)


devta_blocks_filter_str <- st_cast(devta_blocks_filter, 'MULTILINESTRING')
min(st_distance(devta_vill$centroid[1000], devta_blocks_filter_str))


get_dist_to_block_border <- function(x){
  
  
  dist_id <- devta_vill$DIST_CODE[x]
  thsil_id <- devta_vill$THSIL_CODE[x]
  block_id <- devta_vill$BLOCK_CODE[x]
  
  devta_block_filter_str <- devta_blocks_filter_str %>% 
    filter(DIST_CODE == dist_id, THSIL_CODE == thsil_id, BLOCK_CODE == block_id)
  
  final_dist <-  st_distance(devta_vill$centroid[x], devta_block_filter_str)[1,1]
  
  return(final_dist)
  
}


get_dist_to_block_border <- function(x){
  
  tryCatch(
    expr = {
      
      
      dist_id <- devta_vill$DIST_CODE[x]
      thsil_id <- devta_vill$THSIL_CODE[x]
      block_id <- devta_vill$BLOCK_CODE[x]
      
      devta_block_filter_str <- devta_blocks_filter_str %>% 
        filter(DIST_CODE == dist_id, THSIL_CODE == thsil_id, BLOCK_CODE == block_id)
      
      final_dist <-  st_distance(devta_vill$centroid[x], devta_block_filter_str)[[1,1]]
      
      return(final_dist)
    },
    
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
      final_dist <- -99
      return(final_dist)
      
      
    }
  )
  
}


get_dist_to_block_border(20)

map_dbl(1:100, get_dist_to_block_border )


map_dbl(1:100, ~ min(st_distance(devta_vill$centroid[.x], devta_blocks_filter_str)) )

devta_vill$dist_to_block_border <- map_dbl(1:nrow(devta_vill), get_dist_to_block_border )

saveRDS(devta_vill$dist_to_block_border, file = "devta/dist_to_block_border.RData")

devta_vill$dist_to_block_border <- readRDS("devta/dist_to_block_border.RDS")


sum(devta_vill$dist_to_block_border == -99)

devta_vill <- devta_vill %>% 
  mutate(within_1.5km = ifelse(dist_to_block_border < 1500 & dist_to_block_border !=-99, 1, 0))

devta_vill_near_border <- devta_vill %>% 
  filter(within_1.5km == 1)


ggplot(devta_vill) +
  geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
  #  geom_sf(fill = "transparent", color = "white", size = 1.5,  data = devta_vill_with_zero_dist) +
  geom_sf(fill = "yellow", color = "transparent", size = 1,  data = devta_vill_near_border) +  
  geom_sf(fill = "transparent", color = "gray20", size = 1,           data = devta_blocks) +
  theme_void() +
  annotation_scale() +
  annotation_north_arrow(location = 'tl')+
  coord_sf(ndiscr = F)

ggplot(devta_blocks) +
  geom_sf(fill = "gray95", color = "gray50", size = 0.5) +
  #  geom_sf(fill = "transparent", color = "gray20", size = 1,           data = devta_blocks) +
  #  geom_sf(fill = "transparent", color = "white", size = 1.5,  data = devta_vill_with_zero_dist) +
  geom_sf(fill = "yellow", color = "transparent", size = 1.5,  data = devta_vill[1, ]) +
  theme_void() +
  annotation_scale() +
  annotation_north_arrow(location = 'tl')+
  coord_sf(ndiscr = F)


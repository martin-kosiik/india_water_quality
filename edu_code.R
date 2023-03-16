library(tidyverse)
library(here)
library(janitor)
library(rdrobust)
library(sf)
library(mapview)
setwd(here::here())


census_shrid <- read_csv('data/matching_village_codes/census_2001_codes_with_shrid.csv')

census_shrid %>% 
  filter(is.na(shrid), STATE != 0) %>% 
  View()

census_shrid %>% 
  filter(is.na(shrid), STATE != 0, LEVEL != 'TOWN', TOT_P !=0, !is.na(LEVEL)) %>% 
  count(SID, DID, sort = T)


shrug_pc01r_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01r_key.csv") 

shrug_pc01u_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01u_key.csv") 

shrug_pc01u_keys <- shrug_pc01u_keys %>% 
  mutate(pc01_state_id = as.numeric(pc01_state_id)) %>% 
  rename(shrid_town = shrid)


sum(is.na(census_shrid$shrid))

census_shrid <- census_shrid %>% 
  left_join(shrug_pc01u_keys, by = c('STATE' = 'pc01_state_id',  'TOWN_VILL' = 'pc01_town_id')) %>% 
  mutate(shrid = ifelse((!is.na(shrid_town)) & is.na(shrid), shrid_town, shrid ))
  




shrug_pc01r_keys <- shrug_pc01r_keys %>%
  mutate(pc01_state_id  = as.numeric(pc01_state_id),
         pc01_district_id  = as.numeric(pc01_district_id),
         pc01_subdistrict_id  = as.numeric(pc01_subdistrict_id),
         pc01_village_id  = as.numeric(pc01_village_id),)






village_map <- sf::read_sf("data\\india_2001_census_map\\all_india\\all_india_village_level_map.shp")


village_map_no_geom <- village_map
village_map_no_geom$geometry <- NULL

village_map_no_geom <- village_map_no_geom %>% 
  left_join(shrug_pc01r_keys, by = c('STATE' = 'pc01_state_id', 'DISTRICT' = 'pc01_district_id', 
                                     'SUB_DIST' = 'pc01_subdistrict_id', 'TOWN_VILL' = 'pc01_village_id'))


village_map_no_geom <- village_map_no_geom %>% 
  left_join(shrug_pc01u_keys, by = c('STATE' = 'pc01_state_id',  'TOWN_VILL' = 'pc01_town_id')) %>% 
  mutate(shrid = ifelse((!is.na(shrid_town)) & is.na(shrid), shrid_town, shrid ))



library(haven)
shrug_pc11 <- read_dta("C:/Users/marti/OneDrive/Plocha/research_projects/hyderabad/data/shrug/shrug-v1.5.samosa-pop-econ-census-dta/shrug_pc11.dta")

shrug_pc11 <- shrug_pc11 %>% 
  mutate(pc11_p_sch = ifelse(is.na(pc11_vd_p_sch), pc11_td_p_sch, pc11_vd_p_sch),
         pc11_m_sch = ifelse(is.na(pc11_vd_m_sch), pc11_td_m_sch, pc11_vd_m_sch),
         pc11_s_sch = ifelse(is.na(pc11_vd_s_sch), pc11_td_s_sch, pc11_vd_s_sch),
         pc11_s_s_sch = ifelse(is.na(pc11_vd_s_s_sch), pc11_td_s_s_sch, pc11_vd_s_s_sch),
         all_schools = pc11_p_sch + pc11_m_sch + pc11_s_sch + pc11_s_s_sch,
         all_secondary_schools = pc11_s_sch + pc11_s_s_sch)
  

shrug_pc11 <- shrug_pc11 %>% 
  dplyr::select(shrid, pc11_p_sch, pc11_m_sch, pc11_s_sch, pc11_s_s_sch, all_schools, all_secondary_schools, pc11_pca_p_lit,
                pc11_pca_tot_p)


village_map_no_geom <- village_map_no_geom %>% 
  left_join(shrug_pc11, by = 'shrid') 
  



village_map_no_geom %>% 
  dplyr::select(STATE, DISTRICT, SUB_DIST, ST_CODE, DIST_CODE, THSIL_CODE, BLOCK_CODE) %>% 
  View()


block_level <- village_map_no_geom %>% 
 # group_by(STATE, DISTRICT, SUB_DIST) %>% 
  group_by(ST_CODE, DIST_CODE, THSIL_CODE, BLOCK_CODE) %>% 
  summarize_at(vars(TOT_M, TOT_F, M_06, F_06, M_LIT, F_LIT, M_ILL, F_ILL, pc11_p_sch, pc11_m_sch, pc11_s_sch, pc11_s_s_sch, all_schools, all_secondary_schools, pc11_pca_p_lit,
                    pc11_pca_tot_p), sum, na.rm = T)




shrug_pc01subdistrict_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01_subdistrict_key.csv") 
shrug_pc01subdistrict_keys_unique <- shrug_pc01subdistrict_keys %>% 
  distinct(pc01_state_id, pc01_state_name, pc01_district_id, pc01_district_name, pc01_subdistrict_id, pc01_subdistrict_name)

shrug_pc01subdistrict_keys_unique <- shrug_pc01subdistrict_keys_unique %>%
  mutate(pc01_state_id  = as.numeric(pc01_state_id),
         pc01_district_id  = as.numeric(pc01_district_id),
         pc01_subdistrict_id  = as.numeric(pc01_subdistrict_id))



block_level <- block_level %>% 
  left_join(shrug_pc01subdistrict_keys_unique, by = c('STATE' = 'pc01_state_id', 'DISTRICT' = 'pc01_district_id', 
                                                      'SUB_DIST' = 'pc01_subdistrict_id'))


block_level <- block_level %>% 
  left_join(shrug_pc01subdistrict_keys_unique, by = c('ST_CODE' = 'pc01_state_id', 'DIST_CODE' = 'pc01_district_id', 
                                                      'THSIL_CODE' = 'pc01_subdistrict_id'))



block_level <- block_level %>% 
  mutate(male_lit_rate = 100 * M_LIT/(M_LIT + M_ILL),
         female_lit_rate = 100 * F_LIT/(F_LIT + F_ILL),
         gender_lit_gap = male_lit_rate - female_lit_rate)


block_level <- block_level %>% 
  mutate(male_lit_rate_dif = 100 * M_LIT/(M_LIT + M_ILL - M_06),
         female_lit_rate_dif = 100 * F_LIT/(F_LIT + F_ILL - F_06),
         gender_lit_gap_dif = male_lit_rate_dif - female_lit_rate_dif)


block_level_filter <- block_level %>% 
  filter(ST_CODE != 0, !is.na(female_lit_rate_dif), !is.na(gender_lit_gap_dif)) %>% 
  mutate(ebb = ifelse((female_lit_rate_dif < 46.13) & (gender_lit_gap_dif > 21.56), "Treated (EBB)", "Not EBB" ))
  
# Plots
##########################################
block_level_filter %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > 0) %>% 
  ggplot(aes(x = female_lit_rate_dif, y = gender_lit_gap_dif)) + geom_point()


block_level_filter %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > -20) %>% 
  ggplot(aes(x = female_lit_rate_dif, y = gender_lit_gap_dif, col = ebb)) + geom_point()


block_level_filter %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > -20) %>% 
  ggplot(aes(x = female_lit_rate_dif)) + geom_histogram(bins = 100, col = "white") + geom_vline(xintercept = 46.13, col = "red")



block_level_filter %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > -20) %>% 
  ggplot(aes(x = gender_lit_gap_dif)) + geom_histogram(bins = 100, col = "white") + geom_vline(xintercept = 21.56, col = "red")


block_level_filter %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > -20) %>% 
  ggplot(aes(x = female_lit_rate_dif, y = gender_lit_gap_dif) ) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

##########################################


# x is female literacy rate and y is gender gap in literacy
dist_to_cutoff <- function(x, y, x_cutoff = 46.13, y_cutoff = 21.56){
  if ((x > x_cutoff) & (y < y_cutoff)) { 
    output <- sqrt((x - x_cutoff)^2 + (y - y_cutoff)^2)
  } else if ((x < x_cutoff) & (y < y_cutoff)) {
    output <- abs(y - y_cutoff)
  } else if  ((x > x_cutoff) & (y > y_cutoff)) {
    output <- abs(x - x_cutoff)
  } else {
    output <- -min(abs(x - x_cutoff), abs(y - y_cutoff))
  }
  return(output)
  }



dist_to_cutoff(40, 21)

map2_dbl(block_level_filter$female_lit_rate_dif, block_level_filter$gender_lit_gap_dif, dist_to_cutoff)

block_level_filter <- block_level_filter %>%
  rowwise() %>% 
  mutate(distance_to_cutoff = dist_to_cutoff(x = female_lit_rate_dif, y = gender_lit_gap_dif)) %>% 
  ungroup()
  

block_level_filter <- block_level_filter %>%
  ungroup() %>% 
  mutate(distance_to_cutoff = case_when(female_lit_rate_dif >  46.13 (y = gender_lit_gap_dif)))

block_level_filter <- block_level_filter %>%
  mutate(lit_rate_2011 = 100 * pc11_pca_p_lit/pc11_pca_tot_p)



block_level_filter %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > -20) %>% 
  ggplot(aes(x = female_lit_rate_dif, y = gender_lit_gap_dif, col = distance_to_cutoff)) + geom_point()+ 
  scale_color_continuous(type = "viridis") 


block_level_filter<- block_level_filter %>% 
  filter(!is.na(distance_to_cutoff), !is.na(pc11_s_sch)) %>% 
  filter(gender_lit_gap_dif < 100, gender_lit_gap_dif > -20) 


y_var <- log(block_level_filter$pc11_s_s_sch +1)
y_var <- log(block_level_filter$all_secondary_schools +1)
y_var <- block_level_filter$lit_rate_2011



est <- rdrobust(y=y_var, x=block_level_filter$distance_to_cutoff, p=1)
rdplot(y=y_var, x=block_level_filter$distance_to_cutoff, subset=-est$bws[1,1]<= block_level_filter$distance_to_cutoff & block_level_filter$distance_to_cutoff <= est$bws[1,2],
       binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1,
       title="Local quadratic regression", 
       y.label="Elevation",
       x.label="Distance to Hyderabad border (in meters) - negative within Hyderabad")

summary(est)


shrug_secc <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-secc-csv/shrug_secc.csv")





###########################################################################################
# creating the map

map_by_blocks <- village_map %>% 
  group_by(ST_CODE, DIST_CODE, THSIL_CODE, BLOCK_CODE) %>%
  summarise() %>%
  ungroup() %>% st_as_sf()


plot(map_by_blocks$geometry)


st_write(map_by_blocks, "data/india_2001_census_map/all_india/all_india_block_level_map.shp")




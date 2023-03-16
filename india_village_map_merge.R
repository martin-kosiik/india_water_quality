library(tidyverse)
library(here)
library(janitor)
library(rdrobust)
library(sf)
library(mapview)
library(ggspatial)
library(estimatr)
library(fixest)
library(DBI)
library(modelsummary)
library(stargazer)
setwd(here::here())


# Create all India village map
#################################################################
zip_files <- list.files(path = "data/india_2001_census_map")


read_shapefile <- function(zipfile){
  temp <- tempfile()
  unzip(zipfile = str_c( "data/india_2001_census_map/", zipfile), exdir = temp)
  SHP_file <-list.files(temp, pattern = ".shp$",full.names=TRUE)
  village_map <- sf::read_sf(SHP_file)
  return(village_map)
}



some_map <- read_shapefile(zip_files[3])

robust_read_shapefile <- possibly(read_shapefile, otherwise = "error")


list_of_maps <- map(zip_files[13:26], robust_read_shapefile)

list_of_maps <- map(zip_files, read_shapefile)



zip_files[13:28]

list_of_maps[[7]]$EB
summary(list_of_maps[[8]]$WARD)

list_of_maps <- list_of_maps %>% 
  map(~ .x %>% mutate_at(c('No_HH', 'AREA', 'T_HH', 'POWER_SUPL', 'A_INCEXP', 'TOT_P', 'TOT_M', 'TOT_F', 'T_P',
                           'T_M', 'T_F', 'P_06', 'M_06', 'F_06', 'P_SC', 'M_SC', 'F_SC', 'P_ST', 'M_ST', 'F_ST',
                           'P_LIT', 'M_LIT', 'F_LIT', 'P_ILL', 'M_ILL', 'F_ILL', 'TOT_WORK_P', 'TOT_WORK_F', 'TOT_WORK_M',
                           'MAINWORK_P', 'MAINWORK_F', 'MAINWORK_M', 'MAIN_CL_P', 'MAIN_CL_F', 'MAIN_CL_M',
                           'MAIN_AL_P', 'MAIN_AL_M', 'MAIN_AL_F', 'MAIN_HH_P', 'MAIN_HH_M', 'MAIN_HH_F',
                           'MAIN_OT_P', 'MAIN_OT_M', 'MAIN_OT_F', 'MARGWORK_P', 'MARGWORK_F', 'MARGWORK_M',
                           'MARG_CL_P', 'MARG_CL_M', 'MARG_CL_F', 'MARG_AL_P', 'MARG_AL_M', 'MARG_AL_F',
                           'MARG_HH_P', 'MARG_HH_M', 'MARG_HH_F', 'MARG_OT_P', 'MARG_OT_M', 'MARG_OT_F',
                           'NON_WORK_P', 'NON_WORK_M', 'NON_WORK_F', 'SC_P', 'SC_M', 'SC_F',
                           'RANG_BS',
                           'RANG_SS', 'POST_OFF', 'DIST_TOWN', 'POWER_AGR',
                           'TOT_INC', 'LAND_FORES', 'CANAL_GOVT', 'TOT_EXP'), as.numeric) %>% 
        mutate_at(c('SS_CODE', 'MAN_COMM1', 'WARD', 'EB'), as.character)) %>% 
  bind_rows()



list_of_maps_head <- list_of_maps[1:3000,]



## it will guess the driver automatically based on the .shp extension
st_write(list_of_maps, "data/india_2001_census_map/all_india/all_india_village_level_map.shp")

st_write(list_of_maps_head, "data/india_2001_census_map/all_india/all_india_village_level_map_head.shp")

list_of_maps_head_read <- sf::read_sf("data/india_2001_census_map/all_india/all_india_village_level_map_head.shp")

# https://gis.stackexchange.com/questions/356531/r-sf-aggregate-polygons-at-various-geographic-levels

by_district <- list_of_maps %>% 
  group_by(SID, DID) %>%
  summarise() %>%
  ungroup() %>% st_as_sf()


by_district %>% mapview()

plot(by_district$geometry)
###############################################

village_map <- sf::read_sf("data\\india_2001_census_map\\all_india\\all_india_village_level_map.shp")


######################################################
# Merging with SRUG 
###############################
library(readr)
shrug_pc01r_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01r_key.csv") 

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

census_shrid <- village_map #%>% dplyr::select(1:19, shrid)
  


shrug_pc01subdistrict_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01_subdistrict_key.csv") 
shrug_pc01subdistrict_keys_unique <- shrug_pc01subdistrict_keys %>% 
  distinct(pc01_state_id, pc01_state_name, pc01_district_id, pc01_district_name, pc01_subdistrict_id, pc01_subdistrict_name)

shrug_pc01subdistrict_keys_unique <- shrug_pc01subdistrict_keys_unique %>%
  mutate(pc01_state_id  = as.numeric(pc01_state_id),
         pc01_district_id  = as.numeric(pc01_district_id),
         pc01_subdistrict_id  = as.numeric(pc01_subdistrict_id))



census_shrid <- census_shrid %>% 
  left_join(shrug_pc01subdistrict_keys_unique, by = c('STATE' = 'pc01_state_id', 'DISTRICT' = 'pc01_district_id', 
                                     'SUB_DIST' = 'pc01_subdistrict_id'))

rm(list = c('shrug_pc01r_keys', 'shrug_pc01subdistrict_keys', 'shrug_pc01subdistrict_keys_unique'))
gc()



#census_shrid$geometry <- NULL

census_shrid %>% write_csv('data/matching_village_codes/census_2001_codes_with_shrid.csv')



#########################################
# census mathcing with IMIS data

state_level <- census_shrid %>% 
  distinct(pc01_state_name)



habs_2009 <- read_csv('data/BASIC_HABITATION_INFORMATION_AS_ON_1_APR_09.csv')%>% 
  clean_names()
qual_habs_2009 <- read_csv('data/QUALITY_AFFECTED_HABITATIONS_AS_ON_1_APR_09.csv')%>% 
  clean_names()

habs_2009 <- habs_2009 %>% 
  distinct(state_name, district_name, block_name, panchayat_name, village_name, habitation_name, .keep_all = TRUE)
qual_habs_2009 <- qual_habs_2009 %>% 
  distinct(state_name, district_name, block_name, panchayat_name, village_name, habitation_name, .keep_all = TRUE)

habs_2009 <- habs_2009 %>% 
  left_join(qual_habs_2009, by = c("state_name", "district_name", "block_name", "panchayat_name", "village_name", "habitation_name"))


habs_2009 <- habs_2009 %>% 
  mutate(quality_parameter = ifelse(is.na(quality_parameter), "None", quality_parameter))

habs_2009 <- habs_2009 %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name),
         district_name_clean = str_remove(district_name, '\\(\\d+\\)$'),
         block_name = str_to_lower(block_name),
         block_name_clean = str_remove(block_name, '\\(\\d+\\)$'),
         panchayat_name = str_to_lower(panchayat_name),
         panchayat_name_clean = str_remove(panchayat_name, '\\(\\d+\\)$'),
         village_name = str_to_lower(village_name),
         village_name_clean = str_remove(village_name, '\\(\\d+\\s?\\)$'), 
         habitation_name = str_to_lower(habitation_name),
         habitation_name_clean = str_remove(habitation_name, '\\(\\d+\\s?\\)$'))



############################
# Read DB with the water quality tests results
con <- dbConnect(drv=RSQLite::SQLite(), dbname="data/water_quality_tests/water_quality_tests.db")
dbListTables(con)
test_results <- dbGetQuery(con, 'SELECT * FROM test_results')
dbDisconnect(con)

test_results %>% 
  count(TypeOfSource, sort = T)

test_results <- test_results %>% 
  mutate(all_mand = str_c(AbovePMandatory, ',', BelowPMandatory),
         all_emerging = str_c(AbovePEmerging, ',', BelowPEmerging),
         #iron = str_extract(all_mand, 'Iron\\[(\\d\\.\\d\\d\\d) mg/l\\]', "\\1"),
         #iron = str_replace(all_mand, 'Iron\\[(\\d\\.\\d\\d\\d) mg/l\\]',  "\\1"),
         iron = as.numeric(gsub('.*Iron\\[(\\d\\.\\d\\d\\d) mg/l\\].*', "\\1", all_mand)),
         arsenic = as.numeric(gsub('.*Arsenic\\[(\\d\\.\\d\\d\\d) mg/l\\].*', "\\1", all_mand)),
         fluoride = as.numeric(gsub('.*Fluoride\\[(\\d\\.\\d\\d\\d) mg/l\\].*', "\\1", all_mand)),
         nitrate = as.numeric(gsub('.*Nitrate\\[(\\d\\.\\d\\d\\d) mg/l\\].*', "\\1", all_mand)),
         manganese = as.numeric(gsub('.*Manganese\\[(\\d\\.\\d\\d\\d) mg/l\\].*', "\\1", all_emerging)),
         
  ) %>% 
  filter(TypeOfSource == "Deep Tubewell")


village_test_results <- test_results %>% 
  dplyr::select(StateName, DistrictName, BlockName, PanchayatName, VillageName, iron, arsenic, fluoride, nitrate, manganese) %>% 
  group_by(StateName, DistrictName, BlockName, PanchayatName, VillageName) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup()

habs_test_results <- test_results %>% 
  dplyr::select(StateName, DistrictName, BlockName, PanchayatName, VillageName, HabitationName, iron, arsenic, fluoride, nitrate, manganese) %>% 
  group_by(StateName, DistrictName, BlockName, PanchayatName, VillageName, HabitationName) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup()

habs_test_results <- habs_test_results %>% 
  mutate_at(vars(StateName, DistrictName, BlockName, PanchayatName, VillageName, HabitationName), str_to_lower)



village_test_results <- village_test_results %>% 
  mutate_at(vars(StateName, DistrictName, BlockName, PanchayatName, VillageName), str_to_lower) %>% 
  mutate(district_name_clean = case_when((StateName == 'assam')& (DistrictName == 'morigaon') ~ 'marigaon',
                                         #(StateName == 'assam')& (DistrictName == 'morigaon') ~ 'marigaon',
                                         (StateName == 'west bengal')& (DistrictName == 'darjeeling') ~ 'darjiling',
                                         (StateName == 'west bengal')& (DistrictName == 'coochbehar') ~ 'koch bihar',
                                         (StateName == 'west bengal')& (DistrictName == 'purba bardhaman') ~ 'barddhaman',
                                         (StateName == 'west bengal')& (DistrictName == 'north 24 paraganas') ~ 'north 24 parganas',
                                         (StateName == 'west bengal')& (DistrictName == 'hooghly') ~ 'hugli',
                                         (StateName == 'west bengal')& (DistrictName == 'purulia') ~ 'puruliya',
                                         (StateName == 'west bengal')& (DistrictName == 'howrah') ~ 'haora',
                                         (StateName == 'west bengal')& (DistrictName == 'south 24 parganas') ~ 'south 24 parganas',
                                         TRUE ~ DistrictName
                                         ),
         district_name_clean = str_remove(district_name_clean, '\\(\\d+\\)$'),
         block_name_clean = str_remove(BlockName, '\\(\\d+\\)$'),
         panchayat_name_clean = str_remove(PanchayatName, '\\(\\d+\\)$'),
         village_name_clean = str_remove(VillageName, '\\(\\d+\\s?\\)$'), 
         StateName = ifelse(StateName == 'odisha', 'orissa', StateName)
         
         )



######################################

village_test_results  <- village_test_results %>% 
  distinct(StateName, district_name_clean, PanchayatName, VillageName, .keep_all = T)



habs_2009 <- habs_2009 %>% 
 # distinct(state_name, district_name_clean, block_name_clean, panchayat_name_clean, village_name_clean) %>% 
  left_join(village_test_results, by = c('state_name' = 'StateName', 'district_name_clean',
                                         #'block_name_clean' = 'BlockName', 
                                         'panchayat_name_clean' = 'PanchayatName',
                                         'village_name_clean' = 'VillageName'))


# not necessary code
#####################################


habs_2009 %>% 
  distinct(state_name, district_name_clean, block_name_clean, panchayat_name_clean, village_name_clean) %>% 
  semi_join(village_test_results, by = c('state_name' = 'StateName', 'district_name_clean',
                                      #'block_name_clean' = 'BlockName', 
                                      'panchayat_name_clean',
                                      'village_name_clean' )) %>% 
  View()




habs_2009 %>% 
  distinct(state_name, district_name_clean) %>% 
  filter(state_name %in% c('assam', 'west bengal')) %>% 
  semi_join(village_test_results %>% distinct(StateName, district_name_clean), by = c('state_name' = 'StateName', 'district_name_clean')) 
  
habs_2009 %>% 
  distinct(state_name, district_name_clean, block_name_clean) %>% 
  filter(state_name %in% c('assam', 'west bengal')) %>% 
  anti_join(village_test_results %>% distinct(StateName, district_name_clean, BlockName), by = c('state_name' = 'StateName', 'district_name_clean', 'block_name_clean' = 'BlockName')) 


habs_2009 %>% 
  semi_join(habs_test_results, by = c('state_name' = 'StateName', 'district_name_clean' = 'DistrictName', 
                                         'block_name_clean' = 'BlockName', 'panchayat_name_clean' = 'PanchayatName',
                                         'village_name_clean' = 'VillageName', 'habitation_name_clean' = 'HabitationName'))

#####################################################


  


# just testing
###############################
habs_2009_state_level <- habs_2009 %>% 
  distinct(state_name) %>% 
  mutate(state_name = str_to_lower(state_name))


state_level %>% 
  inner_join(habs_2009_state_level, by = c('pc01_state_name' = 'state_name'))


state_level %>% 
  anti_join(habs_2009_state_level, by = c('pc01_state_name' = 'state_name'))



district_level <- census_shrid %>% 
  distinct(pc01_state_name, pc01_district_name) %>% 
  mutate(pc01_district_name = str_to_lower(pc01_district_name))

habs_2009_district_level <- habs_2009 %>% 
  distinct(state_name, district_name) %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name),
         district_name_clean = str_remove(district_name, '\\(\\d+\\)$'))

district_level %>% 
  inner_join(habs_2009_district_level, by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean'))



subdistrict_level <- census_shrid %>% 
  distinct(pc01_state_name, pc01_district_name, pc01_subdistrict_name) %>% 
  mutate(pc01_district_name = str_to_lower(pc01_district_name),
         pc01_subdistrict_name = str_to_lower(pc01_subdistrict_name))



habs_2009_subdistrict_level <- habs_2009 %>% 
  distinct(state_name, district_name, block_name) %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name),
         district_name_clean = str_remove(district_name, '\\(\\d+\\)$'),
         block_name = str_to_lower(block_name),
         block_name_clean = str_remove(block_name, '\\(\\d+\\)$'),
  )

subdistrict_level %>% 
  inner_join(habs_2009_subdistrict_level, 
             by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean', 
                    'pc01_subdistrict_name' = 'block_name_clean'))

############################################



census_shrid <- census_shrid %>% 
  mutate(pc01_district_name = str_to_lower(pc01_district_name),
         pc01_subdistrict_name = str_to_lower(pc01_subdistrict_name),
         village_name_1 = str_to_lower(NAME),
         village_name_2 = str_to_lower(NAME_1))


habs_2009_village_level <- habs_2009 %>% 
  distinct(state_name, district_name, block_name, village_name, .keep_all = TRUE) %>% 
  mutate(state_name = str_to_lower(state_name),
         district_name = str_to_lower(district_name),
         district_name_clean = str_remove(district_name, '\\(\\d+\\)$'),
         block_name = str_to_lower(block_name),
         block_name_clean = str_remove(block_name, '\\(\\d+\\)$'),
         village_name = str_to_lower(village_name),
         village_name_clean = str_remove(village_name, '\\(\\d+\\s?\\)$'),  ) %>% 
  distinct(state_name, district_name_clean, block_name_clean, village_name_clean, .keep_all = TRUE)  
  



census_shrid %>% 
  semi_join(habs_2009_village_level, 
             by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean', 
                    'pc01_subdistrict_name' = 'block_name_clean', 'village_name_1' = 'village_name_clean'))

census_shrid %>% 
  semi_join(habs_2009_village_level, 
             by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean', 
                    'pc01_subdistrict_name' = 'block_name_clean', 'village_name_1' = 'village_name_clean'))




census_shrid %>% 
  semi_join(habs_2009_village_level, 
             by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean', 
                    'village_name_1' = 'village_name_clean'))




##############################################


census_shrid <- census_shrid %>% 
  left_join(habs_2009_village_level, 
             by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean', 
                    'village_name_1' = 'village_name_clean'))


census_shrid <- census_shrid %>% 
  left_join(habs_2009_village_level, 
            by = c('pc01_state_name' = 'state_name', 'pc01_district_name' ='district_name_clean', 
                   'pc01_subdistrict_name' = 'block_name_clean', 'village_name_1' = 'village_name_clean'))



matched_villages <- census_shrid %>% 
  filter(!is.na(quality_parameter))


matched_villages <- census_shrid %>% 
  filter(!is.na(DistrictName))




table(matched_villages$quality_parameter)

matched_villages <- matched_villages %>% 
  mutate(male_share = TOT_M/TOT_P,
         children_share = P_06/TOT_P,
         sc_share = P_SC/TOT_P,
         st_share = ST_P/TOT_P,
         irr_share = TOT_IRR/(TOT_IRR + UN_IRR),
         lit_share = P_LIT/(P_LIT + P_ILL),
         fluoride_class = ifelse(quality_parameter == "Fluoride", 1, 0),
         iron_class = ifelse(quality_parameter == "Iron", 1, 0),
         nitrate_class = ifelse(quality_parameter == "Nitrate", 1, 0),
         arsenic_class = ifelse(quality_parameter == "Arsenic", 1, 0),
         salinity_class = ifelse(quality_parameter == "Salinity", 1, 0),
         )


matched_villages$geometry <- NULL





# correlation tables
#######################################
(fe_model <- feols(I(log(TOT_P)) ~ iron, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ iron|SID, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ iron |SID^district_name, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ iron|SID^district_name^block_name, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ iron|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))


(fe_model <- feols(I(log(TOT_P)) ~ arsenic, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ arsenic|SID, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ arsenic |SID^district_name, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ arsenic|SID^district_name^block_name, data = matched_villages))
(fe_model <- feols(I(log(TOT_P)) ~ arsenic|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))





tidy_custom.fixest <- function(x) {
  out <- tidy(x, se = 'hetero')

  return(out)
}


glance_custom.fixest <- function(x) {
  dv <- as.character(formula(x)[2])
  y_true <- x$fitted.values + x$residuals
  mean_dv <- mean(y_true, na.rm = T)
  sd_dv <- sd(y_true)
  fe_size <- sum(x$fixef_sizes)
  out <- tibble::tibble("mean_dv" = mean_dv, "sd_dv" = sd_dv,  "nobs" = stats::nobs(x), 'fe_size' = fe_size)
  return(out)
}


(iron_no_fe <- feols(iron ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT , data = matched_villages, se = "hetero"))
(iron_state_fe <- feols(iron ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID , data = matched_villages, se = "hetero"))
(iron_dist_fe <- feols(iron ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name, data = matched_villages, se = "hetero"))
(iron_block_fe <- feols(iron ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name, data = matched_villages, se = "hetero"))
(iron_panch_fe <- feols(iron ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))


f <- function(x) format(round(x, 3), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "Number of obs.", "fmt" =  f),
  list("raw" = "mean_dv", "clean" = "DV mean:", 'fmt' =  f),
  list("raw" = "sd_dv", "clean" = "DV SD:", "fmt" = f),
  list("raw" = "fe_size", "clean" = "Number of FE", "fmt" = f))

additional_rows <- data.frame('1'=c('FE level'), '2'=c('No FE'), '3'=c('State'), '4'=c('District'), '5'=c('Block'), '6' = c('Panchayat'))

cm <- c('I(log(TOT_P))'    = 'Log of population',
        'lit_share'   = 'Literate share',
        'irr_share' = 'Irrig. land share',
        'PHS_CNT' = 'Pr. health centres')



modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)' = iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Iron in water (mg/l)', coef_map = cm)

modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)' = iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Iron in water (mg/l)', coef_map = cm, output = 'outputs/tables/corrs_iron.tex')




(iron_no_fe <- feols(arsenic ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT , data = matched_villages, se = "hetero"))
(iron_state_fe <- feols(arsenic ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID , data = matched_villages, se = "hetero"))
(iron_dist_fe <- feols(arsenic ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name, data = matched_villages, se = "hetero"))
(iron_block_fe <- feols(arsenic ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name, data = matched_villages, se = "hetero"))
(iron_panch_fe <- feols(arsenic ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))


modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)' = iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Arsenic in water (mg/l)', coef_map = cm)
modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)' = iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Arsenic in water (mg/l)', coef_map = cm,
             output = 'outputs/tables/corrs_arsenic.tex')




(iron_no_fe <- feols(fluoride ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT , data = matched_villages, se = "hetero"))
(iron_state_fe <- feols(fluoride ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID , data = matched_villages, se = "hetero"))
(iron_dist_fe <- feols(fluoride ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name, data = matched_villages, se = "hetero"))
(iron_block_fe <- feols(fluoride ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name, data = matched_villages, se = "hetero"))
(iron_panch_fe <- feols(fluoride ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))


modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)' = iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Fluoride in water (mg/l)', coef_map = cm)

modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)' = iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Fluoride in water (mg/l)', coef_map = cm,
             output = 'outputs/tables/corrs_fluoride.tex')



(iron_no_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT , data = matched_villages, se = "hetero"))
(iron_state_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID , data = matched_villages, se = "hetero"))
(iron_dist_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name, data = matched_villages, se = "hetero"))
(iron_block_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name, data = matched_villages, se = "hetero"))
(iron_panch_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))


modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)'=iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Nitrate in water (mg/l)', coef_map = cm)
modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe, '(5)'=iron_panch_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Nitrate in water (mg/l)', coef_map = cm,
             output = 'outputs/tables/corrs_nitrate.tex')


(iron_no_fe <- feols(manganese ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT , data = matched_villages, se = "hetero"))
(iron_state_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID , data = matched_villages, se = "hetero"))
(iron_dist_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name, data = matched_villages, se = "hetero"))
(iron_block_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name, data = matched_villages, se = "hetero"))
(iron_panch_fe <- feols(nitrate ~ I(log(TOT_P)) + lit_share + irr_share + PHS_CNT|SID^district_name^block_name^panchayat_name, data = matched_villages, se = "hetero"))

modelsummary(list('(1)' = iron_no_fe, '(2)' = iron_state_fe, '(3)' = iron_dist_fe, '(4)' = iron_block_fe), stars = c('*'=0.1, '**' = 0.05, '***' = 0.01),
             coef_omit = "(Intercept)", gof_map = gm, add_rows = additional_rows, title = 'Fluoride in water (mg/l)')


##############################################################

# Making the WB map
########################################

census_shrid %>% 
  unique(pc01_state_name)

wb_map <- census_shrid %>% 
  filter(pc01_state_name == "west bengal")

district_map <- wb_map %>% 
  filter(pc01_district_name %in% c("birbhum", "murshidabad", "barddhaman", "nadia"))
# maldah is good
# there is a lot of arsenic in murshidabad

table(wb_map$quality_parameter, useNA = "always")

table(district_map$quality_parameter, useNA = "always")


plot(district_map$geometry)

mapview(district_map, zcol = "quality_parameter")


ggplot(data = district_map) +
    annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = quality_parameter)) +
  ggtitle(label = "Contamination of drinking water")+
  annotation_scale() +
  labs(fill = "Contamination")


ggsave('outputs/figures/wb_districts_water_contamination_map.pdf', scale = 1.5)



ggplot(data = district_map) +
  annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = arsenic)) +
  ggtitle(label = "Arsenic contamination of drinking water")+
  annotation_scale() +
  labs(fill = "Arsenic in water (mg/L)")+
  scale_fill_gradient2(low = "black", mid = "yellow",high = "red", midpoint = .05, limits = c(0,0.15), oob = scales::squish )
  #scale_fill_viridis_c(option = "magma", #values = c(0, 0.05, 0.4, 0.7)
  #                     trans = 'log10')
  #scale_fill_gradient2(low = "black", mid = "yellow", high = "white", midpoint = .2)

ggsave('outputs/figures/wb_districts_arsenic_water_contamination_map.pdf', scale = 1.5)




ggplot(data = district_map %>% mutate(arsenic_above_norm = ifelse(arsenic > 0.01, "Above norm (>0.01 mg/L)", 'Below norm (>0.01 mg/L)'))) +
  annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = arsenic_above_norm)) +
  ggtitle(label = "Arsenic contamination of drinking water")+
  annotation_scale() +
  labs(fill = "Arsenic in water")#+  scale_fill_gradient2(low = "black", mid = "yellow", high = "white", midpoint = .2)
  #scale_fill_viridis_c(option = "magma")

ggsave('outputs/figures/wb_districts_arsenic_aboc_norm_map.pdf', scale = 1.5)


ggplot(data = district_map) +
  annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = fluoride)) +
  ggtitle(label = "Fluoride contamination of drinking water")+
  annotation_scale() +
  labs(fill = "Fluoride in water (mg/L)")+
  scale_fill_gradient2(low = "black", mid = "yellow",high = "red", midpoint = 1.5#, limits = c(0,0.15), oob = scales::squish 
                       )
  #♦scale_fill_viridis_c(option = "magma")


ggsave('outputs/figures/wb_districts_fluoride_water_contamination_map.pdf', scale = 1.5)




######################################


# to read only some columns
# https://stackoverflow.com/questions/68627927/reading-large-shapefile-in-r-using-using-sf-package


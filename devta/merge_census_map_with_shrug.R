
######################################################
# Merging with SRUG 
###############################
shrug_pc01r_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01r_key.csv") 

shrug_pc01r_keys <- shrug_pc01r_keys %>%
  mutate(pc01_state_id  = as.numeric(pc01_state_id),
         pc01_district_id  = as.numeric(pc01_district_id),
         pc01_subdistrict_id  = as.numeric(pc01_subdistrict_id),
         pc01_village_id  = as.numeric(pc01_village_id),)


village_map <- village_map %>% 
  left_join(shrug_pc01r_keys, by = c('STATE' = 'pc01_state_id', 'DISTRICT' = 'pc01_district_id', 
                                     'SUB_DIST' = 'pc01_subdistrict_id', 'TOWN_VILL' = 'pc01_village_id'))





shrug_pc01subdistrict_keys <- read_csv("C:/Users/marti/OneDrive/Plocha/research_projects/india-land-reform/data/shrug/secc/shrug-v1.5.samosa-keys-csv/shrug_pc01_subdistrict_key.csv") 
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

library(tidyverse)
library(here)
library(janitor)
library(rdrobust)
library(measurements)
library(sf)
library(mapview)
setwd(here::here())


library(readxl)
GWQ_2010_2018 <- read_excel("data/ground_water_quality/GWQ_2010-2018.xlsx")


GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  clean_names()

str(GWQ_2010_2018)


dhs_locs <- st_read("data\\DHS\\2015-2016\\gps data\\IAGE71FL\\IAGE71FL.shp")


vars_to_mut <- names(GWQ_2010_2018)[14:37]

GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate_at(vars(vars_to_mut), ~ as.numeric(ifelse(.x == "BDL", 0, .x)))



GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate(degrees = ifelse(is.na(as.numeric(latitude)) | is.na(as.numeric(longitude)), 1, 0 ))


GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate(lat = ifelse(degrees == 1, str_replace_all(latitude, '°|\'|"', ' ') %>% 
                        str_replace("N", "") %>% 
                        str_trim(), latitude ),
         lon = ifelse(degrees == 1, str_replace_all(longitude, '°|\'|"', ' ') %>% 
                        str_replace("E", "") %>% 
                        str_trim(), longitude ))

GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate(lat_num = ifelse(degrees == 1, conv_unit(lat, "deg_min_sec", "dec_deg"), lat),
         lon_num = ifelse(degrees == 1, conv_unit(lon, "deg_min_sec", "dec_deg"), lon),
         lat_fin = as.numeric(lat_num),
         lon_fin = as.numeric(lon_num))


GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate(lat_num =conv_unit(lat, "deg_min_sec", "dec_deg"),
         lon_num = conv_unit(lon, "deg_min_sec", "dec_deg"),
         lat_fin = as.numeric(lat_num),
         lon_fin = as.numeric(lon_num))


GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate(lat_num = ifelse(degrees == 1,convert(lat), lat),
         lon_num = ifelse(degrees == 1,convert(lon), lat),
         lat_fin = as.numeric(lat_num),
         lon_fin = as.numeric(lon_num))



convert<-function(x){
  z <- sapply((strsplit(x, " ")), as.numeric)
  z[1, ] + z[2, ]/60 + z[3, ]/3600
} 




gw_fe <- GWQ_2010_2018 %>% 
  filter(!is.na(fe), !is.na(lat_fin), !is.na(lon_fin)) %>% 
  filter(lat_fin > 0, lat_fin <180, lon_fin > 0, lon_fin <180)

gw_fe_sf <- st_as_sf(gw_fe, coords = c("lat_fin", "lon_fin"), crs = "WGS84")

gw_fe_sf <- st_as_sf(gw_fe, coords = c("lon_fin", "lat_fin"), crs = "WGS 84")


gw_fe_sf %>% 
  mapview()


"33 44 28	"
conv_unit("33 44 28	",  "deg_min_sec", "dec_deg")


dhs_locs %>% 
  mapview()


to_rep <- "14°21'12\" N"


str_replace_all(to_rep, '°|\'|"', ' ') %>% 
  str_replace("N", "") %>% 
  str_trim()

GWQ_2010_2018 <- GWQ_2010_2018 %>% 
  mutate_at(vars(latitude, longitude), as.numeric)


conv_unit("33 1 1", "deg_min_sec", "dec_deg")

conv_unit("29°38'50 l", "deg_min_sec", "dec_deg")



GWQ_2010_2018 %>% 
  mutate_all(~ ifelse(.x == "ND", 0, .x))


names(GWQ_2010_2018)[15]




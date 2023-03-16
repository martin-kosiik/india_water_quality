library(tidyverse)
library(here)
library(janitor)
library(rdrobust)
library(sf)
library(mapview)
library(ggspatial)

setwd(here::here())


wb_lithology <- sf::read_sf("data\\GSI\\west_bengal\\Lithology.shp")

plot(wb_lithology[1:2000, ]$geometry)

plot(wb_lithology$geometry)

wb_lithology



ggplot(data = wb_lithology) +
 # annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = AGE)) +
  ggtitle(label = "Age of formation")+
  annotation_scale() #+labs(fill = "Contamination")


table(wb_lithology$LITHOLOGIC)

wb_lithology %>% 
  filter(AGE == "HOLOCENE", GROUP_NAME == "NEWER ALLUVIUM", LITHOLOGIC == "SAND, SILT AND CLAY")


wb_lithology <- wb_lithology %>% 
  mutate(arsenic_pred = ifelse((AGE == "HOLOCENE") & (GROUP_NAME == "NEWER ALLUVIUM") & (LITHOLOGIC == "SAND, SILT AND CLAY"),
                               1, 0),
         arsenic_pred = as.factor(arsenic_pred))



ggplot(data = wb_lithology) +
  # annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = arsenic_pred)) +
  ggtitle(label = "Predicted arsenic")+
  annotation_scale()

ggsave('outputs/figures/pred_arsenic_all_wb.pdf', scale = 1.5)


ggplot(data = wb_lithology) +
  # annotation_map_tile("osm", zoom = 8) +
  geom_sf(aes(fill = arsenic_pred)) +
  ggtitle(label = "Predicted arsenic")+
  annotation_scale() +
  scale_x_continuous( limits = c( 86.6 , 88.8 ) , expand = c( 0 , 0 ) )+
  scale_y_continuous( limits = c( 22.9 , 25 ) , expand = c( 0 , 0 ) )

ggsave('outputs/figures/pred_arsenic_selected_dist_in_wb.pdf', scale = 1.5)



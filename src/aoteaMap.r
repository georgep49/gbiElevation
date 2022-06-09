## R code for drawing base maps of Aotea

library(tidyverse)
library(patchwork)
library(ggrepel)
library(dendextend)
library(sf)
library(ggspatial)


coast <- read_sf("./data/coast_poly_NZTM")
coast.akld <- st_crop(coast, xmin = 1741620, ymin = 5859550, xmax = 1834360,	ymax = 6012920)
coast.gbi <- st_crop(coast.akld, xmin = 1803169, ymin = 5971850, xmax = 6012920,	ymax = 6012920)

places <- read_csv("./data/map_places.csv", col_types = "cddddfn")
places.rg <- filter(places, gbi.loc == 0)
places.gbi <- filter(places, gbi.loc == 1)


rg.map <- ggplot(data = coast.akld) +
  geom_sf(col = "blue", fill = "#eeeeee") +
  coord_sf(expand = FALSE) +
  geom_point(data = places.rg, aes(x = long.tm, y = lat.tm, shape = sym), size = 4) +
  geom_text_repel(data = places.rg, aes(x = long.tm, y = lat.tm, label = name), size = 4.5, alpha = 0.7) +
  xlab("Latitude") +
  ylab("Longitude") + 
  annotation_scale(location = "bl", width_hint = 0.4) +
  guides(shape = "none") +
  theme_bw()

gbi.map <- ggplot(data = coast.gbi) +
  geom_sf(col = "blue", fill = "#eeeeee") +
  coord_sf(expand = FALSE) +
  geom_point(data = places.gbi, aes(x = long.tm, y = lat.tm, shape = sym), size = 4) +
  geom_text_repel(data = places.gbi, aes(x = long.tm, y = lat.tm, label = name), size = 4.5, alpha = 0.7) +
  xlab("Latitude") +
  ylab("Longitude") + 
  annotation_scale(location = "bl", width_hint = 0.4) +
  guides(shape = "none") +
  theme_bw()

maps.sideby <- rg.map + gbi.map
maps.sideby

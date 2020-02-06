library(osmdata)
library(dodgr)
library(tidyverse)
library(sf)
library(ggmap)
library(tidycensus)
options(tigris_use_cache = TRUE)

bbox <- st_bbox(c(xmin = -74.05, 
                  xmax = -73.81, 
                  ymax = 40.74,
                  ymin = 40.55), 
                crs = st_crs(4326)) %>% 
  st_as_sfc()

bbnj <- st_bbox(c(xmin = -74.05, 
                  xmax = -74.02, 
                  ymax = 40.74,
                  ymin = 40.69), 
                crs = st_crs(4326)) %>% 
  st_as_sfc()

url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_19d.zip"
fil <- basename(url)
if (!file.exists(fil)) download.file(url, fil)

nyc <- unzip(fil)

nyc <- st_read(nyc[1]) %>% 
  st_transform(4326) %>% 
  st_intersection(bbox)

bklyn <- nyc[2, ]
queens <- nyc[3, ]
mnhtn <- nyc[4, ]

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

estimate_box <- osmdata::getbb("Brooklyn, New York")
streets_raw <- dodgr_streetnet(estimate_box, expand = 1)


bike_lanes <- 
  streets_raw %>% 
  as_tibble() %>% 
  select(osm_id, name, contains("bicycle"), contains("cycleway")) %>% 
  pivot_longer(cols = c(-osm_id, -name), names_to = "variables") %>% 
  mutate(value = ifelse(is.na(value), "no", "yes")) %>% 
  group_by(osm_id, name) %>% 
  mutate(bike_lane = ifelse(any(value == "yes"), "yes", "no")) %>% 
  select(-variables, -value) %>% 
  distinct() %>% 
  ungroup()

bike_lanes <- 
  left_join(bike_lanes, streets_raw) %>% 
  st_as_sf() %>% 
  st_intersection(bbox) %>% 
  st_erase(bbnj) %>%
  select(osm_id, name, bike_lane)

ggplot() +
  geom_sf(data = bklyn, fill = "#666699", alpha = 0.7, size = 0.5) + 
  geom_sf(data = queens, fill = "grey35", alpha = 0.5, size = 0.25) +
  geom_sf(data = mnhtn, fill = "grey35", alpha = 0.5, size = 0.25) + 
  geom_sf(data = filter(bike_lanes, bike_lane == "yes"), alpha = 0.4, color = "#8cff1a", show.legend = FALSE) +
  labs(title = "The Bike Lanes of Brooklyn") +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, color = "grey25", face = "bold", family = "Minion Pro", vjust = -1),
    plot.background = element_rect(fill = "#f0f0f5", color = NA),
    panel.background = element_rect(fill = "#f0f0f5",  color = NA),
    panel.grid = element_blank(), 
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"))

ggsave("bike_lanes_of_brooklyn.pdf", 
       dpi = 500, 
       width = 210, 
       height = 297, 
       units = "mm",
       device = cairo_pdf)

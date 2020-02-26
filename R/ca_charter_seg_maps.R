library(tidyverse)
library(educationdata)
library(patchwork)
library(cowplot)
library(glue)
library(sf)
library(tigris)
library(seg)
options(tigris_use_cache = TRUE)

theme_set(
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill =  "#F8F8FF",
                                     color = NA),
      panel.background = element_rect(fill = "#F8F8FF",
                                      color = NA),
      legend.background = element_rect(fill = "#F8F8FF",
                                       color = NA),
      plot.title = element_text(size = 12, colour = "grey25", face = "bold"),
      plot.subtitle = element_text(size = 11, colour = "grey45"),
      plot.caption = element_text(size = 7, colour = "grey45", face = "italic", hjust = 1)
    )
)

dir_dat <-
  get_education_data(level = "school-districts",
                     source = "ccd",
                     topic = "directory",
                     filters = list(year = c(2005, 2015),
                                    fips = 6)) %>% 
  as_tibble()

sch_dir <-
  get_education_data(level = "schools",
                     source = "ccd",
                     topic = "directory",
                     filters = list(year = c(2005, 2015),
                                    fips = 6)) %>% 
  as_tibble()

charter_enroll <- 
  sch_dir %>% 
  na_if(-1) %>% 
  na_if(-2) %>% 
  na_if(-3) %>% 
  na_if(-6) %>%
  na_if(-9) %>%
  group_by(leaid, charter, year) %>% 
  summarize(enrollment = sum(enrollment, na.rm = TRUE)) %>%
  ungroup() %>% 
  complete(charter, nesting(leaid, year), fill = list(enrollment = 0)) %>% 
  arrange(leaid, year, charter) %>% 
  group_by(leaid, year) %>% 
  mutate(prop = enrollment/sum(enrollment)) %>% 
  ungroup()

charter_enroll <- charter_enroll %>%
  mutate(charter = ifelse(charter == 1, "charter", "traditional")) %>% 
  pivot_wider(id_cols = c(leaid, year), names_from = charter, values_from = c(enrollment, prop)) %>% 
  mutate(leaid = as.character(as.numeric(leaid)))

enroll_dat <-
  get_education_data(level = "schools",
                     source = "ccd",
                     topic = "enrollment",
                     filters = list(year = c(2005, 2015),
                                    fips = 6,
                                    grade = 99),
                     by = list("race")) %>% 
  as_tibble() %>% 
  na_if(-1) %>% 
  na_if(-2) %>% 
  na_if(-3) %>% 
  na_if(-6) %>%
  na_if(-9) %>%
  mutate(
    race = case_when(
      race == 1 ~ "white",
      race %in% c(2:20) ~ "non_white",
      race == 99 ~ "total"
    )
  )  %>% 
  pivot_wider(id_cols = c(ncessch, leaid, year), 
              names_from = race, 
              values_from = enrollment,
              values_fn = list(enrollment = ~(sum(., na.rm = TRUE)))) %>% 
  arrange(ncessch, year) %>% 
  select(white, non_white, ncessch, leaid, year)

# Capturing district-level segregation
# Keep only districts with at least 2 schools!
tmp <- 
  enroll_dat %>% 
  group_split(year) %>% 
  map(~ .x %>%
        group_by(leaid) %>% 
        mutate(n = n()) %>% 
        filter(n > 1) %>% 
        ungroup() %>% 
        select(-n))

# Remove districts with NO white students! They exist!
leaids_to_remove <- 
  map(tmp, ~ .x %>% group_by(leaid) %>% summarize_if(is.numeric, sum) %>% filter(white == 0) %>% pull(leaid)) %>% unlist()

tmp <-
  tmp %>% 
  map(~ .x %>% filter(!leaid %in% leaids_to_remove))

seg_district <-
  map(tmp,  
      ~ map_dfr(
        unique(.x$leaid),
        function(y,z) 
          tibble(leaid = y,
                 seg = seg::dissim(data = .x %>% filter(leaid == y)) %>% pluck("d"))))

seg_district <- map2(seg_district, c(2005, 2015), ~ .x %>% mutate(year = .y)) %>% bind_rows()

charter_enroll <- charter_enroll %>% left_join(seg_district)

# district boundary info

shps <- dir("./shapefiles", pattern = ".shp$")

shps <- shps[-2]

ca_shps <- 
  map2(shps, c(2005, 2015), ~ read_sf(paste("shapefiles", .x, sep = "/")) %>% 
       filter(STATEFP == "06") %>% 
       mutate(year = .y,
             GEOID = as.character(as.numeric(GEOID))) %>% 
       select(GEOID, year, geometry))

ca_ds_2015 <- 
  map(c("unified", "elementary"),
      ~ school_districts("ca", year = 2015, type = .x, class = "sf")) %>% 
  rbind_tigris() %>% 
  mutate(year = 2015,
         GEOID = as.character(as.numeric(GEOID)))  %>% 
  select(GEOID, year, geometry)


ca_ds <- list()

ca_ds[[1]] <- ca_shps[[1]] %>% st_as_sf() %>% st_set_crs(4269)

ca_ds[[2]] <- rbind(ca_ds_2015, ca_shps[[2]]) %>% distinct(GEOID, .keep_all = TRUE) %>% st_set_crs(4269)

#charter_enroll <- charter_enroll %>% inner_join(ca_ds)

# Creating the maps ----
bivariate_color_scale <- tibble(
  group = c(
    "3 - 3", # high charter enrollment, high segregation
    "2 - 3", # medium charter enrollment, high segregation
    "1 - 3", # low charter enrollment, high segregation
    "3 - 2", # high charter enrollment, med segregation
    "2 - 2", # medium charter enrollment, med segregation
    "1 - 2", # low charter enrollment, med segregation
    "3 - 1", # high charter enrollment, low segregation
    "2 - 1", # medium charter enrollment, low segregation
    "1 - 1"  # low charter enrollment, low segregation
  ),
  color = rev(pals::stevens.pinkblue())
)

ca_chart_seg <- charter_enroll %>%  
  filter(!is.na(seg)) %>% 
  group_split(year) %>% 
  map2(
    ca_ds, 
    ~ .x %>% 
      mutate(
        charter_quantiles = ntile(prop_charter, 3),
        seg_quantiles = ntile(seg, 3)
      ) %>% 
      select(leaid, year,  charter_quantiles:last_col()) %>% 
      mutate(group = paste(as.numeric(charter_quantiles), "-", as.numeric(seg_quantiles))) %>% 
      left_join(bivariate_color_scale) %>% 
      left_join(.y %>% 
                  select(GEOID, geometry),
                by = c("leaid" = "GEOID"))
  )

ca_chart_seg <- rbind(ca_chart_seg[[1]], ca_chart_seg[[2]])


ca_map <- ca_chart_seg %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = color), size = 0.1, color = "white") +
  scale_fill_identity() +
  labs(title = glue("Segregation and Charter School Enrollment in California"),
       caption = "Source: Common Core of Data (https://nces.ed.gov/ccd/),\naccessed via the Urban Institute's API") +
  coord_sf() +
  facet_wrap(~ year)


leg <-
  bivariate_color_scale  %>% 
  separate(group, into = c("rev", "white"), sep = " - ") %>%
  mutate(rev = as.integer(rev),
         white = as.integer(white)) %>% 
  ggplot() +
  geom_tile(
    mapping = aes(
      x = rev,
      y = white,
      fill = color)
  ) +
  scale_fill_identity() +
  labs(x = "More segregated \u2192",
       y = "Higher charter enrollment \u2192") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),
    axis.title.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()

ggdraw(ca_map) +
  draw_plot(leg, 0.12, 0., 0.2, 0.2, scale = 0.75)

ggsave("ca_map.png", scale = 1.5)


library(gganimate)
library(transformr)

morph <- tween_sf(ca_chart_seg %>% filter(year == 2005), 
                  ca_chart_seg %>% filter(year == 2015), 
                  ease = 'linear',
                  nframes = 5)

ca_chart_seg %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = color), size = 0.1, color = "white") +
  scale_fill_identity(guide = "legend") +
  labs(title = "Segregation and Charter School Enrollment in California",
       subtitle = "Year: {current_frame}",
       caption = "Source: Common Core of Data (https://nces.ed.gov/ccd/),\naccessed via the Urban Institute's API") +
  coord_sf() +
  transition_manual(year)

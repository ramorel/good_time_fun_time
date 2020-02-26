library(tidyverse)
library(educationdata)
library(patchwork)
library(cowplot)
library(glue)
library(sf)
library(tigris)
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
                     filters = list(year = c(2011, 2015),
                                    fips = 17)) %>% 
  as_tibble()

fin_dat <-
  get_education_data(level = "school-districts",
                     source = "ccd",
                     topic = "finance",
                     filters = list(year = c(2011, 2015),
                                    fips = 17)) %>% 
  as_tibble()

enroll_dat <-
  get_education_data(level = "school-districts",
                     source = "ccd",
                     topic = "enrollment",
                     filters = list(year = c(2011, 2015),
                                    fips = 17,
                                    grade = 99),
                     by = list("race")) %>% 
  as_tibble() %>% 
  mutate(
    race = case_when(
      race == 1 ~ "white",
      race == 2 ~ "black",
      race == 3 ~ "hispanic",
      race == 4 ~ "asian",
      race == 5 ~ "other",
      race == 6 ~ "other",
      race == 7 ~ "other",
      race == 8 ~ "other", 
      race == 9 ~ "other",
      race == 20 ~ "other",
      race == 99 ~ "total"
    )
  ) %>% 
  pivot_wider(id_cols = c(leaid, year), 
              names_from = race, 
              values_from = enrollment,
              values_fn = list(enrollment = ~(sum(., na.rm = TRUE))))

il_ds_2015 <- 
  map(c("unified", "elementary"),
      ~ school_districts("il", year = 2015, type = .x, class = "sf")) %>% 
  rbind_tigris() %>% 
  mutate(year = 2015)

il_ds_2011 <- 
  map(c("unified", "elementary"),
      ~ school_districts("il", year = 2011, type = .x, class = "sf")) %>% 
  rbind_tigris()  %>% 
  mutate(year = 2011)

il_ds <- list(il_ds_2011, il_ds_2015)

fin_dat %>% names()

fin_dat <-
  fin_dat %>% 
  filter(!is.na(leaid)) %>% 
  select(year, leaid, enrollment_fall_responsible, contains("total")) %>% 
  select(-exp_current_elsec_total:-benefits_employee_total) %>% 
  na_if(-3) %>% 
  na_if(-2) %>% 
  na_if(-1) %>% 
  arrange(leaid, year)

fin_dat <- 
  fin_dat %>% 
  group_by(leaid) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  select(-n) %>% 
  mutate_at(vars(rev_total:exp_total),
            list(change = ~ . - lag(.))) %>% 
  mutate_at(vars(rev_total:exp_total),
            list(per_pupil = ~ ifelse(. > 0,
                                      ./enrollment_fall_responsible,
                                      0))) %>% 
  ungroup()

il_dat <- 
  inner_join(fin_dat, enroll_dat)

# Revenue by year
p1 <- fin_dat %>% 
  ggplot() +
  geom_histogram(aes(x = rev_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_total), color = "midnightblue") + 
  scale_x_continuous(labels = scales::label_number(scale = 0.001)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total revenue distribution in thousands of dollars", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p2 <- fin_dat %>% 
  mutate(rev_total = log(rev_total)) %>% 
  ggplot() +
  geom_histogram(aes(x = rev_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_total), color = "midnightblue") +
  labs(title = "Distribution of total logged revenue", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p3 <- fin_dat %>% 
  ggplot() +
  geom_histogram(aes(x = rev_local_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_local_total), color = "midnightblue") + 
  scale_x_continuous(labels = scales::label_number(scale = 0.001)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Local revenue distribution in thousands of dollars", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p4 <- fin_dat %>% 
  mutate(rev_local_total = log(rev_local_total)) %>% 
  ggplot() +
  geom_histogram(aes(x = rev_local_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_local_total), color = "midnightblue") +
  labs(title = "Distribution of total logged revenue", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p5 <- fin_dat %>% 
  ggplot() +
  geom_histogram(aes(x = rev_fed_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_fed_total), color = "midnightblue") + 
  scale_x_continuous(labels = scales::label_number(scale = 0.001)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Local revenue distribution in thousands of dollars", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p6 <- fin_dat %>% 
  mutate(rev_fed_total = log(rev_fed_total)) %>% 
  ggplot() +
  geom_histogram(aes(x = rev_fed_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_fed_total), color = "midnightblue") +
  labs(title = "Distribution of total logged revenue", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p7 <- fin_dat %>% 
  ggplot() +
  geom_histogram(aes(x = rev_state_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_state_total), color = "midnightblue") + 
  scale_x_continuous(labels = scales::label_number(scale = 0.001)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Local revenue distribution in thousands of dollars", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p8 <- fin_dat %>% 
  mutate(rev_state_total = log(rev_state_total)) %>% 
  ggplot() +
  geom_histogram(aes(x = rev_state_total, stat(density)), bins = 50, fill = "cornflowerblue", alpha = 0.7) + 
  geom_density(aes(x = rev_state_total), color = "midnightblue") +
  labs(title = "Distribution of total logged revenue", x = "", y = "") +
  facet_wrap(~ year, scales = "free")

p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
p5 + p6 + p7 + p8 + plot_layout(ncol = 2)

# Map of 2015 local revenue
il_dat %>%  
  filter(year == 2015) %>% 
  mutate(rev_local_total = log(rev_local_total)) %>% 
  left_join(il_ds %>% 
              select(GEOID, geometry) %>% 
              mutate(GEIOD = as.numeric(GEOID)),
            by = c("leaid" = "GEOID")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = rev_local_total)) + 
  scale_fill_viridis_c() + 
  coord_sf()

fin_dat %>%  
  filter(year == 2015) %>% 
  mutate(rev_total_per_pupil = log(rev_total_per_pupil)) %>% 
  left_join(il_ds %>% 
              select(GEOID, geometry) %>% 
              mutate(GEIOD = as.numeric(GEOID)),
            by = c("leaid" = "GEOID")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = rev_total_per_pupil), size = 0.5) + 
  scale_fill_viridis_c() + 
  coord_sf()
  
# Bivariate map

# create 3 buckets for local rev
quantiles_loc_rev <- il_dat %>%
  filter(year == 2015) %>% 
  pull(rev_local_total_per_pupil) %>%
  quantile(probs = seq(0, 1, length.out = 5), na.rm = TRUE)

# create 3 buckets for white
quantiles_white <- il_dat %>%
  filter(year == 2015) %>% 
  mutate(prop_white = white/total) %>%
  pull(prop_white) %>%
  quantile(probs = seq(0, 1, length.out = 5))

# V1 100  = #ff80ff
# V1 75   = #f2a6f2
# V1 55   = #ecc6ec
# V1 25   = 

bivariate_color_scale <- tibble(
  group = c(
    "4 - 4", # highest per pupil local revenue, mostly white
    "3 - 4", # high per pupil local revenue, mostly white
    "2 - 4", # low per pupil local revenue, mostly white
    "1 - 4", # lowest per pupil local revenue, mostly white
    "4 - 3", # highest per pupil local revenue, majority white
    "3 - 3", # high per pupil local revenue, majority white
    "2 - 3", # low per pupil local revenue, majority white
    "1 - 3", # lowest per pupil local revenue, majority white
    "4 - 2", # highest per pupil local revenue, majority non-white
    "3 - 2", # high per pupil local revenue, majority non-white
    "2 - 2", # low per pupil local revenue, majority non-white
    "1 - 2", # lowest per pupil local revenue, majority non-white
    "4 - 1", # highest per pupil local revenue, mostly non-white
    "3 - 1", # high per pupil local revenue, mostly non-white
    "2 - 1", # low per pupil local revenue, mostly non-white
    "1 - 1"  # lowest per pupil local revenue, mostly non-white
  ),
  color = rev(c("#f4f1f4", "#ffccfd", "#ff99ff", "#ff4dff", pals::arc.bluepink()[5:16]))
)


il_rev <- il_dat %>%  
  group_split(year) %>% 
  map2(
    il_ds, 
    ~ .x %>% 
      mutate(prop_white = white/total) %>% 
      mutate(
        rev_local_quantiles = cut(
          rev_local_total_per_pupil,
          breaks =  quantile(rev_local_total_per_pupil, probs = seq(0, 1, length.out = 5), na.rm = TRUE),
          labels = c(1, 2, 3, 4),
          include.lowest = TRUE
        ),
        rev_state_quantiles = cut(
          rev_state_total_per_pupil,
          breaks =  quantile(rev_state_total_per_pupil, probs = seq(0, 1, length.out = 5), na.rm = TRUE),
          labels = c(1, 2, 3, 4),
          include.lowest = TRUE
        ),
        white_quantiles = cut(
          prop_white,
          breaks = seq(0, 1, length.out = 5), 
          labels = c(1, 2, 3, 4),
          include.lowest = TRUE
        )
      ) %>% 
      select(leaid, year,  rev_local_quantiles:last_col()) %>% 
      pivot_longer(cols = c(-leaid, -year, -white_quantiles)) %>% 
      mutate(group = paste(as.numeric(value), "-", as.numeric(white_quantiles))) %>% 
      left_join(bivariate_color_scale) %>% 
      left_join(.y %>% 
                  select(GEOID, geometry),
                by = c("leaid" = "GEOID"))
  )

il_rev <- rbind(il_rev[[1]], il_rev[[2]]) %>% st_as_sf()

cities <-
  tibble(
    city = c("Chicago", "Aurora", "Naperville", "Joliet", "Rockford", "Springfield"),
    longitude = c(-87.6244, -88.3201,  -88.1535, -88.0817, -89.09399, -89.6501),
    latitude = c(41.8756, 41.7605, 41.7508, 41.5250, 42.2711, 39.7817)
  ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4269, agr = "constant")

  
il_map <- il_rev %>% 
  mutate(name = 
           case_when(
             name == "rev_local_quantiles" ~ "Local Revenue",
             name == "rev_state_quantiles" ~ "State Revenue"
           )) %>% 
  group_split(name) %>% 
  map(
    ~ .x %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = color), size = 0.1, color = "white") +
      geom_sf(data = cities, size = 0.9) +
      scale_fill_identity() +
      labs(subtitle = glue("Proportion white and per pupil {tolower(unique(.x$name))} by district")) +
      coord_sf() +
      facet_wrap(~ year)
  )


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
  labs(x = "Higher per pupil revenue \u2192",
       y = "More white students \u2192") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 5),
    axis.title.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()

il_map[[1]] <- il_map[[1]] + labs(title = glue("Race and Local + State Revenue in Illinois School Districts"))
il_map[[2]] <- il_map[[2]] + labs(caption = "Source: Common Core of Data (https://nces.ed.gov/ccd/),\naccessed via the Urban Institute's API")

il_maps <-
  plot_grid(
    il_map[[1]], 
    il_map[[2]], 
    ncol = 1
)

ggdraw() +
  draw_plot(il_maps, 0, 0, 1, 1) +
  draw_plot(leg, 0.32, 0.46, 0.2, 0.2, scale = 0.5)

ggsave("il_maps.png", scale = 1.5)


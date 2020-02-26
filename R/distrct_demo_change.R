library(tidyverse)
library(educationdata)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

school_dat <-
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    filters = list(year = 2006:2016)
  )

school_race_dat <- 
  get_education_data(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    filters = list(year = 2006:2016, grade = 99),
    by = list("race")
    )

tmp <-
  school_race_dat %>% 
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

pov_dat <- 
  get_education_data(
    level = "school-districts",
    source = "saipe",
    filters = list(year = 2006:2016))

sum(!unique(school_dat$leaid) %in% unique(pov_dat$leaid))
sum(!unique(pov_dat$leaid) %in% unique(school_dat$leaid))


school_dat <-
  school_dat %>% 
  na_if(-2) %>% 
  na_if(-9)

school_pov_dat <-
  inner_join(school_dat %>% mutate(fips = as.character(fips)), pov_dat) %>% 
  filter(!state_location %in% c("", "VI", "PR", "AP", "AS", "AE", "MP", "GU"))

ccss <-
  tibble(
    state_location = unique(school_pov_dat$state_location),
    ccss_status = 
      c("Repealed",
        "Never",
        "Repealed",
        rep("Formally adopted", 6),
        "Repealed",
        rep("Formally adopted", 4),
        "Repealed",
        rep("Formally adopted", 9),
        rep("Formally adopted", 3),
        "Never",
        rep("Formally adopted", 2),
        "Repealed",
        rep("Formally adopted", 5),
        "Repealed",
        rep("Formally adopted", 3),
        "Repealed",
        rep("Formally adopted", 2),
        "Repealed",
        rep("Formally adopted", 2),
        "Repealed",
        rep("Formally adopted", 4)
        )
  )


school_pov_dat <- left_join(school_pov_dat, ccss)

state_enroll <-
  school_pov_dat %>% 
  group_by(state_location, year) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE),
            total_est_population_5_17 = sum(est_population_5_17, na.rm = TRUE)) %>% 
  mutate(prop_enrolled = total_enrollment/total_est_population_5_17) %>% 
  mutate(prop_enrolled = ifelse(prop_enrolled > 1, 1, prop_enrolled)) %>% 
  mutate(change = prop_enrolled - lag(prop_enrolled))


state_enroll %>% 
  filter(state_location != "VT", year > 2008) %>% 
  ggplot(aes(x = factor(year), y = prop_enrolled, group = state_location)) +
  geom_line(aes(color = state_location), alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(color = state_location), alpha = 0.5, show.legend = FALSE)

state_enroll %>% 
  filter(state_location != "VT", year > 2008) %>% 
  ggplot(aes(x = factor(year), y = change, group = state_location)) +
  geom_line(aes(color = ccss_status), alpha = 0.5)


ccss_status_enroll <-
  school_pov_dat %>% 
  filter(state_location != "VT") %>% 
  group_by(ccss_status, year) %>% 
  summarize(total_enrollment = sum(enrollment, na.rm = TRUE),
            total_est_population_5_17 = sum(est_population_5_17, na.rm = TRUE)) %>% 
  mutate(prop_enrolled = total_enrollment/total_est_population_5_17) %>% 
  mutate(prop_enrolled = ifelse(prop_enrolled > 1, 1, prop_enrolled)) %>% 
  mutate(change = prop_enrolled - lag(prop_enrolled))

ccss_status_enroll %>% 
  filter(year > 2008) %>% 
  ggplot(aes(x = factor(year), y = change, group = ccss_status)) +
  geom_line(aes(color = ccss_status), alpha = 0.5)

ccss_status_enroll %>% 
  ggplot(aes(x = factor(year), y = prop_enrolled, group = ccss_status)) +
  geom_line(aes(color = ccss_status), alpha = 0.5) +
  geom_point(aes(shape = ccss_status, color = ccss_status))


school_pov_dat <-
  school_pov_dat %>% 
  mutate(prop_enrolled = enrollment/est_population_5_17)


# Diversity over time - district level
tmp2 <-
  left_join(school_dat, tmp) %>% 
  na_if(-1) %>% 
  na_if(-2) %>% 
  na_if(-3) %>% 
  na_if(-6) %>% 
  na_if(-9)

diverse_dat <-
  tmp2 %>% 
  select(year:state_leaid, enrollment, white:other) %>% 
  arrange(leaid, year) %>% 
  as_tibble()

diversity <- vegan::diversity(diverse_dat %>% select(white:other) %>% as.matrix(), index = "simpson")

diverse_dat$diversity <- diversity

tmp3 <- diverse_dat %>% 
  filter(
    year %in% c(2006, 2016),
    enrollment != 0,
    !is.na(enrollment),
    !is.na(diversity)) %>% 
  filter_at(vars(white:other), all_vars(. != 0)) %>% 
  group_by(leaid) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>%
  arrange(leaid) %>% 
  select(-n) %>% 
  mutate(change_div = diversity - lag(diversity))
  
  
# Diversity over time - county level
county <-
  tmp2 %>% 
  filter(
    !is.na(county_code),
    !state_location %in% c("HI", "AK"),
    fips <= 56) %>% 
  select(year:state_leaid, county_code, county_name, enrollment, white:other) %>% 
  group_by(county_name, county_code, year) %>% 
  summarize_at(
    vars(enrollment:other), sum, na.rm = TRUE
  ) %>% 
  arrange(county_code, year) %>% 
  ungroup()

county_diversity <- vegan::diversity(county %>% select(white:other) %>% as.matrix(), index = "simpson")

county$diversity <- county_diversity

county <- 
  county %>% 
  filter(
    year %in% c(2006, 2016),
    enrollment != 0,
    !is.na(enrollment),
    !is.na(diversity)) %>% 
  filter_at(vars(white:other), all_vars(. != 0)) %>% 
  group_by(county_code) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>%
  arrange(county_code) %>% 
  select(-n) %>% 
  mutate(change_div = diversity - lag(diversity)) %>% 
  ungroup()

county_div_chg <-
  county %>% 
  filter(year == 2016) %>% 
  mutate(county_code = ifelse(nchar(county_code) < 5, paste0("0", county_code), county_code))

# County geometry
county_geo <- counties(state = NULL)

county_geo <- county_geo %>% st_as_sf()

county_div_chg <- inner_join(county_div_chg, county_geo, by = c("county_code" = "GEOID"))

county_div_chg %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = change_div), size = 0.5)

     

# White change
wh_change <-
  tmp2 %>% 
  filter(
    !is.na(fips),
    fips <= 56
  ) %>% 
  mutate(prop_wh = white /enrollment) %>% 
  group_by(county_code, county_name, year) %>% 
  summarize(white = sum(white, na.rm = TRUE),
            enrollment = sum(enrollment, na.rm = TRUE)) %>% 
  arrange(county_code, year) %>% 
  mutate(prop_wh = white/enrollment) %>% 
  filter(year %in% c(2006, 2016)) %>% 
  group_by(county_code) %>% 
  mutate(change_prop_wh = prop_wh - lag(prop_wh)) %>% 
  filter(year == 2016) 

wh_change <-
  left_join(wh_change, county_geo, by = c("county_code" = "GEOID"))

p1 <- 
  wh_change %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = change_prop_wh), size = 0.5) + theme_void()
ggsave("tmp.png", plot = p1)

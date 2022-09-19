library(tidyverse)
library(tidycensus)

# ACS 1-Year
y_ACS1 <- c(2005:2019, 2021)
names(y_ACS1) <- y_ACS1

# Get variables
# v_acs1_19 <- load_variables(2019, "acs1") %>% mutate(yr = 2019, .before = name)
# write_csv(v_acs1_19, "_data/v_acs1_19.csv")
# v_acs1 <- read_csv("_data/v_acs1.csv") %>% rbind(v_acs1_19)
# write_csv(v_acs1, "_data/v_acs1.csv")

# Get data
# Single variable for Memphis
acs1_single <- map_dfr(y_ACS1, ~ {
  get_acs(
    geography = "place",
    state = "TN",
    variables = "B25001_001",
    year = .x,
    survey = "acs1"
  )
}, .id = "yr") %>%
  filter(str_detect(NAME, "Memphis"))

acs1_single_update <- read_csv("_data/acs1_single.csv") %>% 
  rbind(acs1_single) %>% 
  mutate(yr = as.numeric(yr))

write_csv(acs1_single_update, "_data/acs1_single.csv")

rm(acs1_single, acs1_single_update)

# Multiple variables
acs1_multiple <- map_dfr(y_ACS1, ~ {
  get_acs(
    geography = "place",
    state = "TN",
    table = "B25130",
    summary_var = "B25130_001",
    year = .x,
    survey = "acs1"
  )
}, .id = "yr") %>%
  filter(str_detect(NAME, "Memphis"))

acs1_multiple_update <- read_csv("_data/acs1_multi.csv") %>% 
  rbind(acs1_multiple) %>% 
  mutate(yr = as.numeric(yr))

write_csv(acs1_multiple_update, "_data/acs1_multi.csv")

rm(acs1_multiple, acs1_multiple_update)

# Multiple variables, one year
acs1_multiple <- get_acs(
    geography = "place",
    state = "TN",
    table = "B25131",
    summary_var = "B25131_001",
    year = 2021,
    survey = "acs1"
  ) %>%
  filter(str_detect(NAME, "Memphis")) %>% 
  mutate(yr = 2021, .before = GEOID)

acs1_multiple_update <- read_csv("_data/acs1_multi.csv") %>% 
  rbind(acs1_multiple)

write_csv(acs1_multiple_update, "_data/acs1_multi.csv")

rm(acs1_multiple, acs1_multiple_update)


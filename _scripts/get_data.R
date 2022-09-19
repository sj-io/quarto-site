library(tidyverse)
library(tidycensus)

# ACS 1-Year
y_ACS1 <- c(2005:2019, 2021)
names(y_ACS1) <- y_ACS1

# Get variables
v_acs1_21 <- load_variables(2021, "acs1")
write_csv(v_acs1_21, "_data/v_acs1.csv")

# Reuse variables
v_acs1 <- read_csv("_data/v_acs1.csv")

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

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
acs1 <- map_dfr(y_ACS1, ~ {
  get_acs(
    geography = "place",
    state = "TN",
    variables = "B25071_001",
    year = .x,
    survey = "acs1"
  )
}, .id = "yr") %>%
  filter(str_detect(NAME, "Memphis"))

acs <- read_csv("_data/acs1_single.csv")
acs <- rbind(acs, acs1)

write_csv(acs, "_data/acs1_single.csv")

# Adjust for inflation
# Source: https://data.bls.gov/timeseries/SUUR0000SA0
CCPIU <- read_csv("_data/C-CPI-U.csv") %>% 
  filter(Period == "M06" &
           Year > 2004 &
           !str_detect(Year, "202(0|2)")) %>% 
  mutate(Value = str_remove_all(Value, "\\(U\\)"),
         CCPIU = as.numeric(Value))

# Source: https://fred.stlouisfed.org/series/PCEPI/
PCEPI <- read_csv("_data/PCEPI.csv")

inflation <- PCEPI %>% 
  filter(str_detect(DATE, "-06-") & 
           DATE > "2004-06-01" &
           !str_detect(DATE, "202(0|2)")) %>% 
  bind_cols(CCPIU) %>% 
  select(DATE, PCEPI, CCPIU)

write_csv(inflation, "_data/inflation.csv")

# Source: https://fred.stlouisfed.org/series/PCE
PCE <- read_csv("_data/PCE.csv")

# Playing with variables
# Over cleaning
t1 <- v_acs1_21 %>% 
  mutate(simple = str_remove_all(concept, "IN (PUERTO RICO|THE UNITED STATES)"),
         simple = str_remove_all(simple, paste0("\\(", "(", v_race, ")", ".*", "\\)")),
         simple = str_remove_all(simple, "\\((3|5) TYPES\\)"),
         simple = str_remove_all(simple, "--.*LEVEL.*"),
         simple = str_remove_all(simple, "(FE)?MALE(S)?"),
         simple = str_remove_all(simple, "FOR (CURRENT )?RESIDENCE( 1 YEAR AGO)?"),
         simple = str_remove_all(simple, "FOR WORKPLACE GEOGRAPHY"),
         simple = str_remove_all(simple, "(FULL-TIME, YEAR-ROUND )?CIVILIAN EMPLOYED"),
         simple = str_remove_all(simple, "(AGED )?\\d{1,2} TO \\d{2}( YEARS)?"),
         simple = str_remove_all(simple, "\\d{1,2} YEARS AND OVER"),
         simple = str_remove_all(simple, "(FOR )?THE (CIVILIAN|FOREIGN-BORN )?POPULATION"),
         simple = str_remove_all(simple, "(OWN|RELATED)(?= CHILDREN)"),
         simple = str_remove_all(simple, "(ALL|OCCUPIED)(?= HOUSING)"),
         simple = str_remove_all(simple, "(SINGLE|MULTIPLE)(?= ANCESTRY)"),
         simple = str_remove_all(simple, "(UPPER|LOWER)(?=.*QUARTILE)"),
         simple = str_remove_all(simple, "RATE(S)?"),
         simple = str_squish(simple)) %>% 
  filter(!str_detect(simple, "AGGREGATE|ALLOCATION|UNWEIGHTED"))

# Less cleaning, separate out concept
t2 <- v_acs1_21 %>% 
  mutate(simple = str_remove_all(concept, "IN (PUERTO RICO|THE UNITED STATES)"),
         simple = str_remove_all(simple, paste0("\\(", "(", v_race, ")", ".*", "\\)")),
         simple = str_remove_all(simple, "\\((3|5) TYPES\\)"),
         simple = str_remove_all(simple, "--.*LEVEL.*"),
         simple = str_squish(simple)) %>% 
  filter(!str_detect(simple, "AGGREGATE|ALLOCATION|UNWEIGHTED")) %>% 
  separate(simple, into = c("v1", "v2", "v3", "v4"), sep = " BY ")

t2_2 <- t2 %>% 
  select(starts_with("v")) %>% 
  distinct() %>% 
  pivot_longer(cols = 1:4,
               names_to = "lvl",
               values_to = "value") %>% 
  distinct(value) %>% 
  filter(!is.na(value))

# Less cleaning, separate out labels
t3 <- v_acs1_21 %>%
  mutate(label = str_remove_all(label, "--|:(?!\\d{2} (a|p).m.)")) %>% 
  separate(label, 
           into = c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8"), 
           sep = "!!", fill = "right") %>% 
  select(-v1)

t3_2 <- u1 %>% 
  select(starts_with("v")) %>% 
  pivot_longer(cols = 1:7,
               names_to = "lvl",
               values_to = "value") %>% 
  distinct(value) %>% 
  filter(!is.na(value)) %>% 
  separate(value,
           into = c("v1", "v2", "v3", "v4", "v5", "v6"),
           sep = ";", fill = "right") %>% 
  pivot_longer(cols = 1:6,
               names_to = "lvl",
               values_to = "value") %>% 
  mutate(value = str_squish(value)) %>% 
  distinct(value) %>% 
  filter(!is.na(value)) 

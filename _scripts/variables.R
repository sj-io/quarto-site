# Adding categories to census variables
library(tidyverse)
library(tidycensus)

# Load categories csv
v_cat <- read_csv("_data/v_acs_categories.csv")

# Get census variables
v_acs1_21 <- read_csv("_data/v_acs1_21.csv")
# v_acs1_21 <- load_variables(2021, "acs1")

# Join
v_acs1_cat <- v_acs1_21 %>% 
  mutate(v_start = str_extract_all(name, "^[:alnum:]{3}")) %>% 
  unnest(v_start) %>% 
  left_join(v_cat) %>% 
  select(category, concept, name, label)

# Main variables, housing only
v_hsg_long <- v_acs1_21 %>% 
  filter(str_detect(name, "\\d_")) %>% 
  mutate(v_start = str_extract_all(name, "^[:alnum:]{3}")) %>% 
  unnest(v_start) %>% 
  left_join(v_cat) %>% 
  select(category, concept, name, label) %>% 
  filter(str_detect(category, "Housing"))

# Playing around with variables
# Top-level variables
v_top <- v_acs1_21 %>% 
  filter(str_ends(name, "_001"))

# Main variables
v_short <- v_top %>% 
  filter(!str_ends(name, "\\D_001")) %>% 
  mutate(v_start = str_extract_all(name, "^[:alnum:]{3}")) %>% 
  unnest(v_start) %>% 
  left_join(v_cat) %>% 
  select(category, concept, name, label)

# Main variables, housing only
v_hsg_short <- v_short %>% 
  filter(str_detect(category, "Housing"))

# To-do
v_done_s <- read_csv("_data/acs1_single.csv") %>% 
  distinct(variable)
v_done <- read_csv("_data/acs1_multi.csv") %>% 
  distinct(variable) %>% 
  rbind(v_done_s)

v_todo_short <- v_hsg_short %>% 
  anti_join(v_done, by = c("name" = "variable"))
v_todo_long <- v_hsg_long %>% 
  anti_join(v_done, by = c("name" = "variable"))

# Get labels
v_labs <- v_hsg_long %>% 
  mutate(lab = str_remove_all(label, ":"),
         lab = str_remove_all(lab, ".*(?=!!)"),
         lab = str_remove_all(lab, "!!")) %>% 
  select(name, lab)

write_csv(v_labs, "_data/v_labs.csv")

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

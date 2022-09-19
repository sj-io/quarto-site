# Years
y_ACS1 <- c(2005:2019, 2021)
names(y_ACS1) <- y_ACS1

# Adjust for inflation
# Source: https://data.bls.gov/timeseries/SUUR0000SA0
CCPIU <- read_csv("_data/inflation/C-CPI-U.csv") %>% 
  filter(Period == "M06" &
           Year > 2004 &
           !str_detect(Year, "202(0|2)")) %>% 
  mutate(Value = str_remove_all(Value, "\\(U\\)"),
         CCPIU = as.numeric(Value))

# Source: https://fred.stlouisfed.org/series/PCEPI/
PCEPI <- read_csv("_data/inflation/PCEPI.csv")

inflation <- PCEPI %>% 
  filter(str_detect(DATE, "-06-") & 
           DATE > "2004-06-01" &
           !str_detect(DATE, "202(0|2)")) %>% 
  bind_cols(CCPIU) %>% 
  select(DATE, PCEPI, CCPIU) %>% 
  mutate(yr = y_ACS1)

write_csv(inflation, "_data/inflation.csv")

# Source: https://fred.stlouisfed.org/series/PCE
PCE <- read_csv("_data/inflation/PCE.csv")
# Create data to use for Mike's students

library(dplyr)

eggdata <- read.csv("data/eggdata_for_app.csv")

eggdata_2014 <-
  eggdata %>% 
  filter(Year == 2014) %>%
  mutate(Date = as.Date(Julian_Day, origin = as.Date("2014-01-01"))) %>%
  mutate(Day = lubridate::day(Date))

eggdata_2015 <-
  eggdata %>% 
  filter(Year == 2015) %>%
  mutate(Date = as.Date(Julian_Day, origin = as.Date("2015-01-01"))) %>%
  mutate(Day = lubridate::day(Date))

eggdata_2016 <-
  eggdata %>% 
  filter(Year == 2016) %>%
  mutate(Date = as.Date(Julian_Day, origin = as.Date("2016-01-01"))) %>%
  mutate(Day = lubridate::day(Date))

example_eggdata <- 
  bind_rows(eggdata_2014, eggdata_2015, eggdata_2016) %>%
  mutate(Egg_ID = 1:n()) %>%
  select(-Julian_Day, -Date, -Genus, -Common_Name, -Family_ACGC, -Genus_ACGC, -Common_Name_ACGC) %>%
  select(Egg_ID, Dataset, Site, River, Year, Month, Day, everything())

write.csv(x = example_eggdata, file = "data/example_data.csv", row.names = FALSE)

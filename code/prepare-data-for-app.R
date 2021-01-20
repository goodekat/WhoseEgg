
eggdata141516 <- read.csv(file = "data/eggdata141516.csv")

eggdata_for_app <- 
  eggdata141516 %>%
  mutate(
    Family_ACGC = stringr::str_replace(Family_ACGC, "ACGC", "Invasive Carp"),
    Genus_ACGC = stringr::str_replace(Genus_ACGC, "ACGC", "Invasive Carp"),
    Common_Name_ACGC = stringr::str_replace(Common_Name_ACGC, "ACGC", "Invasive Carp")
  )

write.csv(x = eggdata_for_app, file = "data/eggdata_for_app.csv", row.names = FALSE)

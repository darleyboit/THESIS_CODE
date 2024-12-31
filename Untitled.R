library(readr)
patent2012_precinct <- read_csv("data_cleaned/precinct_data/patent2012_precinct.csv")
View(patent2012_precinct)

print(colnames(patent2012_precinct))

# Rename the 'Age' column to 'Years'
colnames(patent2012_precinct)[colnames(patent2012_precinct) == "lat"] <- "latitude"
colnames(patent2012_precinct)[colnames(patent2012_precinct) == "lng"] <- "longitude"

print(colnames(patent2012_precinct))

write.csv(patent2012_precinct, file = "data_cleaned/precinct_data/patent2012_precinct.csv", row.names = FALSE)

patent2012_precinct <- read_csv("data_cleaned/precinct_data/patent2012_precinct.csv")
View(patent2012_precinct)

library(readr)
draft1precinctvote_veryrough <- read_csv("data_cleaned/precinct_data/draft1precinctvote_veryrough.csv")
View(draft1precinctvote_veryrough)

library(dplyr)
vote_data <- draft1precinctvote_veryrough |>
  filter(state %in% c("MA", "NH", "VT", "RI", "ME", "CT"))

View(vote_data)

library(readr)
vtvote <- read_csv("data_cleaned/vtvote.csv")
View(vtvote)

dataarcgis <- read_csv("data_cleaned/precinct_data/dataarcgis.csv")
vt_patent <- dataarcgis |>
  filter(state == "VT")
View(vt_patent)
write.csv(vt_patent, file = "data_cleaned/precinct_data/vt_patent.csv", row.names = FALSE)

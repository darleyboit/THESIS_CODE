

#goal of this script: cleaning our data set

#loading packages
library(tidyverse)
library(vroom)
library(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data ----

## Voting data ----

library(readr)
votee <- read_csv("data_raw/votee.csv")

votee <- votee |>
  select(election_year, county_name, state, democratic_raw_votes, republican_raw_votes)

votee <- votee |>
  filter(election_year >= 1976)

colnames(votee) <- c("year", "county", "state", "democrat_votes", "republican_votes")
votee$total_votes <- votee$democrat_votes + votee$republican_votes


library(writexl)
write_xlsx(votee, "data_cleaned/votee.xlsx")

## Patent data ---- 

patent <-
  vroom(
    file = "data_raw/dataverse_files/geoc_inv.txt",
    col_select = c("appln_id", "filing_date", "lat", "lng", "ctry_code", "name_1", "name_2")
  )

#selecting only patents filed in the US
usdata <- subset(patent, ctry_code == "US") |>
  group_by(name_1, name_2, filing_date)
head(usdata)

write.csv(usdata, "data_cleaned/only_half_clean/uspatent.csv", row.names = FALSE)






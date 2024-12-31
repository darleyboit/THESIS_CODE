
## SEPARATING VOTING DATA ----
# opening voting data. remember vote diff = rep - dem 
voting <- read_csv("data_cleaned/only_half_clean/clean_voting_data.csv")


view(clean_voting_data)


patent <-
  vroom(
    file = "data_raw/dataverse_files/geoc_inv.txt")

#selecting only patents filed in the US
usdata <- subset(patent, ctry_code == "US")
head(usdata)
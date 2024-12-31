
## SEPARATING VOTING DATA ----
# opening voting data. remember vote diff = rep - dem 
library(readr)
library(dplyr)
library(tidyverse)

# Read the CSV file
voting <- read_csv("data_cleaned/only_half_clean/clean_voting_data.csv")

# Create the new column for the difference
voting <- voting |> 
  mutate(difference_lib_values = democrat_votes - republican_votes)

# Calculate the percentage of liberal values
voting <- voting |> 
  mutate(percentlibvalues = ifelse(democrat_votes != 0, difference_lib_values / democrat_votes, NA))

cleanvoting <- voting |>
  select(county_name, state_po, year, democrat_votes, republican_votes, difference_lib_values, percentlibvalues)

#renaming columns  
colnames(uspatent) <- c("county", "state", "year", "democrat_values", "republican_values", "diff_lib_values", "percent_lib_values")

cleanvoting <- cleanvoting |>
  select(county_name, state_po, year, percentlibvalues) |>
  rename(county = county_name, state = state_po, "%lib_values" = percentlibvalues)

view(cleanvoting)

#Saving as an independent xlsx file
library(writexl)
write_xlsx(cleanvoting, path = "data_cleaned/cleanvoting.xlsx")
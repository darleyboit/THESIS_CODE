
## SEPARATING VOTING DATA ----
# opening voting data. remember vote diff = rep - dem 
library(readr)
library(dplyr)

# Read the CSV file
voting <- read_csv("data_cleaned/only_half_clean/clean_voting_data.csv")

# Create the new column for the difference
voting <- voting |> 
  mutate(difference_lib_values = democrat_votes - republican_votes)

# Calculate the percentage of liberal values
voting <- voting |> 
  mutate(percentlibvalues = ifelse(democrat_votes != 0, difference_lib_values / democrat_votes, NA))

voting <- voting |>
  select(county_name, state_po, year, democrat_votes, republican_votes, difference_lib_values, percentlibvalues)

view(voting)

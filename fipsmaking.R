
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(readxl)
controls <- read_excel("data_cleaned/datawcontrols.xlsx")
data1984 <- controls |>
  filter(year == 1984)
data2012 <- controls |>
  filter(year == 2012)

###

#install.packages("usmap")
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization

head(countypop)
##making sure my dataset is geographically right
fips <- countypop |>
  select(fips, abbr, county) |>
  rename(fips = fips, state = abbr, county = county) 
fips$county <- toupper(fips$county)
fips$county <- gsub("COUNTY", "", fips$county)
fips <- fips |>
  filter(state != "PR" & state != "AK" & state != "HI")
fips$county <- str_replace(fips$county, " PARISH$", "")

fips$state <- str_trim(fips$state)
fips$county <- str_trim(fips$county)

library(writexl)
write_xlsx(fips, path = "data_cleaned/fips.xlsx")

####NEW CODE AREA
geography <- read_excel("data_cleaned/fips.xlsx")

controls$county <- str_trim(controls$county)
geography$county <- str_trim(geography$county)

data <- merge(controls, geography, by = c("state", "county"))
View(data)

#CAPITOL PLANNING REGION
#GREATER BRIDGEPORT PLANNING REGION
#LOWER CONNECTICUT RIVER VALLEY PLANNING REGION
#NAUGATUCK VALLEY PLANNING REGION
#NORTHEASTERN CONNECTICUT PLANNING REGION
#NORTHWEST HILLS PLANNING REGION
#SOUTH CENTRAL CONNECTICUT PLANNING REGION
#SOUTHEASTERN CONNECTICUT PLANNING REGION
#WESTERN CONNECTICUT PLANNING REGION

#FAIRFIELD
#HARTFORD
#LITCHFIELD
#MIDDLESEX
#NEW HAVEN
#NEW LONDON
#TOLLAND
#WINDHAM


write_xlsx(data, path = "data_cleaned/finaldata.xlsx")


#############














merged_data <- merge(geographical_data, data, by = c("county", "state"), all = TRUE) 
View(merged_data)

df_merged <- merged_data |>
  group_by(state, county) %>%
  summarise(across(everything(), ~ coalesce(.x, .y), .names = "merged_{.col}"), .groups = "drop")






plot_usmap(data = merged_data, values = "patents_per_capita", include = .new_england, color = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "patent", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "subtitle") +
  theme(legend.position = "right")




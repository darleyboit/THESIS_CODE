#control variables 

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

controls1990 <- read_csv("data_raw/Controls/controls1990.csv")
View(controls1990)
controls1990 <- controls1990 |>
  rename(county = Geo_NAME, state = Geo_QName, population_over25 = SE_T022_001, count_lessthan_hs = SE_T022_002, count_hs = SE_T022_003,
         count_some_college = SE_T022_004, count_bachelors = SE_T022_005, count_grad_degree = SE_T022_006, total_civilian_labor_employedandnot = SE_T029_001, 
         civilian_unemployed_labor = SE_T029_003, total_households = SE_T041_001, house_less_than_5k = SE_T041_002, house_5to10k = SE_T041_003, 
         house_10to12.5k = SE_T041_004, house_12.5to15k = SE_T041_005, house_15to17.5k = SE_T041_006, house_17.5to20k = SE_T041_007, house_20to22.5k = SE_T041_008,
         house_22.5to25k = SE_T041_009, house_25to27.5k = SE_T041_010, house_27.5to30k = SE_T041_011, house_30to32.5k = SE_T041_012, 
         house_32.5to35k = SE_T041_013, house_35to37.5k = SE_T041_014, house_37.5to40k = SE_T041_015, house_40to42.5k = SE_T041_016, 
         house_42.5to45k = SE_T041_017, house_45to47.5k = SE_T041_018, house_47.5to50k = SE_T041_019, house_50to55k = SE_T041_020, 
         house_55to60k = SE_T041_021, house_60to75k = SE_T041_022, house_75to100k = SE_T041_023, house_100to125k = SE_T041_024, house_125to150k = SE_T041_025, 
         house_over150k = SE_T041_026, per_capita_income_1989_dollars = SE_T065_001)
controls1990 <- controls1990 |> 
  select(county, state, population_over25, count_lessthan_hs, count_hs, count_some_college, count_bachelors, count_grad_degree, total_civilian_labor_employedandnot,
         civilian_unemployed_labor, total_households, house_less_than_5k, house_5to10k, house_10to12.5k, house_12.5to15k, house_15to17.5k, house_17.5to20k, 
         house_20to22.5k, house_22.5to25k, house_25to27.5k, house_27.5to30k, house_30to32.5k,
         house_32.5to35k, house_35to37.5k, house_37.5to40k, house_40to42.5k, house_42.5to45k, house_45to47.5k, house_47.5to50k, house_50to55k, 
         house_55to60k, house_60to75k,house_75to100k, house_100to125k, house_125to150k, house_over150k, per_capita_income_1989_dollars)
controls1990 <- controls1990 |> 
  mutate(year = 1990)
#renaming income bits
controls1990 <- controls1990 |>
  mutate(
    houseunder5k = house_less_than_5k,
    house5to25k = house_5to10k + house_10to12.5k + house_12.5to15k + house_15to17.5k + house_17.5to20k + house_20to22.5k + house_22.5to25k,
    house25to50k = house_25to27.5k + house_27.5to30k, house_30to32.5k + house_32.5to35k + house_35to37.5k + house_37.5to40k + house_40to42.5k + house_42.5to45k + house_45to47.5k + house_47.5to50k,
    house50to75k = house_50to55k + house_55to60k + house_60to75k, 
    house75to100k = house_75to100k, 
    house100to125k = house_100to125k,
    house125to150k = house_125to150k,
    house_over150k = house_over150k
  ) 

controls1990 <- controls1990 |>
  mutate(
    percent_under5k = house_less_than_5k/total_households, 
    percent_5to25k = house5to25k/total_households,
    percent_25to50k = house25to50k/total_households,
    percent_50to75k = house50to75k/total_households,
    percent_75to100k = house75to100k/total_households, 
    percent_100to125k = house100to125k/total_households,
    percent_125to150k = house125to150k/total_households,
    percent_over150k = house_over150k/total_households
  )


controls1990 <- controls1990 |>
  mutate(percent_nohs = count_lessthan_hs/population_over25,
         percent_bachelors = (count_bachelors + count_grad_degree)/population_over25,
         percent_grad = count_grad_degree/population_over25)

controls1990 <- controls1990 |>
  select(county, state, year, percent_nohs, percent_bachelors, percent_grad, percent_under5k, percent_5to25k, 
         percent_25to50k, percent_50to75k, percent_75to100k, percent_100to125k, percent_125to150k, percent_over150k, per_capita_income_1989_dollars)

controls1990 <- controls1990 |>
  mutate(state = gsub(".*,\\s*", "", state))
controls1990 <- controls1990 |>
  mutate(state = recode(state,
                        "Alabama" = "AL",
                        "Alaska" = "AK",
                        "Arizona" = "AZ",
                        "Arkansas" = "AR",
                        "California" = "CA",
                        "Colorado" = "CO",
                        "Connecticut" = "CT",
                        "Delaware" = "DE",
                        "Florida" = "FL",
                        "Georgia" = "GA",
                        "Hawaii" = "HI",
                        "Idaho" = "ID",
                        "Illinois" = "IL",
                        "Indiana" = "IN",
                        "Iowa" = "IA",
                        "Kansas" = "KS",
                        "Kentucky" = "KY",
                        "Louisiana" = "LA",
                        "Maine" = "ME",
                        "Maryland" = "MD",
                        "Massachusetts" = "MA",
                        "Michigan" = "MI",
                        "Minnesota" = "MN",
                        "Mississippi" = "MS",
                        "Missouri" = "MO",
                        "Montana" = "MT",
                        "Nebraska" = "NE",
                        "Nevada" = "NV",
                        "New Hampshire" = "NH",
                        "New Jersey" = "NJ",
                        "New Mexico" = "NM",
                        "New York" = "NY",
                        "North Carolina" = "NC",
                        "North Dakota" = "ND",
                        "Ohio" = "OH",
                        "Oklahoma" = "OK",
                        "Oregon" = "OR",
                        "Pennsylvania" = "PA",
                        "Rhode Island" = "RI",
                        "South Carolina" = "SC",
                        "South Dakota" = "SD",
                        "Tennessee" = "TN",
                        "Texas" = "TX",
                        "Utah" = "UT",
                        "Vermont" = "VT",
                        "Virginia" = "VA",
                        "Washington" = "WA",
                        "West Virginia" = "WV",
                        "Wisconsin" = "WI",
                        "Wyoming" = "WY"
  ))

controls1990 <- controls1990 |>
  mutate(county = toupper(county))
controls1990 <- controls1990 |>
  mutate(county = gsub(" county$", "", county, ignore.case = TRUE))
controls1990 <- controls1990 |>
  filter(!state %in% c("Puerto Rico", "AK", "HI"))

library(writexl)
write_xlsx(controls1990, path = "data_cleaned/controls1990.xlsx")

#loading data
controls <- read_excel("data_cleaned/controls1990.xlsx")
together <- read_excel("data_cleaned/together.xlsx")

controls$county <- str_replace(controls$county, " PARISH$", "")


controls1990 <- merge(controls, together, by = c("county", "state"))
controls1990 <- controls1990 |>
  select(county, state, year.y,  percent_dem, patent, patents_per_capita, democrat_votes, republican_votes, total_votes, percent_nohs, percent_bachelors, percent_grad, per_capita_income_1989_dollars, percent_under5k, 
         percent_5to25k, percent_25to50k, percent_50to75k, percent_75to100k, percent_100to125k, percent_125to150k, percent_over150k)
controls1990 <- controls1990 |>
  rename(year = year.y)

View(controls1990)


write_xlsx(controls1990, path = "data_cleaned/datawcontrols.xlsx")





#2000 data  -- not used atm 
controls2000 <- read_csv("data_raw/Controls/controls2000.csv")
View(controls2000)
controls2000 <- controls2000 |>
  rename(county = Geo_NAME, state = Geo_QName, population_over25 = SE_T040_001, count_lessthan_hs = SE_T040_002, count_hs = SE_T040_003,
         count_some_college = SE_T040_004, count_bachelors = SE_T040_005, count_master_degree = SE_T040_006, count_profschool_degree = SE_T040_007, 
         count_doctorate_degree = SE_T040_008, total_civilian_labor_employedandnot = SE_T073_001, 
         civilian_unemployed_labor = SE_T073_003, total_households = SE_T092_001, house_less_than_10k = SE_T092_002, house_10to15k = SE_T092_003, 
         house_15to20k = SE_T092_004, house_20to25k = SE_T092_005, house_25to30k = SE_T092_006, house_30to35k = SE_T092_007, 
         house_35to40k = SE_T092_008, house_40to45k = SE_T092_009, house_45to50k = SE_T092_010, house_50to60k = SE_T092_011, 
         house_60to75k = SE_T092_012, house_75to100k = SE_T092_013, house_100to125k = SE_T092_014, house_125to150k = SE_T092_015, 
         house_150to200k = SE_T092_016, house_over200k = SE_T092_017, per_capita_income_1999_dollars = SE_T145_001)

controls2000 <- controls2000 |> 
  select(county, state, population_over25, count_lessthan_hs, count_hs, count_some_college, count_bachelors, count_master_degree, count_profschool_degree, count_doctorate_degree,
         total_civilian_labor_employedandnot,
         civilian_unemployed_labor, total_households, house_less_than_10k, house_10to15k, house_15to20k, house_20to25k, house_25to30k, house_30to35k, house_35to40k,
         house_40to45k, house_45to50k, house_50to60k, house_60to75k,house_75to100k, house_100to125k, house_125to150k, house_150to200k, house_over200k, per_capita_income_1999_dollars)
controls2000 <- controls2000 |> 
  mutate(year = 2000)
educontrol2000 <- controls2000 |>
  select(county, state, year, population_over25, count_lessthan_hs, count_hs, count_some_college, count_bachelors, count_master_degree, count_profschool_degree, count_doctorate_degree) |>
  mutate(count_grad_degree = count_master_degree + count_profschool_degree + count_doctorate_degree)

educontrols <- bind_rows(educontrol1990, educontrol2000)











#the goal of this RScript is to make the per capita measure in the patent data

library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(modelsummary) #Angelo says this is good for visualizing data/quickly summarizing

#population data in 1980 
population1980 <- read_csv("~/Desktop/R13776492_SL11.csv")
population1980 <- population1980 |> 
  select(Geo_Name, Geo_QName, SE_T001_001) |> 
  rename(county = Geo_Name, state = Geo_QName, population = SE_T001_001)
population1980 <- population1990 |> 
  mutate(year = 1980)

#population data in 1990
population1990 <- read_csv("~/Desktop/R13766305_SL050.csv")
population1990 <- population1990 |> 
  select(Geo_NAME, Geo_QName, SE_T001_001) |> 
  rename(county = Geo_NAME, state = Geo_QName, population = SE_T001_001)
population1990 <- population1990 |> 
  mutate(year = 1990)

#population data in 2000
population2k <- read_csv("~/Desktop/R13766322_SL050.csv")
population2k <- population2k |> 
  select(Geo_NAME, Geo_QName, SE_T001_001) |> 
  rename(county = Geo_NAME, state = Geo_QName, population = SE_T001_001)
population2k <- population2k |> 
  mutate(year = 2000)

#population data in 2010
population2k10 <- read_csv("~/Desktop/R13766323_SL050.csv")
population2k10 <- population2k10 |> 
  select(Geo_NAME, Geo_QName, SE_T001_001) |> 
  rename(county = Geo_NAME, state = Geo_QName, population = SE_T001_001)
population2k10 <- population2k10 |> 
  mutate(year = 2010)

population <- bind_rows(population1980, population1990, population2k, population2k10) 

population <- population |>
  mutate(state = gsub(".*,\\s*", "", state))
population <- population |>
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

population <- population |> 
  mutate(county = toupper(county))
population <- population |> 
  mutate(county = gsub(" county$", "", county, ignore.case = TRUE))

population <- population |>
  filter(!state %in% c("Puerto Rico", "AK", "HI"))

#exporting population data 
library(writexl)
write_xlsx(population, path = "data_cleaned/population.xlsx")

#reloading it
populationfinal <- read_excel("data_cleaned/population.xlsx")

#step 1: create duplicate values, 2 per census year
correctyears <- populationfinal |>
  bind_rows(
    populationfinal |>
      filter(year == 1980) |> 
      mutate(year = 1984),
    populationfinal |>
      filter(year == 1980) |> 
      mutate(year = 1988)
  ) |>
  bind_rows(
    populationfinal |>
      filter(year == 1990) |> 
      mutate(year = 1992),
    populationfinal |>
      filter(year == 1990) |> 
      mutate(year = 1996)
  ) |>
  bind_rows(
    populationfinal |>
      filter(year == 2000) |> 
      mutate(year = 2004)
  ) |>
  bind_rows(
    populationfinal |>
      filter(year == 2010) |> 
      mutate(year = 2008),
    populationfinal |>
      filter(year == 2010) |> 
      mutate(year = 2012)
  ) |>
  filter(year %in% c(1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012)) |>
  arrange(county, state, year)

####

####

####

#getting both data sets together 
data <- read_excel("data_cleaned/combined.xlsx")

#making it only continental US
data <- data |> 
  filter(!state %in% c("Puerto Rico", "AK", "HI"))

correctyears$county <- str_replace(correctyears$county, " PARISH$", "")

#merging
together <- merge(data, correctyears, by = c("county", "state", "year"))

check_together <- together |>
  filter(state == "LA")
########

###

########

#making patent per capita value 
together$patents_per_capita <- together$patent / together$population

together <- together |>
  mutate(patent = ifelse(is.na(patent), 0, patent))

# making a percent democrat value, too
together$percent_dem <- together$democrat_votes / (together$democrat_votes + together$republican_votes)
# View merged dataset
View(together)


# Making final data
library(writexl)
write_xlsx(together, path = "data_cleaned/together.xlsx")





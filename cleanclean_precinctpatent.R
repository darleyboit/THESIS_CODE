#goal of this script: cleaning our data set

#loading packages
library(tidyverse)
library(vroom)
library(dplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SEPARATING PATENT DATA ---- THIS DOESNT HAVE PER CAPITA MEASURE THO WHICH IS AN ISSUE. 
# opening patent data
setwd("/Users/darleyboit/Desktop")

uspatent <- read_csv("Gov99r_Quant/data_cleaned/only_half_clean/cleanish_patent.csv") 

uspatent_summary <- uspatent |>
  mutate(
    year = case_when(
      year >= 1981 & year <= 1984 ~ "1984",
      year >= 1985 & year <= 1988 ~ "1988",
      year >= 1989 & year <= 1992 ~ "1992",
      year >= 1993 & year <= 1996 ~ "1996",
      year >= 1997 & year <= 2000 ~ "2000",
      year >= 2001 & year <= 2004 ~ "2004",
      year >= 2005 & year <= 2008 ~ "2008",
      year >= 2009 & year <= 2012 ~ "2012",)
  ) |>
  group_by(county, state, year, lat, lng) |>
  summarize(patent_count = n(), .groups = "drop")

uspatent_summary$year <- as.numeric(uspatent_summary$year)


uspatent_summary <- uspatent_summary |>
  filter(year %in% c(1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012))

View(uspatent_summary)

#Saving as an independent csv file
write.csv(uspatent_summary, "Gov99r_Quant/data_cleaned/precinct_patent_clean.csv", row.names = FALSE)

library(readr)
precinct_patent_clean <- read_csv("Gov99r_Quant/data_cleaned/precinct_patent_clean.csv")

patent2012 <- precinct_patent_clean |>
  filter(year == 2012)

View(patent2012)
write.csv(patent2012, "Gov99r_Quant/data_cleaned/patent2012_precinct.csv", row.names = FALSE)








#Combining voting + patent data 
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(modelsummary) #Angelo says this is good for visualizing data/quickly summarizing

patent1 <- read_excel("data_cleaned/countylevel_patent.xlsx")

ggplot(patent1, aes(x = year, y = patent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Patents Filed by Year, Total in the US", 
       x = "Year", 
       y = "Number of Patents Filed") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#combining patent data into year brackets (i'm not sure if this is the most effective way to do this but...)

patent1 <- patent1 |>
  mutate(year = case_when(
    year >= 1981 & year <= 1984 ~ "1984",
    year >= 1985 & year <= 1988 ~ "1988",
    year >= 1989 & year <= 1992 ~ "1992", 
    year >= 1993 & year <= 1996 ~ "1996", 
    year >= 1997 & year <= 2000 ~ "2000",
    year >= 2001 & year <= 2004 ~ "2004",
    year >= 2005 & year <= 2008 ~ "2008",
    year >= 2009 & year <= 2012 ~ "2012",
    TRUE ~ NA_character_  
  )) |>
  group_by(county, year, state) |>
  summarize(patent = sum(patent, na.rm = TRUE), .groups = "drop")  

patent1$patent[is.na(patent1$patent)] <- 0
patent1 <- na.omit(patent1)

#this makes all NA patents turn to 0... then eradicates any rows with NA values -- ie. patents filed in 1980 which isnt a relevant year atm 

#histogram with grouping (tbh this is an ugly graph)
ggplot(patent1, aes(x = year, y = patent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Patents Filed by Year, Total in the US", 
       x = "Year", 
       y = "Number of Patents Filed") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

# Making it compatible
patent <- patent1 |>
  select("county", "state", "year", "patent")
patent <- patent |>
  mutate(year = as.numeric(year))


#load voting data
library(readxl)
votee <- read_excel("data_cleaned/votee.xlsx")
View(votee)

#Joining together
combined <- full_join(votee, patent, by = c("county", "state", "year"))
View(combined)

combined <- combined |>
  filter(year <= 2014)
combined <- combined |>
  filter(year >= 1984)
combined$patent[is.na(combined$patent)] <- 0

combined$patent[is.na(combined$patent)] <- 0

library(openxlsx)
write.xlsx(combined, "data_cleaned/combined.xlsx")





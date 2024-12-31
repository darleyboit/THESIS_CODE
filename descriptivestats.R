library(readxl)
data <- read_excel("data_cleaned/finaldata.xlsx")

#notes to remember: data is only continental US (no AK, no HI)
#patent per capita value is multiplied by 10^5

## STARTING WITH SOME DESCRIPTIVE ANALYSIS OF MY VARIABLES
#FIRST: HOW ARE THEY MADE?
#county and state are purely location-based; only includes the continental US
#year is the exact year for voting information, population info for per capita_patent measure is used from the closest census
#population is from the census, from the nearest census year: 2000+2004 (2000 census), 2008+2012 (2010 census), 2016+2020 (2020 census)
#democrat votes is sheer number of votes for the democratic candidate
#republican votes is sheer number of votes for the republican candidate
#percent dem = dem votes / (rep+dem votes) -- all votes for 3rd parties are not included
#total votes is total number of votes for any candidate -- democrat, republican, or otherwise
#patent is the quantity of patents in each county in that year -- it is NOT the cumulative number of patents. I should probably have another data set for the cumulative number of patents
#patent per capita is the number of patents in that county in that year "patent" divided by population multiplied by 10^5

####################


library(dplyr)
library(tidyverse)
library(ggplot2)

# Summary statistics
summary(data$patents_per_capita)
summary(data$patent)
summary(data$percent_dem)

d1984 <- data |>
  filter(year == 1984)
d2012 <- data |>
  filter(year == 2012)

summary(d1984$percent_dem)
summary(d2012$percent_dem)

# Quartiles
quantiles <- quantile(data$patents_per_capita, na.rm = TRUE)
print(quantiles)

###Looking at percent democrat

# Summary statistics2


#this average isn't doing what I want it to
average_dem <- mean(data$population, na.rm = TRUE)
average_dem




#should think about mean, median, mode. 
#standard deviation
#p values


## I SHOULD ALSO MAP OUT DISTRIBUTIONS OF THESE VARIABLES, have info for quartiles, etc. 
## ranges, box plots, histograms, density plots -- summary statistics

#################note the regression is not a scatterplot dumbass



library(readxl)
data <- read_excel("data_cleaned/finaldata.xlsx")

###MAKING MAPS 
#install.packages("sf")
#install.packages("terra")
#install.packages("spData")
#install.packages("leaflet")
#install.packages('spDataLarge',
repos='https://nowosad.github.io/drat/', type='source')

#install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
"https://cloud.r-project.org"))
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

##understanding what functions do what 
#tm_fill(): shaded areas for (multi)polygons
#tm_borders(): border outlines for (multi)polygons
#tm_polygons(): both, shaded areas and border outlines for (multi)polygons
#tm_lines(): lines for (multi)linestrings
#tm_symbols(): symbols for (multi)points, (multi)linestrings, and (multi)polygons
#tm_raster(): colored cells of raster data (there is also tm_rgb() for rasters with three layers)
#tm_text(): text information for (multi)points, (multi)linestrings, and (multi)polygons

###

#install.packages("usmap")
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization

data[is.na(data)] <- 0

data2012 <- data |>
  filter(year == 2012)
data2008 <- data |>
  filter(year == 2008)
data2004 <- data |>
  filter(year == 2004)
data2000 <- data |>
  filter(year == 2000)
data1996 <- data |>
  filter(year == 1996)
data1992 <- data |>
  filter(year == 1992)
data1988 <- data |>
  filter(year == 1988)
data1984 <- data |>
  filter(year == 1984)


#map percent dem
plot_usmap(data = data1984, values = "percent_dem", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "red", high = "blue", name = "Percent Democrat", label = scales::comma) + 
  labs(title = "Democrats in the US", subtitle = "1984 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())
plot_usmap(data = data2000, values = "percent_dem", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "red", high = "blue", name = "Percent Democrat", label = scales::comma) + 
  labs(title = "Democrats in the US", subtitle = "2000 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())
plot_usmap(data = data2012, values = "percent_dem", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "red", high = "blue", name = "Percent Democrat", label = scales::comma) + 
  labs(title = "Democrats in the US", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

#histogram of patents filed by year




summary(data2012$patent)
excludingoutliers <- data2012 |>
  filter(patents_per_capita <= 1.239e-03)

plot_usmap(data = excludingoutliers, values = "patent", regions = "counties", color = "lightgrey") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Pat perCap", label = scales::comma) + 
  labs(title = "Title", subtitle = "subtitle") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

onlytopthird <- data2012 |>
  filter(patent >= 60)

plot_usmap(data = onlytopthird, values = "patents_per_capita", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "lightblue", high = "darkblue", name = "Pat perCap", label = scales::comma) + 
  labs(title = "Title", subtitle = "subtitle") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())




plot_usmap(data = data, values = "pct_pov_2021", include = c("CT", "ME", "MA", "NH", "VT"), color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "Poverty Percentage Estimates for New England Counties in 2014") +
  theme(legend.position = "right")

##




###

###

###

#making sure it's only continental USA for simplicity sake
data <- data |>
  filter(!state %in% c("Puerto Rico", "AK", "HI"))
View(data)
##### 

zero <- data |>
  filter(patent == 0)
head(zero)

histogram_noninnovative <- ggplot(zero, aes(x = percent_dem)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Democrats in Non Innovative County", x = "Percent Dem", y = "Frequency")
histogram_noninnovative
summary(zero$percent_dem)
summary(data$percent_dem)
summary(most_innovative$percent_dem)
#non innovative counties are SLIGHTLY more conservative, but not by a ton overall 

#understanding most innovative counties 
summary(data$patent)
most_innovative <- data |>
  filter(patent >= 40000)
summary(most_innovative$percent_dem)

super_innovative <- data |>
  filter(patent >= 8000)
summary(super_innovative$percent_dem)
View(super_innovative)

#new york data 
View(data)
nyc <- data |>
  filter(state == "NY")
View(nyc)


summary(most_innovative$patents_per_capita)
head(most_innovative)
histogram_most_innovative <- ggplot(most_innovative, aes(x = percent_dem)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Democrats in Non Innovative County", x = "Percent Dem", y = "Frequency")
histogram_most_innovative


# Year 1992
data1992 <- data |>
  filter(year == 1992)

y1992 <- ggplot(data1992, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Scatter Plot of Lib Values vs Patents", 
       x = "Liberal Values", 
       y = "Patents") + 
  theme_minimal() 
y1992


# Year 2012 scatter plot -- LINEAR 
data2012 <- data |>
  filter(year == 2012)

y2012 <- ggplot(data2012, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Scatter Plot of Lib Values vs Patents", 
       x = "Liberal Values", 
       y = "Patents") + 
  theme_minimal() 
y2012

##REGRESSION PLOTS AFTER; YEAR OVER YEAR 
filtered_data <- data |>
  filter(patents_per_capita <= 5000)

regression <- ggplot(filtered_data, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = " Plot of Percent Democrats vs Patents per Capita", 
       x = "Liberal Values", 
       y = "Patents per Capita") + 
  theme_minimal() 
regression

View(data)

##getting equation of trendline: y = 6.44*x - 103.39
model <- lm(patents_per_capita ~ percent_dem, data = data)
coefficients <- coef(model)
equation <- paste("y =", round(coefficients[1], 2), "+", round(coefficients[2], 2), "* x")
print(equation)

###########

###QUADRATIC FUNCTIONS
y20
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = data)
coefficients_quadratic <- coef(model_quadratic)

equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 0 + 0 * x + 0 * x^2" 

ggplot(data, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patents per Capita vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))



###GRAVEYARD
# Plot using FIPS codes
plot_usmap(data = data2012, values = "percent_dem", regions = "counties", include = .new_england) +
  scale_fill_continuous(low = "white", high = "blue", name = "Democratic Percentage", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "Democratic Percentage by County") +
  theme(legend.position = "right")



plot_usmap(data = data1984, values = "patents_per_capita", regions = "counties") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Patent Count", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "Patent Distribution by County") +
  theme(legend.position = "right")

plot_usmap(data = data2012, values = "patents_per_capita", regions = "counties") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Patent Count", label = scales::comma) + 
  labs(title = "New England Region", subtitle = "Patent Distribution by County") +
  theme(legend.position = "right")

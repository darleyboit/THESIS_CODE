
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(readxl)
data <- read_excel("data_cleaned/finaldata.xlsx")

data <- data |>
  mutate(percent_dem = as.numeric(percent_dem) * 100)

#making sure it's only continental USA for simplicity sake
data <- data |>
  filter(!state %in% c("Puerto Rico", "AK", "HI"))
##### 

#top 5% innovative counties by quantity -- looking at their value lean 
percentile_95 <- quantile(data2012$patent, 0.95, na.rm = TRUE)
top_5_percent <- data2012[data2012$patent >= percentile_95, ]
summary(top_5_percent$percent_dem)

#####

summary(data$patent)

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

###doing value change over time 

changetime <- data |>
  filter(year == c(1984, 2012))
changetime <- changetime |>
  filter(!is.na(percent_dem)) |>
  group_by(county, state, fips) |>
  filter(any(year == 1984) & any(year == 2012)) |>
  mutate(percent_dem_change = percent_dem[year == 2012] - percent_dem[year == 1984]) |>
  ungroup()
changetime <- changetime |>
  select(state, county, fips, percent_dem_change)
finalchange <- unique(changetime)
finalchange <- finalchange |>
  mutate(percent_dem_change = abs(percent_dem_change))

View(finalchange)
summary(finalchange$percent_dem_change)

plot_usmap(data = finalchange, values = "percent_dem_change", regions = "counties", color = "lightgrey") + 
  scale_fill_continuous(low = "white", high = "red", na.value = "white", name = "Patent Quantity", label = scales::comma) + 
  labs(title = "Top 5% Counties in Innovation", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())


library(readr)
cleanpatent <- read_csv("data_cleaned/cleanpatent.csv")
pat <- cleanpatent |>
  filter(year == 2012)
write.csv(pat, "data_cleaned/dataarcgis.csv", row.names = FALSE)

getwd()





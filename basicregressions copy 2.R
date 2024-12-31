library(readxl)
data <- read_excel("data_cleaned/together.xlsx")
View(data)
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

library(cartography)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
data <- read_excel("data_cleaned/together.xlsx")

data <- data |>
  mutate(percent_dem = as.numeric(percent_dem) * 100)
View(data)

#making sure it's only continental USA for simplicity sake
data <- data |>
  filter(!state %in% c("Puerto Rico", "AK", "HI"))


###AVERAGE COUNTY DEMOGRAPHY DATA
summary(data$population)
#looking at changes at beginning of 20 year period vs. end of it
data1992 <- data |>
  filter(year == 1992)
summary(data1992$population)
data2012 <- data |>
  filter(year == 2012)
summary(data2012$population)

####HOW PEOPLE VOTE
summary(data$percent_dem)
#histogram to show
histogram_percent_dem <- ggplot(data, aes(x = percent_dem)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Democrat Percentage", x = "Percent of Voters who are Democrats", y = "Frequency") + 
  theme(  panel.background = element_rect(fill = "white"),  # Panel background
          plot.background = element_rect(fill = "white"),   # Overall plot background
          panel.grid.major = element_line(color = "light gray"),      # Major grid lines
          panel.grid.minor = element_line(color = "white")       # Minor grid lines
  )
histogram_percent_dem
#have there been big changes here? 
data1992 <- data |>
  filter(year == 1992)
summary(data1992$percent_dem)
data2012 <- data |>
  filter(year == 2012)
summary(data2012$percent_dem)

####PATENTS PER CAPITA
summary(data$patents_per_capita)

data1992 <- data |>
  filter(year == 1992)
summary(data1992$patents_per_capita)
data2012 <- data |>
  filter(year == 2012)
summary(data2012$patents_per_capita)

##I REALLY NEED TO CHECK THIS 
"red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red"

total_patents_ea_year <- data |>
  group_by(year) |>
  summarise(total_patents = sum(patent, na.rm = TRUE))
print(total_patents_ea_year)
patents_ea_year_histogram <- ggplot(total_patents_per_year, aes(x = year, y = total_patents)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Total Number of Patents Filed by Year", 
       x = "Year", 
       y = "Total Patents") +
  theme(  panel.background = element_rect(fill = "white"),  
          plot.background = element_rect(fill = "white"),  
          panel.grid.major = element_line(color = "light gray"),    
          panel.grid.minor = element_line(color = "white") 
  )
patents_ea_year_histogram
"red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red" "red"

#histogram: per cap distribution 
patents_percap_distribution <- ggplot(data, aes(x = patents_per_capita)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "blue") +
  labs(title = "Histogram of Patents Per Capita", x = "Patents Per Capita", y = "Frequency") + 
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(color = "light gray"),  
          panel.grid.minor = element_line(color = "white")       
  )
patents_percap_distribution

#REGRESSION
#scatterplot with regression line
scatterplot <- ggplot(data, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Scatter Plot of Counties by Percentage of Democrats vs Patents per Capita", 
       x = "Percent of Democrats", 
       y = "Patents per Capita") + 
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(color = "light gray"),  
          panel.grid.minor = element_line(color = "white")       
  )
scatterplot

#trying this again while controlling for outliers 
datacontrolled <- data |>
  filter(patents_per_capita <= 5000)

scatterplot <- ggplot(datacontrolled, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Scatter Plot of Counties by Percentage of Democrats vs Patents per Capita", 
       x = "Percent of Democrats", 
       y = "Patents per Capita") + 
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(color = "light gray"),  
          panel.grid.minor = element_line(color = "white")       
  )
scatterplot

#trying it as a quadratic instead
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = datacontrolled)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic)
data_plot <- ggplot(datacontrolled, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patents per Capita vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))
data_plot

##TAKING QUADRATIC RECESSION LINE FOR EACH YEAR AND PUTTING THOSE TOGETHER. 
#equations for my trends by year: 
#1992: [1] "y = 194.93 + -1.84 * x + 0 * x^2"
#1996: [1] "y = 91.76 + 1.84 * x + -0.02 * x^2"
#2000: [1] "y = 144.44 + -2.17 * x + 0.05 * x^2"
#2004: [1] "y = 202.01 + -6.44 * x + 0.13 * x^2"
#2008: [1] "y = 120.73 + -2.9 * x + 0.1 * x^2"
#2012: [1] "y = 110.16 + -1.78 * x + 0.08 * x^2"
x_values <- seq(0, 100, by = 0.1)
f_1992 <- function(x) {194.93 + -1.84 * x + 0 * x^2}
f_1996 <- function(x) {91.76 + 1.84 * x + -0.02 * x^2}
f_2000 <- function(x) {144.44 + -2.17 * x + 0.05 * x^2}
f_2004 <- function(x) {202.01 + -6.44 * x + 0.13 * x^2}
f_2008 <- function(x) {120.73 + -2.9 * x + 0.1 * x^2}
f_2012 <- function(x) {110.16 + -1.78 * x + 0.08 * x^2}
yeardata <- data.frame(
  x = x_values,
  year_1992 = f_1992(x_values),
  year_1996 = f_1996(x_values),
  year_2000 = f_2000(x_values),
  year_2004 = f_2004(x_values),
  year_2008 = f_2008(x_values),
  year_2012 = f_2012(x_values)
)

data_long <- pivot_longer(yeardata, cols = -x, names_to = "year", values_to = "y")
labels <- data.frame(
  year = c("1992", "1996", "2000", "2004", "2008", "2012"),
  x = rep(8, 6),  
  y = c(194.93, 91.76, 144.44, 202.01, 120.73, 110.16)  
)
charttt <- ggplot(data_long, aes(x = x, y = y, color = factor(year))) + 
  geom_line(size = 1) +  
  labs(title = "Relationship Trend By Year",
       x = "Percent Democrat",
       y = "Patents per Capita",
       color = "Year") +
  scale_color_manual(values = c("year_1992" = "red", "year_1996" = "orange", "year_2000" = "yellow", "year_2004" = "green", "year_2008" = "blue", "year_2012" = "purple")) +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))
charttt

#DOING THE SAME OVER AGAIN, BUT WITH LINES INSTEAD OF QUADRATICS
##equations below (only looking at per capita patents under 5k)

#1992: "y =  152.78  +  -1.02  * x"
#1996: "y =  124.12  +  -0.09  * x"
#2000: "y =  51.65  +  2.07  * x"
#2004: "y =  -0.31  +  4.21  * x"
#2008: "y =  -48.16  +  5.32  * x"
#2012: "y =  -14.81  +  4.68  * x"

x_values <- seq(0, 100, by = 0.1)
f_1992 <- function(x) { 152.78 + -1.02 * x }
f_1996 <- function(x) { 124.12 + -0.09 * x }
f_2000 <- function(x) { 51.65 + 2.07 * x }
f_2004 <- function(x) { -0.31 + 4.21 * x }
f_2008 <- function(x) { -48.16 + 5.32 * x }
f_2012 <- function(x) { -14.81 + 4.68 * x }

yeardata <- data.frame(
  x = x_values,
  year_1992 = f_1992(x_values),
  year_1996 = f_1996(x_values),
  year_2000 = f_2000(x_values),
  year_2004 = f_2004(x_values),
  year_2008 = f_2008(x_values),
  year_2012 = f_2012(x_values)
)

data_long <- pivot_longer(yeardata, cols = -x, names_to = "year", values_to = "y")

labels <- data.frame(
  year = c("1992", "1996", "2000", "2004", "2008", "2012"),
  x = c(10, 10, 10, 10, 10, 10),  
  y = c(f_1992(10), f_1996(10), f_2000(10), f_2004(10), f_2008(10), f_2012(10))  
)

linearyears <- ggplot(data_long, aes(x = x, y = y, color = factor(year))) +
  geom_line(size = 1) +  # Add lines for each year
  labs(title = "Relationship Trend By Year (Linear Relationship)",
       x = "Percent Democrat",
       y = "Patents per Capita",
       color = "Year") +
  scale_color_manual(values = c("year_1992" = "red", "year_1996" = "orange", "year_2000" = "yellow", "year_2004" = "green", "year_2008" = "blue", "year_2012" = "purple")) +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))  
linearyears













###########EVERYTHING LOWER IS OLD CODE

#this looks at it but closer up.. 
patents_percap_distribution_controlled <- data[data$patents_per_capita < 250, ]
hist1 <- ggplot(greaterthan0, aes(x = patents_per_capita)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Patents Per Capita", x = "Patents Per Capita", y = "Frequency")
patents_percap_distribution_controlled


###SCATTER PLOTS BY YEAR BEFORE REGRESSIONS WOULD BE GOOD 


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


=== 
  
  
filtered_data <- data |>
  filter(patents_per_capita <= 5000) 

model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = data)

coefficients_quadratic <- coef(model_quadratic)

equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 334.15 + -16.6 * x + 0.27 * x^2" 

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

#### YEAR BY YEAR 
#2000
data2000 <- data |>
  filter(year == 2000)
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = data2000)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")

data2000_plot <- ggplot(data2000, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "2000 Patents per Capita vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))
data2000_plot

library(readxl)
data <- read_excel("data_cleaned/together.xlsx")

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

#Making a percent_dem value, which divides the share of democrats by the total votes for democrats and republicans (NOT TOTAL VOTES)
colnames(data) <- c("county", "state", "year", "population", "democrat_votes", "republican_votes", "total_votes", "patent", "patents_per_capita")
data$percent_dem <- 100 * data$democrat_votes / (data$democrat_votes + data$republican_votes) #note this is not divided by total votes so that it's just a binary

View(data)
#Average patents per capita = 155.0593
average_patent_per_capita <- mean(data$patents_per_capita, na.rm = TRUE)
average_patent_per_capita

# Summary statistics
summary(data$patents_per_capita)
# Quartiles
quantiles <- quantile(data$patents_per_capita, na.rm = TRUE)
print(quantiles)

#HISTOGRAM for patents per capita; def not a normal distribution or close to it
library(ggplot2)
patents_percap_distribution <- ggplot(data, aes(x = patents_per_capita)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Patents Per Capita", x = "Patents Per Capita", y = "Frequency") + 
  theme(  panel.background = element_rect(fill = "white"),  # Panel background
          plot.background = element_rect(fill = "white"),   # Overall plot background
          panel.grid.major = element_line(color = "light gray"),      # Major grid lines
          panel.grid.minor = element_line(color = "white")       # Minor grid lines
  )
patents_percap_distribution

#HISTOGRAM for patent   quantity; not population controlled
patentss <- ggplot(data, aes(x = patent)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Patents, Quantity", x = "Patents Per Capita", y = "Frequency") + 
  theme(  panel.background = element_rect(fill = "white"),  # Panel background
          plot.background = element_rect(fill = "white"),   # Overall plot background
          panel.grid.major = element_line(color = "light gray"),      # Major grid lines
          panel.grid.minor = element_line(color = "white")       # Minor grid lines
  )
patentss


#this looks at it but closer up.. 
patents_percap_distribution_controlled <- data[data$patents_per_capita < 250, ]
hist1 <- ggplot(greaterthan0, aes(x = patents_per_capita)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Patents Per Capita", x = "Patents Per Capita", y = "Frequency")
patents_percap_distribution_controlled


### LOOKING AT OTHER VARIABLES
#Average patents quantity = 363.4011; Santa Clara, CA is the most innovative in terms of quantity, year over year
average_patent <- mean(data$patent, na.rm = TRUE)
average_patent
#this looks at the same thing but closer up; can also just cut the top bit and change data to "data"
patent_quant_controlled <- data[data$patent > 2000, ]
hist1 <- ggplot(patent_quant_controlled, aes(x = patent)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Patent Quantity by County", x = "Patent Quantity", y = "Frequency")
hist1


#Average population  = 119k people/county
average_pop <- mean(data$population, na.rm = TRUE)
average_pop

# Summary statistics
summary(data$population)

# Quartiles
quantiles <- quantile(data$population, na.rm = TRUE)
print(quantiles)

#Looking at voting data stuff 
hist2 <- ggplot(data, aes(x = democrat_votes)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Democrats by County", x = "Democrats", y = "Frequency")
hist2

hist3 <- ggplot(data, aes(x = republican_votes)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Republicans by County", x = "Republicans", y = "Frequency")
hist3

##EXCEPT I REALLY NEED A MEASURE TO PUT THESE TWO TOGETHER
hist4 <- ggplot(data, aes(x = percent_dem)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Histogram of Percent Democrats by County", x = "Democrat Percent", y = "Frequency")
hist4

#looking just at the middle 80% 
Qx <- quantile(data$percent_dem, 0.1, na.rm = TRUE)
Qy <- quantile(data$percent_dem, 0.9, na.rm = TRUE)
middle_data <- data$percent_dem[data$percent_dem >= Qx & data$percent_dem <= Qy]
hist(middle_data, main="Histogram of Middle 80% of Percent Dem", xlab="Values", breaks=30)


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

## YEAR 2000 ----- 
# Selecting data from year 2000
data2020 <- data |>
  filter(year == 2020)

# Year 2000 scatter plot
xx <- ggplot(data, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Scatter Plot of Lib Values vs Patents", 
       x = "Liberal Values", 
       y = "Patents") + 
  theme_minimal() 
xx

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

#2004
data2004 <- data |>
  filter(year == 2004)
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = data2004)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")

data2004_plot <- ggplot(data2004, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patents per Capita vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))
data2004_plot

#2020
data2020 <- data |>
  filter(year == 2020)
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = data2020)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic)

data2020_plot <- ggplot(data2020, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "2020 Patents per Capita vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))
data2020_plot



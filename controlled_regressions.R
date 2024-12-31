library(readxl)
data <- read_excel("data_cleaned/finaldata.xlsx")
View(data)
#basic regression
summary(data$per_capita_income_1989_dollars)
#  min =4152, mean = 11077, median =  10673, middle 50% =  9469 to 120076, max = 28381



#HISTOGRAM for patents per capita; def not a normal distribution or close to it
library(ggplot2)

littledata <- data |>
  filter(patent <= 10)

earlydata <- data |>
  filter(year <= 1990)
View(earlydata)

ggplot(littledata, aes(x = year, y = patent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Patents Filed Each Year",
       x = "Year", 
       y = "Number of Patents") +
  theme_minimal()


######KMS

data <- read.csv("data_cleaned/cleanpatent.csv")
View(data)

data$year <- as.numeric(as.character(data$year))


xx <- data |>
  filter(year <= 1990)



View(xx)











#HIGH INCOME
highincome <- data |>
  filter(per_capita_income_1989_dollars >= 11461) |>
  filter(year == 1992) |>
  filter(patents_per_capita <= 5000)

#lets graph it
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = highincome)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 308.47 + -1097.08 * x + 2150.42 * x^2"

ggplot(highincome, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) + 
  labs(title = "Patents per Capita vs. Percent Democratic in 1992: Higher Income", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#LOW INCOME 
lowincome <- datawcontrols |>
  filter(per_capita_income_1989_dollars <= 11461) |>
  filter(year.y == 1992) |>
  filter(patents_per_capita <= 5000)
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = lowincome)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) [1] "y = 194.3 + -416.78 * x + 289.32 * x^2"

ggplot(lowincome, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) + 
  labs(title = "Patents per Capita vs. Percent Democratic in 1992: Lower Income", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))


##LETS DO THE SAME FOR EDUCATION 

summary(datawcontrols$percent_uni_or_more)

datawcontrols <- datawcontrols |>
  mutate(percent_uni_or_more = percent_bachelors + percent_grad)

upper_nohs <- datawcontrols |>
  filter(percent_uni_or_more <= 0.1) |>
  filter(year.y == 1992) |>
  filter(patents_per_capita <= 5000)

#lets graph it
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = upper_nohs)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 26.92 + 179.13 * x + -235.7 * x^2"

ggplot(upper_nohs, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) + 
  labs(title = "Patents per Capita vs. Percent Democratic in 1992: Higher Income", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))


##CONTROLLING FOR EDUCATION AND INCOME
upper_edu_and_income <- datawcontrols |>
  filter(percent_uni_or_more >= 0.3) |>
  filter(per_capita_income_1989_dollars >= 11461) |>
  filter(year.y == 1996) |>
  filter(patents_per_capita <= 5000)

#lets graph it
model_quadratic <- lm(patents_per_capita ~ percent_dem + I(percent_dem^2), data = upper_edu_and_income)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 26.92 + 179.13 * x + -235.7 * x^2"

ggplot(upper_edu_and_income, aes(x = percent_dem, y = patents_per_capita)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) + 
  labs(title = "Patents per Capita vs. Percent Democratic in 1992: Higher Income", 
       x = "Percent Democratic", 
       y = "Patents Per Capita") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))



##graveyard

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

####

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

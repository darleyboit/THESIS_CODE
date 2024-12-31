data <- read_excel("Gov99r_Quant/data_cleaned/finaldata.xlsx")
data$patent[is.na(data$patent)] <- 0
data$patents_per_capita[is.na(data$patents_per_capita)] <- 0
#############
library(ggplot2) #use ggplot2 to add layer for visualization

#making a bipartisanship measure

data$bipartisanship <- 100 * (1 - abs(data$percent_dem - 50) / 50)
View(xx)

xx <- data |>
  select(county, year, percent_dem, bipartisanship)

data$bipartisanship <- 100 * (1 - abs(data$percent_dem - 0.5) / 50)
df$bipartisanship <- 100 * (1 - abs(df$percent_democrat - 0.5) / 50)

##RAIF SAYS: 

###100 - differnece between the two 
####absolute value of the difference between the percent of dem and rep ad then subtract from 100 

# Check for any value greater than 100
any(data$bipartisanship > 1)

data1984 <- data |>
  filter(year == 1984)
data1988 <- data |>
  filter(year == 1988)
data1992 <- data |>
  filter(year == 1992)
data1996 <- data |>
  filter(year == 1996)
data2000 <- data |>
  filter(year == 2000)
data2004 <- data |>
  filter(year == 2004)
data2008 <- data |>
  filter(year == 2008)
data2012 <- data |>
  filter(year == 2012)

##REGRESSION PLOTS AFTER; YEAR OVER YEAR 
summary(data2012$patent)

filtered1984 <- data1984 |>
  filter(patent > 3)
regression <- ggplot(filtered1984, aes(x = percent_dem, y = patent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Percent Democrats vs. Patent Quantity", 
       subtitle = "1984 County Level Data",
       x = "Liberal Values", 
       y = "Patent Quantity") + 
  theme_minimal() 
regression

#quadratic patent 1984
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data1984)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = -42.36 + 447.85 * x + -308.94 * x^2"
ggplot(data1984, aes(x = percent_dem, y = patent)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  scale_y_continuous(limits = c(0, 1000)) +  
  labs(title = "Patent Quantity vs. Percent Democrat", 
       subtitle = "National 1984 County Level Data",
       x = "Percent Democrat", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 1988
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data1988)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[[1] "y = -30.61 + 408.32 * x + -238.15 * x^2"

ggplot(data1988, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 1992
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data1992)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #y = -74.37 + 447.58 * x + -58.65 * x^2

ggplot(data1992, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 1996
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data1996)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 9.9 + -190.75 * x + 969.75 * x^2"

ggplot(data1996, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 2000
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data2000)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 441.24 + -3292.69 * x + 6298.7 * x^2"

ggplot(data2000, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 2004
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data2004)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 441.24 + -3292.69 * x + 6298.7 * x^2"

ggplot(data2004, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 2008
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data2008)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1][1] "y = 974.64 + -6922.94 * x + 11900.55 * x^2"

ggplot(data2008, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#quadratic patent 2012
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = data2012)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 657.85 + -5333.58 * x + 10363.4 * x^2"

ggplot(data2012, aes(x = percent_dem, y = patent)) +
  geom_point(color = "black") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  scale_y_continuous(limits = c(0, 1000)) +  
  labs(title = "Patent Quantity vs. Percent Democrat",
       subtitle = "National 2012 County Level Data",
       x = "Percent Democrat", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))



#####


###


#####


###MAPPING THE PATENT YEAR QUADRATIC LINES TO SEE CHANGE OVER TIME
x <- seq(0, 1, length.out = 400)  # Adjust the range of x as needed
y_1984 <- -42.36 + 447.85 * x + (-308.94) * x^2
y_1988 <- -30.61 + 408.32 * x + (-238.15) * x^2
y_1992 <- -74.37 + 447.58 * x + (-58.65) * x^2
y_1996 <- 9.9 + (-190.75) * x + 969.75 * x^2
y_2000 <- 441.24 + (-3292.69) * x + 6298.7 * x^2
y_2004 <- 441.24 + (-3292.69) * x + 6298.7 * x^2
y_2008 <- 974.64 + (-6922.94) * x + 11900.55 * x^2
y_2012 <- 657.85 + (-5333.58) * x + 10363.4 * x^2

df <- data.frame(
  x = rep(x, 8), 
  y = c(y_1984, y_1988, y_1992, y_1996, y_2000, y_2004, y_2008, y_2012),
  year = rep(c("1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012"), each = length(x)))

ggplot(df, aes(x = x, y = y, color = year)) +
  geom_line() +
  labs(title = "Value vs. Innovation Relationship Over Time", x = "Percent Democrat", y = "Patent Quantity", color = "Year") +
  theme_minimal() +
  theme(legend.position = "right")




####
####
####



##CONTROLS FOR INCOME PER CAP 
##NEW CODE
nolow_quartile_inc <- data2012 |>
  filter(per_capita_income_1989_dollars >= 9469)

model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = nolow_quartile_inc)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 1717.47 + -12523.21 * x + 21352.04 * x^2"

ggplot(nolow_quartile_inc, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  scale_y_continuous(limits = c(0, 1000)) +  
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democrats",
       subtitle = "2012 Data: Excluding the Lowest Quartile in Income per Capita",
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))


##OLD CODE
data2012 <- data2012 %>%
  rename(percap_income = per_capita_income_1989_dollars)
summary(data2012$percap_income)

bottom25 <- data2012 |>
  filter(percap_income <= 9469)
bottom50totop25 <- data2012 |>
  filter(percap_income >= 9469)
bottom50totop25 <- bottom50totop25 |>
  filter(percap_income <= 10674)
top50tobottom75 <- data2012 |>
  filter(percap_income >= 10674)
top50tobottom75 <- top50tobottom75 |>
  filter(percap_income <= 12076)
top25 <- data2012 |>
  filter(percap_income >= 12076)

#bottom25
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = bottom25)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 58.67 + -210.94 * x + 206.01 * x^2"

ggplot(bottom25, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#bottom 25-50
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = bottom50totop25)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 15.69 + -24.2 * x + 130.21 * x^2"

ggplot(bottom50totop25, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#50-75
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = top50tobottom75)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = -43.08 + 407.42 * x + -241.02 * x^2"

ggplot(top50tobottom75, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))


#top25
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = top25)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 2236 + -15756 * x + 27664.94 * x^2"

ggplot(top25, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

###MAPPING THE PATENT YEAR QUADRATIC LINES TO SEE CHANGE OVER TIME
x <- seq(0, 1, length.out = 400)  

bottom25 <- 58.67 + (-210.94 * x) + (206.01 * x^2)
secondquartile <- 15.69 + (-24.2 * x) + (130.21 * x^2)
thirdquartile <- -43.08 + (407.42 * x) + (-241.02 * x^2)
top25 <- 2236 + (-15756 * x) + (27664.94 * x^2)

df_uni <- data.frame(
  x = rep(x, 4), 
  y = c(bottom25, secondquartile, thirdquartile, top25),
  year = rep(c("first_quartile", "second_quartile", "third_quartile", "fourth_quartile"), each = length(x)))

ggplot(df_uni, aes(x = x, y = y, color = year)) +
  geom_line() +
  labs(title = "Trendlines by Income per Capita", 
       x = "Percent Democrat", y = "Patent Quantity", 
       color = "Year",
       subtitle = "2012 County Level Data") +
  theme_minimal() +
  theme(legend.position = "bottom")


#####


####
#
####


#####

##CONTROLS FOR EDUCATION
summary(data2012$percent_bachelors)

#NEW CODE: JUST EXCLUDING THE LOWEST QUARTILE
nolow_quartile_edu <- data2012 |>
  filter(percent_bachelors >= 0.09135)

model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = nolow_quartile_edu)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 805.06 + -6514.63 * x + 12587.19 * x^2"

ggplot(nolow_quartile_edu, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  scale_y_continuous(limits = c(0, 1000)) +  
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democrats",
       subtitle = "2012 Data: Excluding the Lowest Quartile in College Graduates",
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))


####OLD CODE: 
bottom25_uni <- data2012 |>
  filter(percent_bachelors <= 0.09135)
bottom50totop25_uni <- data2012 |>
  filter(percent_bachelors >= 0.09135)
bottom50totop25_uni <- bottom50totop25_uni |>
  filter(percent_bachelors <= 0.11718)
top50tobottom75_uni <- data2012 |>
  filter(percent_bachelors >= 0.11718)
top50tobottom75_uni <- top50tobottom75_uni |>
  filter(percent_bachelors <= 0.15419)
top25_uni <- data2012 |>
  filter(percent_bachelors >= 0.15419)
#bottom25
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = bottom25_uni)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = -9.91 + 124 * x + -144.46 * x^2"

ggplot(bottom25_uni, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#bottom 25-50
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = bottom50totop25_uni)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = -24.23 + 243.5 * x + -247.72 * x^2"

ggplot(bottom50totop25_uni, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

#50-75
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = top50tobottom75_uni)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 21.42 + -121.13 * x + 761.05 * x^2"

ggplot(top50tobottom75_uni, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))


#top25
model_quadratic <- lm(patent ~ percent_dem + I(percent_dem^2), data = top25_uni)
coefficients_quadratic <- coef(model_quadratic)
equation_quadratic <- paste("y =", round(coefficients_quadratic[1], 2), 
                            "+", round(coefficients_quadratic[2], 2), "* x +",
                            round(coefficients_quadratic[3], 2), "* x^2")
print(equation_quadratic) #[1] "y = 1339.08 + -10364.57 * x + 20400.55 * x^2"

ggplot(top25_uni, aes(x = percent_dem, y = patent)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Add quadratic fit
  labs(title = "Patent Quantity vs. Percent Democratic by County; Quadratic Regression", 
       x = "Percent Democratic", 
       y = "Patent Quantity") +
  theme(  panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "light gray"),
          panel.grid.minor = element_line(color = "white"))

###MAPPING THE PATENT YEAR QUADRATIC LINES TO SEE CHANGE OVER TIME
x <- seq(0, 1, length.out = 400)  

bottom25 <- -9.91 + (124 * x) + (-144.46 * x^2)
secondquartile <- -24.23 + (243.5 * x) + (-247.72 * x^2)
thirdquartile <- 21.42 + (-121.13 * x) + (761.05 * x^2)
top25 <- 1339.08 + (-10364.57 * x) + (20400.55 * x^2)

df_uni <- data.frame(
  x = rep(x, 4), 
  y = c(bottom25, secondquartile, thirdquartile, top25),
  year = rep(c("first_quartile", "second_quartile", "third_quartile", "fourth_quartile"), each = length(x)))

ggplot(df_uni, aes(x = x, y = y, color = year)) +
  geom_line() +
  labs(title = "Trendlines by College Graduation", 
       x = "Percent Democrat", y = "Patent Quantity", 
       color = "Year",
       subtitle = "2012 County Level Data") +
  theme_minimal() +
  theme(legend.position = "bottom")


#####

###

#####


##LOOKING AT HOW MANY COUNTIES OVERLAP IN THE TOP BUNCH OF CONTROLS 
summary(data2012$percent_bachelors)

#seeingoverlap in top 25% of values
top25_uni <- data2012 |>
  filter(percent_bachelors >= 0.15419)
top25 <- data2012 |>
  filter(percap_income >= 12076)

overlap_rows <- intersect(top25, top25_uni)
overlap_count <- nrow(overlap_rows)
print(overlap_count)
501/765
View(top25)
View(top25_uni)

###THIS CODE ISN'T QUITE RUNNING RIGHT ATM... 
#seeing overlap in top 33% of values  --- there's not as much tbh
sorted_data_uni <- data2012[order(data2012$percent_bachelors, decreasing = TRUE), ]
top_1_3rd_uni <- sorted_data_uni[1:(nrow(sorted_data_uni) %/% 3), ]

sorted_data_inc <- data2012[order(data2012$percap_income, decreasing = TRUE), ]
top_1_3rd_inc <- sorted_data_inc[1:(nrow(sorted_data_inc) %/% 3), ]

overlap_rows_third <- intersect(top_1_3rd_inc, top_1_3rd_uni)
overlap_count_third <- nrow(overlap_rows_third)
print(overlap_count)

View(top_1_3rd_inc)
View(top_1_3rd_uni)
501/765


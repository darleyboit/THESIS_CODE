data <- read_excel("Gov99r_Quant/data_cleaned/finaldata.xlsx")
data$patent[is.na(data$patent)] <- 0
data$patents_per_capita[is.na(data$patents_per_capita)] <- 0
#############
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization

#making annual data
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

#no longer have code for percent dem, but shouldn't be hard to recreate... 

#map of patent concentration -- bottom 50%
summary(data1984$patent)
summary(data2012$percent_dem)

highp <- data2012 |>
  filter(percent_dem >= .375)
middlep <- highp |>
  filter(percent_dem <= .6)


#### FOR THE TABLE I MADE ON PERCENT DEM IN HIGHER INNOVATION BRACKETS
summary(data2012$patent)

highpat <- data2012 |>
  filter(patent >= 68)
middlepat <- highp |>
  filter(patent >= 10)
summary(highpat$percent_dem)
summary(middlepat$percent_dem)

top5percent <- data2012 |>
  filter(patent >= 1300)

vvhighpat <- data2012 |>
  filter(patent >= 5750)

summary(highpat$percent_dem)
summary(vvhighpat$percent_dem)


###MAPPING AGAIN
plot_usmap(data = middlep, values = "patent", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "pink", high = "red", na.value = "white", name = "Patent Quantity", label = scales::comma) + 
  labs(title = "Top 50% Counties in Innovation", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

summary(data2012$patent)
summary(highp$patent)
summary(middlep$patent)






highp <- data |>
  filter(patent >= 10)
plot_usmap(data = highp, values = "patent", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "pink", high = "darkred", na.value = "white", name = "Percent Democrat", label = scales::comma) + 
  labs(title = "Bottom 50% Counties in Innovation", subtitle = "1984 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())


#map of patent per capita concentration -- need to make this by year 

summary(data1984$patents_per_capita)
summary(data2012$patents_per_capita)

highp <- data1984 |>
  filter(patents_per_capita >= 0.0001389)

plot_usmap(data = highp, values = "patents_per_capita", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "pink", high = "darkred", na.value = "white", name = "Patents per Capita", label = scales::comma) + 
  labs(title = "Bottom 50% Counties in Innovation", subtitle = "1984 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

highp <- data2012 |>
  filter(patents_per_capita >= 0.000383)
plot_usmap(data = highp, values = "patents_per_capita", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "pink", high = "darkred", na.value = "white", name = "Patents per Capita", label = scales::comma) + 
  labs(title = "Bottom 50% Counties in Innovation", subtitle = "1984 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

###

highp <- data |>
  filter(patent <= 6)

plot_usmap(data = highp, values = "patents_per_capita", regions = "counties", color = "black") + 
  scale_fill_continuous(low = "black", high = "pink", na.value = "white", name = "Percent Democrat", label = scales::comma) + 
  labs(title = "Bottom 50% Counties in Innovation", subtitle = "1984 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())




###MAKING 2 MAPS TO OVERLAY -- 2012
#TOP 1/3 INNOVATIVE COUNTIES
#TOP 1/3 %DEM

summary(data2012$percent_dem)

top3patent <- data2012 |>
  filter(patent > 68)
plot_usmap(data = top3patent, values = "patent", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "red", high = "red", na.value = "white", name = "Patents per Capita", label = scales::comma) + 
  labs(title = "Top 1/3 Counties in Innovation", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())
top2value <- data2012 |>
  filter(percent_dem > .38)
plot_usmap(data = top3value, values = "percent_dem", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "blue", high = "blue", na.value = "white", name = "Patents per Capita", label = scales::comma) + 
  labs(title = "Top 1/3 Counties in Democrats", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

leftvalues <- data2012 |>
  filter(percent_dem >= 0.48)
rightvalues <- data2012 |>
  filter(percent_dem <= 0.28)
superleftvalues <- data2012 |>
  filter(percent_dem >= 0.72)

common_rows <- intersect(top3patent, superleftvalues)
n_common_rows <- nrow(common_rows)
print(paste("Number of common rows:", n_common_rows))

View(leftvalues)


View(top3patent)
View(top3value)

#346 number common rows
#765 entries top 3 value
#763 entries for top 3 pat 


####

top3patent <- data2012 |>
  filter(patent > 3000)
plot_usmap(data = top3patent, values = "patent", regions = "counties", color = "white") + 
  scale_fill_continuous(low = "red", high = "black", na.value = "white", name = "Patents per Capita", label = scales::comma) + 
  labs(title = "Top 1/3 Counties in Innovation", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

summary(data2012$patent)


plot_usmap(data = top_5_percent, values = "percent_dem", regions = "counties", color = "lightgrey") + 
  scale_fill_continuous(low = "red", high = "blue", na.value = "white", name = "Patent Quantity", label = scales::comma) + 
  labs(title = "Top 5% Counties in Innovation", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())

summary(top_5_percent$percent_dem)



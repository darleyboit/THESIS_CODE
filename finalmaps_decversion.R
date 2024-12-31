#CODE USED FOR FINAL MAP FIGURES

#top 5% innovative counties by quantity 
percentile_95 <- quantile(data2012$patent, 0.95, na.rm = TRUE)
top_5_percent <- data2012[data2012$patent >= percentile_95, ]
View(top_5_percent)

plot_usmap(data = top_5_percent, values = "patent", regions = "counties", color = "lightgrey") + 
  scale_fill_continuous(low = "darkblue", high = "black", na.value = "white", name = "Patent Quantity", label = scales::comma) + 
  labs(title = "Top 5% Counties in Innovation", subtitle = "2012 County Level Data") + 
  theme(
    legend.position = "right",
    panel.border = element_blank())


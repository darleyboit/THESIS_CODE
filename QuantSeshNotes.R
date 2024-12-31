#Quant workshop notes

#want to do log of outcome variable, but idk why 
#regressions allow u to predict into the future 
#outcome (dependent variable) as y, explanatory (independent) variable as x

#thinking about the intercept 
#model estimated value of our outcome when our covariates is set to 0 
#predicted value of line is intercept + [beta(X) multiplied by a value of x]
#intercept is only substantively important bc it helps us get predicted outcome 
#intercept is not super important to a regression table; only important once doing predicted outcome work 

library(readxl)
combined <- read_excel("data_cleaned/combined.xlsx")
View(combined)

detop_mod <- 
  lm(
    data = combined,
    formula = %libvalues - patent
  )

#should have deeper convo w advisor about if linear regressions are the best way to show 

#predicted outcome is given some example value of the predictor variable

#standard data -- think about confidence intervals 
#think about z scores 
#1.96 is the right number bc it's a standard deviation 

detop_coef_95lower <- detop_coef_predictor - 1.96*detop_coef_se
detop_coef_95upper <- detop_coef_predictor + 1.96*detop_coef_se
cat(round(detop_coef_95lower, 3), round(detop_coef_95upper, 3), sep ", ")

#r^2 represents the amount of variation that ur model specification explains, so if it's low, ur model doesn't explain all the variation... which is not great

#stepping into some multivariate regression 
detop_mult <-
  lm(
    data = detop
    formula = log_detainees ~ white_border + 
      foreign + log police #as you can see there are multiple variables here... for the multivariate regression
  )
summary(detop_mult)

#multivaraite model with fixed effects 
detop_multfe <- 
  feols(
    data = detop,
    fl = log_detainees ~ white_border +
      foregin + log_police | factor(county_fips)
  )
summary(detop_mult)
#this controls for county specific ideosynchrisies
#saying given fixed accounts of factor 

#nice table to compare model results 
modelsummary(
  models = list(
    detop_mod,
    detop_mult,
    detop_multfe
  ))
#this will make a really nice little table




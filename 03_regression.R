library(tidyverse)

# Linear regression will be the first model tested. It assumes linearity,
# therefore, a somewhat normally distributed dataset is favourable.

# Checking skewness of original charges
ggplot(insurance_ready, aes(x=charges)) +
    geom_histogram(bins=25)
# This histogram shows the distribution of charges.
# Charges appear somewhat right-skewed.

# Trying log transformation
ggplot(insurance_ready, aes(x=log(charges))) +
    geom_histogram(bins=25)
# The charges became left skewed instead.

# Trying sqrt transformation
ggplot(insurance_ready, aes(x=sqrt(charges))) +
    geom_histogram(bins=25)
# The charges are relatively normally distributed now.

sqrt_charges <- sqrt(insurance_ready$charges)



# Fit the model
model <- lm(
    data = insurance_ready,
    sqrt_charges ~ smoker + exercise_level + prior_accidents + age)

# View the summary
summary(model)
coef(model)

# Predict charges on original scale
predicted_charges <- (predict(model))^2

par(mfrow = c(2,2))
plot(model)

# The results from the regression model show that the included variables
# are statistically significant with a p < 0.01 which means that
# these variables have a clear connection to the charges.

# The most important variable is smoking with a coefficient of around 34.7
# and p < 0.001. This indicates that smokers have significantly higher
# charges even when other variables are held constant.

# Exercise level has an effect on charges albeit not as much as smoking.
# Compared to high exercise level, low and medium exercise level contribute
# to higher charges.

# The model also shows that the number of prior accidents is associated with
# higher charges and that aging increases charges gradually.

# The model has a coefficient of determination of around 51% (r² = 0.516)
# which means that it explains a big part of the variation. However,
# there are still other factors that plays a part.

# The model is limited by the fact that not all variables were used, only
# a few that were deemed interesting were included. Since the model only
# explains 51% of the variation, it is clear that there are other important
# factors to the charges that were not captured. However, the results are
# realistic and consistent with expectations.


# Self reflection:
# I believe that my analysis and interpretation of the data using graphs and
# a regression model was done well. It shows a clear correlation to charges. 
# The hardest part was choosing variables for the analysis.
# All could have been used but that seemed unnecessary
# and overly complicated, so a few were chosen instead. According to the
# grade assessment I would say that this assignment is on a VG level 
# with the exception of "Fördjupad regressionsanalys"






















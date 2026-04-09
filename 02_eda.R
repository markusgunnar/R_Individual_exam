library(tidyverse)

glimpse(insurance_ready)

# Questions to answer:
# 1.  How are charges distributed among bmi_categories?  DONE
# 2.  How are charges distributed among number of children DONE
# 3a. Does more children increase smoking frequency? DONE
# 3b. Do smokers have higher charges? DONE
# 4.  Are charges lower between smoker/non-smokers based on exercise level? DONE

# ---------------------------------
# ---------- Bmi boxplot ----------
# ---------------------------------
insurance_ready %>%
    group_by(bmi_category) %>%
    summarise(
        mean = mean(charges, na.rm=TRUE),
        median = median(charges, na.rm=TRUE),
        sd = sd(charges, na.rm=TRUE)
    )
ggplot(insurance_ready, aes(x=bmi_category, y=charges)) +
    geom_boxplot() +
    labs(x="Bmi category", y="Charges")
insurance_ready %>%
    count(bmi_category)

# Charges do not appear to be strongly influenced by BMI category.
# The "healthy weight" category has slightly lower charges compared to 
# "obesity" and "overweight". This suggests that there may be relevance
# The "underage", "underweight" and "unknown" categories have very few 
# observations (33, 31, 28) making a reliable conclusion difficult.

# --------------------------------------
# ---------- Children boxplot ----------
# --------------------------------------
insurance_ready %>%
    group_by(children) %>%
    summarise(
        mean = mean(charges, na.rm=TRUE),
        median = median(charges, na.rm=TRUE),
        sd = sd(charges, na.rm=TRUE)
    )
ggplot(insurance_ready, aes(x=as.character(children), y=charges)) +
    geom_boxplot() +
    labs(x="Children", y="Charges")
insurance_ready %>%
    count(children)

# The number of children seem to have little effect on the charges.
# Four children appear to increase charges slightly.
# However, the few observations of 4-5 children (76, 57) make it hard to 
# draw a reliable conclusion.

# -----------------------------------------------------
# ---------- Children vs Smoking and Charges ----------
# -----------------------------------------------------
ggplot(insurance_ready, aes(x = factor(children), y = charges, fill = smoker)) +
    geom_boxplot() +
    labs(x="Children", y="Charges")

# This boxplot shows charges across the number of children, separated by
# smoker status. The number of children has little effect on charges which is
# consistent with previous observations. However, smokers have a significant
# increase in charges compared to non-smokers.

# ----------------------------------------------------------
# ---------- Smoker vs Exercise Level and Charges ----------
# ----------------------------------------------------------
ggplot(insurance_ready, aes(x = factor(smoker), y = charges, fill=exercise_level)) +
    geom_boxplot() +
    labs(x="Smoker", y="Charges")

# This boxplot compares charges between smokers and non-smokers, separated by
# exercise level. Consistent with previous observation, higher charges
# for smokers. In contrast, high and medium exercise levels do show a
# tendency to decrease charges but the effect is relatively small
# compared to smokers and non-smokers.

# ------------------------------------------------
# ---------- Prior Accidents vs Charges ----------
# ------------------------------------------------
ggplot(insurance_ready, aes(x=as.character(prior_accidents), y=charges)) +
    geom_boxplot() +
    scale_y_log10() +
    labs(x="Prior accidents", y="Charges (log scale)")
insurance_ready %>%
    count(prior_accidents)

# This boxplot shows the charges over the number of prior accidents.
# The data is skewed, therefore, using a log scale is appropriate to minimize
# the effect of few observations and outliers (28 and 4 observations for 2-3
# prior accidents).
# Charges have a tendency to increase with the number of prior accidents.


# The interesting variables to explore further are smoker, exercise_level and
# prior_accidents. Smokers have significantly higher charges regardless of 
# children and exercise level. The number of prior accidents show a tendency to
# increase charges.
# High and medium exercise level may lower charges as observed.

# Smoker, exercise_level, prior_accidents










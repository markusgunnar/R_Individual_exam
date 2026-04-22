library(tidyverse)


# -------------------------------
# ---------- LOAD DATA ----------
# -------------------------------
insurance_raw <- read_csv("insurance_costs.csv")


glimpse(insurance_raw)
# The tibble is of size 1100x14
# The existing data types are chr and dbl


# ------------------------------------------------------------------------
# ---------- CHECK DATA TYPES, MISSING VALUES AND UNIQUE VALUES ----------
# ------------------------------------------------------------------------
colSums(is.na(insurance_raw))
insurance_raw %>%
    filter(if_any(everything(), is.na))
# Missing values in bmi, exercise_level and annual_checkups

# Check for unique values. The obvious ones are:
# Sex: (male, female)
# Smoker, chronic_condition: (yes, no)
print(
    insurance_raw %>%
        summarise(across(everything(), ~ n_distinct(.))),
    width=Inf
)

# Check spelling mistakes
unique(insurance_raw$region)
# region has 6 unique values (should be 4)
# ("east"  "west"  "south" "north" "North" "South")

unique(insurance_raw$smoker)
# smoker has 3 unique values (should be 2)
# ("no"  "yes" "Yes")

unique(insurance_raw$exercise_level)
# exercice_level is clean, change NA to "unknown"

unique(insurance_raw$plan_type)
# plan_type has 5 unique values (should be 3)
# ("basic"    "standard" "premium"  "Premium"  "Standard")

# ---------------------------
# ---------- CLEAN ----------
# ---------------------------

# Change data types first.
# Age -> int, children -> int, smoker -> lgl, chronic_condition -> lgl,
# prior_accidents -> int, prior_claims -> int
insurance_clean <- insurance_raw %>%
    mutate(
        age = as.integer(age),
        children = as.integer(children),
        prior_accidents = as.integer(prior_accidents),
        prior_claims = as.integer(prior_claims),
        region = str_to_lower(region),
        smoker = str_to_lower(smoker),
        plan_type = str_to_lower(plan_type),
        exercise_level = replace_na(exercise_level, "unknown"),
        across(c(smoker, chronic_condition), ~ .x == "yes")
    )

glimpse(insurance_clean)

# Check if something was missed
colSums(is.na(insurance_clean))
print(
    insurance_clean %>%
        filter(if_any(everything(), is.na)),
    width=Inf
)

# Choosing not to replace NA in bmi and annual_checkups as there are
# so few of them compared to the size of the data set

# -----------------------------------
# ---------- NEW VARIABLES ----------
# -----------------------------------

# Adding new lgl variable sick_and_smoking,
# TRUE when smoker and chronic_condition is TRUE
# Adding new chr variable bmi_category 
# based on https://www.cdc.gov/bmi/adult-calculator/bmi-categories.html
# Ages under 20 will be treated as "underage", NA bmi as "unknown"

insurance_ready <- insurance_clean %>%
    mutate(
        sick_and_smoking = smoker & chronic_condition,
        bmi_category = case_when(
            is.na(bmi) ~ "unknown",
            age < 20 ~ "underage",
            bmi < 18.5 ~ "underweight",
            bmi >= 18.5 & bmi < 25 ~ "healthy weight",
            bmi >= 25 & bmi < 30 ~ "overweight",
            bmi >= 30 ~ "obesity",
            TRUE ~ NA_character_
        )
    )

glimpse(insurance_ready)
unique(insurance_ready$bmi_category)

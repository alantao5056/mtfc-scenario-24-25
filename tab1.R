library(tidyverse)
library(janitor)

# read tab1
tab1 = read_csv("data/tab1.csv")

# clean tab1
tab1 =
tab1 |>
  clean_names()

tab1 =
tab1 |>
  mutate(
    annual_medical_costs = ifelse(
      is.na(annual_medical_costs),
      NA,
      parse_number(annual_medical_costs)
    ),
    metformin_costs = ifelse(
      is.na(metformin_costs),
      NA,
      parse_number(metformin_costs)
    ),
    insulin_costs = ifelse(
      is.na(insulin_costs),
      NA,
      parse_number(insulin_costs)
    ),
    annual_pharmacy_costs = ifelse(
      is.na(annual_pharmacy_costs),
      NA,
      parse_number(annual_pharmacy_costs)
    ),
    date_of_a1c_test = parse_date(date_of_a1c_test, "%m/%d/%Y")
  )

# Part 2 #5
q5 =
tab1 |>
  summarize(annual_medical_costs = sum(annual_medical_costs),
            annual_pharmacy_costs = sum(annual_pharmacy_costs))

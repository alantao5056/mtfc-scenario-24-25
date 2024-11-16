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

# Part 2 #6
q6 =
tab1 |>
  mutate(total_claim = ifelse(is.na(insulin_claims), 0, insulin_claims) +
                       ifelse(is.na(metformin_claims), 0, metformin_claims)) |>
  summarize(total_claim_mean = mean(total_claim),
            total_claim_sd = sd(total_claim))
# Part 2 #7
q7 =
tab1 |>
  summarize(avg_cost = mean(annual_pharmacy_costs),
            .by = med_scripts)

# Part 2 #8
q8a =
tab1 |>
  filter(annual_pharmacy_costs > 0) |>
  count() |>
  mutate(percent = n/500)

q8b =
tab1 |>
  count(med_scripts) |>
  mutate(percent = n/500)

# Part 2 #9

q9 =
tab1 |>
  mutate(med_scripts = ifelse(is.na(med_scripts), "None", med_scripts)) |>
  summarize(total_cost = sum(annual_medical_costs + annual_medical_costs),
            .by = med_scripts) |>
  add_row(med_scripts = "Total", total_cost = 12003471)|>
  arrange(total_cost)

q9 |>
  ggplot(aes(x = fct_reorder(med_scripts, total_cost), y = total_cost)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  geom_col() +
  theme_bw() +
  labs(x = "Medication",
       y = "Annual Medical Cost",
       title = "Annual Medical Cost of Individuals With Diabetes Medications",
       subtitle = "Metformin is the most expensive")
q9


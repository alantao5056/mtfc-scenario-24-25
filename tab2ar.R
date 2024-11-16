library(tidyverse)
library(janitor)

a = read_csv("data/tab2ar.csv")

a =
a |>
  clean_names() |>
  mutate(asian_only = na_if(asian_only, "- - -")) |>
  mutate(asian_only = parse_number(asian_only)) |>
  mutate(hispanic_or_latino = na_if(hispanic_or_latino, "- - -")) |>
  mutate(hispanic_or_latino = parse_number(hispanic_or_latino)) |>
  rename(year = category)

a =
a |>
  mutate(all_persons = all_persons / 100)

fit = lm(all_persons~year, a)

xCoef = fit[[1]][[2]]
constant = fit[[1]][[1]]
r2 = summary(fit)$r.squared

a |>
  ggplot(aes(x = year, y = all_persons)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  annotate("text", x=2013, y=0.068, label = "y = 0.002x - 3.931", size = 4) +
  annotate("text", x=2013, y=0.0655, label = paste("r^2 = ", signif(r2, 6), sep = ""), size = 4) +
  labs(title = "Physician-diagnosed Diabetes for All Persons",
       subtitle = "increasing at a rate of 0.2% each year",
       x = "Year",
       y = "Percent of Population")

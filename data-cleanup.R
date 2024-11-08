library(tidyverse)

# read tab1
tab1 = read_csv("data/tab1.csv")

# clean tab1
tab1 |>
  rename(
    id = `Member ID`,
    age = Age,
    
  )
  mutate(`Metformin Costs` = ifelse(
    is.na(`Metformin Costs`),
    NA,
    as.numeric(str_replace(`Metformin Costs`, "\\$", "")))
  )

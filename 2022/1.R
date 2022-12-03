# advent of code 2022
# day 1
# irmoodie

library(tidyverse)
library(here)

read_table(file = here("2022", "input1.txt"), na = "", skip_empty_rows = FALSE, col_names = "cal") |>
  mutate(elf = cumsum(is.na(cal))+1) |> 
  group_by(elf) |> 
  summarise(total_cal = sum(cal, na.rm = TRUE)) |> 
  pull(total_cal) |> 
  max()

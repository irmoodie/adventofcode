library(tidyverse)

secret_plan <- read_table(file = here("2022", "input2.txt"), col_names = c("p", "q"))

pos_res <-
  crossing(p = c(1:3), q = c(1:3)) |> # all possible games (RPS = 123)
  mutate(
    res = ((q - 1) - (p - 1)) %% 3, # returns 0 for tie, 1 for win, 2 for lose
    res = as.numeric(chartr("012", "360", res)), # translate to score
    score = q + res
  )

# Part 1 ------------------------------------------------------------------

secret_plan |>
  mutate(
    p = as.numeric(chartr("ABC", "123", p)),
    q = as.numeric(chartr("XYZ", "123", q))
  ) |> 
  left_join(pos_res) |> 
  pull(score) |>
  sum()

# Part 2 ------------------------------------------------------------------

secret_plan |> 
  mutate(
    res = as.numeric(chartr("XYZ", "036", q)),
    p = as.numeric(chartr("ABC", "123", p))
  ) |> 
  select(-q) |> 
  left_join(pos_res) |> 
  pull(score) |> 
  sum()
  
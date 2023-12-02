library(tidyverse)

draws <- read_lines("2023/input2.txt")

# Part 1

max_draws <- max(str_count(draws, ";") + 1)

game_data <-
  tibble(draws) |>
  separate_wider_delim(draws, ":", names = c("game", "draws")) |>
  separate_wider_delim(draws, ";", names = c(paste0(rep("draw", max_draws), 1:max_draws)), too_few = "align_start") |>
  pivot_longer(cols = starts_with("draw"), names_to = "draw", values_to = "result") |>
  separate_wider_delim(result, ",", names = c(paste0(rep("type", 3), 1:3)), too_few = "align_start") |>
  pivot_longer(cols = starts_with("type"), names_to = "type", values_to = "result") |>
  mutate(
    result = str_remove_all(result, " "),
    colour = str_remove_all(result, "\\d"),
    count = as.numeric(str_remove_all(result, "\\D"))
    ) |>
  filter(!is.na(result)) |>
  select(-result, -type) |>
  pivot_wider(names_from = colour, values_from = count)

max_game_data <-
  game_data |>
  group_by(game) |>
  summarise(red = max(red, na.rm = TRUE), green = max(green, na.rm = TRUE), blue = max(blue, na.rm = TRUE))

max_game_data |>
  filter(red <= 12 & green <= 13 & blue <= 14) |>
  pull(game) |>
  str_remove_all("\\D") |>
  as.numeric() |>
  sum()

# Part 2

max_game_data |>
  mutate(power = red * green * blue) |>
  pull(power) |>
  sum()

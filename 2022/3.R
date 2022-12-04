library(tidyverse)

priority_of_items <-
  tibble(
    common_item = append(letters, LETTERS),
    priority = 1:52
  )

backpacks <- read_table(file = here("2022", "input3.txt"), col_names = "backpack_contents")


# Part 1 ------------------------------------------------------------------

get_common <- function(a, b){
  
  x <- str_split(a, pattern = "", simplify = TRUE)
  y <- str_split(b, pattern = "", simplify = TRUE)
  
  z <- paste(intersect(x, y), collapse = "")
  
  return(z)
  
}

backpacks |> 
  mutate(
    n_items = str_length(backpack_contents),
    comp1 = str_sub(backpack_contents, start = 1, end = n_items/2),
    comp2 = str_sub(backpack_contents, start = n_items/2 + 1, end = n_items),
    common_item = pmap_chr(list(comp1, comp2), get_common)
    ) |> 
  left_join(priority_of_items) |> 
  pull(priority) |> 
  sum()


# Part 2 ------------------------------------------------------------------

backpacks |> 
  mutate(
    elf_group = rep(row_number(), length.out = n(), each = 3),
    elf_id = rep(c("b1", "b2", "b3"), length.out = n(), each = 1)
    ) |> 
  pivot_wider(names_from = elf_id, values_from = backpack_contents) |> 
  mutate(
    common_item = pmap_chr(list(b1, b2), get_common),
    common_item = pmap_chr(list(common_item, b3), get_common)
  ) |> 
  left_join(priority_of_items) |> 
  pull(priority) |> 
  sum()

library(tidyverse)
library(here)

is_contained <- function(elfA, elfB){
  
  a <- str_split_fixed(elfA, "-", 2)
  b <- str_split_fixed(elfB, "-", 2)
  
  return(any(all(a[1]:a[2] %in% b[1]:b[2]), all(b[1]:b[2] %in% a[1]:a[2])))
  
}

is_overlap <- function(elfA, elfB){
  
  a <- str_split_fixed(elfA, "-", 2)
  b <- str_split_fixed(elfB, "-", 2)
  
  return(any(a[1]:a[2] %in% b[1]:b[2]))
  
}

read_delim(file = here("2022", "input4.txt"), delim = ",", col_names = c("elfA", "elfB")) |> 
  mutate(
    contained = pmap_chr(list(elfA, elfB), is_contained),
    overlap = pmap_chr(list(elfA, elfB), is_overlap)
    ) |> 
  count(overlap, contained)

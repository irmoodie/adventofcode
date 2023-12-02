cali_doc <- readLines("2023/input1.txt")

# Part 1

get_cali_vals <- function(doc) {
  num_dat <- gsub("\\D", "", doc)
  n <- nchar(num_dat)
  a <- substr(num_dat, 1,1)
  b <- substr(num_dat, n, n)
  return(as.numeric(paste0(a,b)))
}

get_cali_vals(cali_doc) |> sum()

# Part 2

numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

cali_doc_fixed <- vector()

for (string in cali_doc) {
  max_nchar <- max(nchar(string))
  for (j in 1:max_nchar) {
    x <- substr(string, j, j+4)
    for (i in seq_along(numbers)){
      y <- unlist(gregexpr(pattern = numbers[i], x))
      if (y != -1){
        substr(string, j+y-1, j+y-1) <- as.character(i)
      }
    }
  }
  cali_doc_fixed <- append(cali_doc_fixed, string)
}

get_cali_vals(cali_doc_fixed) |> sum()

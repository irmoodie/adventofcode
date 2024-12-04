input <- paste(readLines("2024/input3.txt"), collapse = "")

# P1

mull_loc <- gregexpr("mul\\((\\d{1,3}),(\\d{1,3})\\)", input)

matches <- regmatches(input, mull_loc)

numbers <- lapply(matches[[1]], function(x) {
  as.numeric(unlist(regmatches(x, gregexpr("\\d{1,3}", x))))
})

product <- sapply(numbers, prod)

sum(product)

# P2

matches_do <- gregexpr("do\\(\\)", input)

matches_dont <- gregexpr("don't\\(\\)", input)

names(matches_do[[1]]) <- rep("do", times = length(matches_do[[1]]))
names(matches_dont[[1]]) <- rep("dont", times = length(matches_dont[[1]]))

instructions <- sort(c(matches_do[[1]], matches_dont[[1]]))

on_off <- rep(TRUE, times = nchar(input))

for (i in 1:(length(instructions)-1)){
  if (names(instructions[i])== "dont") {
    on_off[instructions[i]:instructions[i+1]] <- FALSE
  }
}

sum(product[on_off[mull_loc[[1]]]])

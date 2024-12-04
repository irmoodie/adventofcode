input <- readLines("2024/input3.txt")

# P1

mull_loc <- gregexpr("mul\\((\\d{1,3}),(\\d{1,3})\\)", input)

matches <- regmatches(input, mull_loc)

matches_parsed <-
  lapply(matches, function(x) {
    gsub(",", "*", gsub("[^0-9,]", "", x))
  })

evaluated <- lapply(matches_parsed, function(x) {
  sapply(x, function(y) eval(parse(text = y)))
})

sum(unlist(evaluated))

# P2 still not working

matches_do <- gregexpr("do\\(\\)", input)
matches_dont <- gregexpr("don't\\(\\)", input)

for (x in 1:length(matches_do)) {
  names(matches_do[[x]]) <- rep("do", times = length(matches_do[[x]]))
  names(matches_dont[[x]]) <- rep("dont", times = length(matches_dont[[x]]))
}

on_off <- lapply(input, function(x){rep(TRUE, times = nchar(x))})

new_evaluated <- list()

for (i in 1:length(on_off)) {
  instructions <- sort(c(matches_do[[i]], matches_dont[[i]]))
  for (j in 1:(length(instructions)-1)){
    if (names(instructions[j])== "dont") {
      on_off[[i]][instructions[j]:instructions[j+1]] <- FALSE
    }
  }
  new_evaluated[[i]] <- evaluated[[i]][on_off[[i]][mull_loc[[i]]]]
}

sum(unlist(new_evaluated))

data <- readLines("2023/input5.txt")

data <- split(data, cumsum(data==""), drop = TRUE)

seeds <- as.numeric(strsplit(data[[1]], " ")[[1]][-1])

data <-
  lapply(
    lapply(data[-1], function(j) strsplit(j[c(-1, -2)], " ")),
    function(i)
    lapply(i, as.numeric)
  )

get_loc <- function(s, dat){
  for (d in seq_along(dat)) {
    if (d == 1) {
      seed <- s
      } else {
       seed <- res
    }
    res <- NA
  for (n in dat[[d]]) {
    if (seed <= n[2]+n[3]-1 & seed >= n[2]){
      res <- n[1]+seed-n[2]
    }
  }
  if (is.na(res)) {res <- seed}
  }
  return(res)
}

# Part 1

locations <- vector()
for (seed in seeds) {
  locations <- append(locations, get_loc(s = seed, dat = data))
}

min(locations)

# Part 2

seed_ranges <- split(seeds, rep(1:(length(seeds)/2), each = 2))

sum(sapply(seed_ranges, function(i) i[2]))

for (x in seed_ranges) {
  for (y in seed_ranges)
  print(y[1] > x[1] & y[1] < x[1]+x[2]-1)
}

x <- seed_ranges[[1]]
y <- seed_ranges[[2]]

if (y[1] > x[1] & y[1] < x[1]+x[2]-1)


library(foreach)
library(doParallel)

registerDoParallel(detectCores())

# very slow brute force approach

seed_range_search <-
  foreach (s = seed_ranges) %dopar% {
    i <- 0
    res <- NULL
    while (i <= s[2]) {
      loc <- get_loc(s[1]+i, data)
      res <- min(res, loc)
      i <- i + 1
    }
    res
  }

min(unlist(seed_range_search))

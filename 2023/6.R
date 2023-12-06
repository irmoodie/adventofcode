dat <- lapply(readLines("2023/input6.txt"), function(i) as.numeric(unlist(strsplit(i, split = "\\s+"))[-1]))

ways_to_win <- function(t, d) {
  waysToWin <- rep(0, t = length(t))
  for (race in seq_along(t)){
    for (holdTime in 0:t[race]){
      if (holdTime*(t[race]-holdTime) > d[race]) {
        waysToWin[race] <- waysToWin[race] + 1
      }
    }
  }
  return(prod(waysToWin))
}

# Part 1

prod(ways_to_win(t = dat[[1]], d = dat[[2]]))

# Part 2

ways_to_win(
  t = as.numeric(paste(dat[[1]], collapse = "")),
  d = as.numeric(paste(dat[[2]], collapse = ""))
)

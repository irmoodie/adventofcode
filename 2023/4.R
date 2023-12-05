cards <- readLines("2023/input4.txt")

get_score <- function(card){
  n <- unlist(strsplit(unlist(strsplit(card, ":"))[2], "\\|"))
  w <- unlist(strsplit(n[1], "\\s"))
  w <- as.numeric(subset(w, grepl(pattern = "\\d", x = w)))
  y <- unlist(strsplit(n[2], "\\s"))
  y <- as.numeric(subset(y, grepl(pattern = "\\d", x = y)))
  return(floor(2^(sum(y %in% w)-1)))
}

sum(sapply(cards, get_score))

# Part 2

get_matches <- function(card){
  n <- unlist(strsplit(unlist(strsplit(card, ":"))[2], "\\|"))
  w <- unlist(strsplit(n[1], "\\s"))
  w <- as.numeric(subset(w, grepl(pattern = "\\d", x = w)))
  y <- unlist(strsplit(n[2], "\\s"))
  y <- as.numeric(subset(y, grepl(pattern = "\\d", x = y)))
  return(sum(y %in% w))
}

matches <- sapply(cards, get_matches, USE.NAMES = FALSE)

total <- length(matches)

reward <- list()
for (i in seq_along(matches)){
  if (matches[i] > 0) {
    reward[[i]] <- seq_along(matches)[(i+1):min(i+matches[i], length(matches))]
  } else {
     reward[[i]] <- vector(mode = "numeric")
  }
}

prev_reward <- unlist(reward)

while(length(prev_reward) > 0) {
  total <- sum(length(prev_reward), total)
  prev_reward <- unlist(sapply(prev_reward, function(i) reward[[i]]))
}

total


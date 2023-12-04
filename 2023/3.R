engine <- readLines("2023/input3.txt")

unique_chars <- unique(unlist(strsplit(paste0(engine, collapse = ""), "")))

not_symbols <- c(0:9, ".")

symbols <- subset(unique_chars, !(unique_chars %in% not_symbols))

sym_regex <- paste(paste0("\\", symbols), collapse = "|")

s <- strsplit(engine, "")

num <- lapply(X = s, FUN = grepl, pattern = "[[:digit:]]")
sym <- lapply(X = s, FUN = grepl, pattern = sym_regex)

locs <- list(
  c(0,1),
  c(0,-1),
  c(-1,0),
  c(-1,-1),
  c(-1,1),
  c(1,0),
  c(1,-1),
  c(1,1)
)

sym_adj <- list()

for (r in seq_along(s)) {
  sym_adj[[r]] <- rep(FALSE, times = length(s[[r]]))
  for (l in seq_along(s[[r]])) {
    if (num[[r]][l]) {
      for (loc in locs) {
        if (r + loc[1] > 0 & r + loc[1] <= length(s)) {
          if (l + loc[2] > 0 & l + loc[2] <= length(s[[r]])) {
            if (sym[[r + loc[1]]][l + loc[2]]) {
              sym_adj[[r]][l] <- TRUE
            }
          }
        }
      }
    }
  }
}

split_s <- lapply(seq_along(s), function(i) split(x = s[[i]], f = cumsum(c(0,diff(num[[i]])!=0))))

to_keep <-
  lapply(
    seq_along(s),
    function(i)
      unlist(lapply(split(sym_adj[[i]], cumsum(c(0,diff(num[[i]])!=0))), any))
  )

kept <-
  lapply(
    seq_along(s),
    function(i)
    split_s[[i]][to_keep[[i]]]
  )

kept_num <-
lapply(
  seq_along(s),
  function(j)
  sapply(
    kept[[j]],
    function(i)
    as.numeric(paste0(i, collapse = ""))
  )
)

sum(unlist(kept_num))

# part 2

pot_gears <- lapply(X = s, FUN = grepl, pattern = "\\*")

gears <- list()

gears_num <- list()

for (i in 1:length(s)) {
  gears_num[[i]] <- rep(NA, times = length(s[[1]]))
}

g <- 0
for (r in seq_along(s)) {
  gears[[r]] <- rep(FALSE, times = length(s[[r]]))
  for (l in seq_along(s[[r]])) {
    if (pot_gears[[r]][l]) {
      check <- 0
      g <- g + 1
      for (loc in locs) {
        if (r + loc[1] > 0 & r + loc[1] <= length(s)) {
          if (l + loc[2] > 0 & l + loc[2] <= length(s[[r]])) {
            if (num[[r + loc[1]]][l + loc[2]]) {
              gears_num[[r + loc[1]]][l + loc[2]] <- g
              check <- check + 1
            }
          }
        }
      }
      if (check >= 2) {
        gears[[r]][l] <- TRUE
      }
    }
  }
}

split_gears_num <- lapply(seq_along(s), function(i) split(x = gears_num[[i]], f = cumsum(c(0,diff(num[[i]])!=0))))

gear_ids <- unique(unlist(gears_num))[!is.na(unique(unlist(gears_num)))]

gear_ratios <- list()

for (gear_id in gear_ids) {
  gear_ratios[[gear_id]] <- vector()
  for (r in seq_along(split_gears_num)) {
    for (l in seq_along(split_gears_num[[r]])) {
      if (any(unlist(split_gears_num[[r]][l]) == gear_id, na.rm = TRUE)) {
        gear_ratios[[gear_id]] <-
          append(
            gear_ratios[[gear_id]],
            as.numeric(paste0(unlist(split_s[[r]][l]), collapse = ""))
          )
      }
    }
  }
}

gears_multipied <- vector()
for (g in gear_ratios){
  if (length(g) == 2){
    gears_multipied <- append(gears_multipied, g[1]*g[2])
  }
}

sum(gears_multipied)
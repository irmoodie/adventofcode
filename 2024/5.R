input <- readLines("2024/input5.txt")

rules <- input[grepl("\\|", input)]

updates <- input[grepl(",", input)]

rules_digits <- strsplit(rules, "\\|")

rules_1 <- sapply(rules_digits, function(x) as.numeric(x[1]))
rules_2 <- sapply(rules_digits, function(x) as.numeric(x[2]))

updates_digits <- strsplit(updates, ",")
updates_digits <- lapply(updates_digits, as.numeric)

# P1

is_correct <- logical()

for (u in seq_along(updates_digits)) {
  in_order <- 0
  for (i in seq_along(updates_digits[[u]])) {
    must_come_after <- rules_2[rules_1 == updates_digits[[u]][i]]
    comes_after <- updates_digits[[u]][-(1:i)]
    if (all(comes_after %in% must_come_after)) {
      in_order <- in_order + 1
    }
  }
  if (in_order == length(updates_digits[[u]])) {
    is_correct <- c(is_correct, TRUE)
  } else {
     is_correct <- c(is_correct, FALSE)
  }
}

sum(sapply(updates_digits[is_correct], function(x){x[ceiling(length(x)/2)]}))


# P2

incorrect_updates <- updates_digits[!is_correct]
corrected_updates <- list()

for (update in seq_along(incorrect_updates)) {
  old_update <- incorrect_updates[[update]]
  new_update <- vector(length = length(old_update))
  for (i in seq_along(old_update)) {
    must_come_before <- rules_1[rules_2 == old_update[i]]
    must_come_before <- must_come_before[must_come_before %in% old_update]
    new_update[(length(must_come_before)+1)] <- old_update[i]
  }
  corrected_updates[[update]] <- new_update
}

sum(sapply(corrected_updates, function(x){x[ceiling(length(x)/2)]}))

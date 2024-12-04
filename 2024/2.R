input <- readLines("2024/input2.txt")

# P1

reports <- sapply(input, function(x){as.numeric(strsplit(x, " ")[[1]])})

reports_diff <- sapply(reports, diff)

is_safe <- function(x){all(x <= -1 & x >= -3) | all(x >= 1 & x <= 3)}

n_safe <- sum(sapply(reports_diff, is_safe))

n_safe

# P2

sum(
  sapply(
    reports[!sapply(reports_diff, is_safe)],
    function(report){
        any(
          sapply(
            1:length(report),
            function(x){is_safe(diff(report[-x]))}
          )
        )
    }
  )
) + n_safe


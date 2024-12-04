input <- readLines("2024/input4.txt")

x <- matrix(unlist(strsplit(input, "")), nrow = length(input), byrow = TRUE)

x

find_adjacent_positions <- function(matrix, row, col) {
  adjacent_positions <- list()
  directions <- list(
    c(-1, 0),  # up
    c(1, 0),   # down
    c(0, -1),  # left
    c(0, 1),   # right
    c(-1, -1), # up-left
    c(-1, 1),  # up-right
    c(1, -1),  # down-left
    c(1, 1)    # down-right
  )
  
  for (direction in directions) {
    new_row <- row + direction[1]
    new_col <- col + direction[2]
    
    if (new_row > 0 && new_row <= nrow(matrix) && new_col > 0 && new_col <= ncol(matrix)) {
      adjacent_positions <- append(adjacent_positions, list(c(new_row, new_col)))
    }
  }
  
  return(adjacent_positions)
}

get_values_at_positions <- function(matrix, positions) {
  values <- sapply(positions, function(pos) {
    matrix[pos[1], pos[2]]
  })
  return(values)
}

# P1

xmas_count <- 0

for (i in 1:nrow(x)) {
  for (j in 1:ncol(x)) {
    if (x[i,j] == "X") {
      x_adj_pos <- find_adjacent_positions(x, i, j)
      x_adj_char <- get_values_at_positions(x, x_adj_pos)
      valid_pos <- x_adj_pos[x_adj_char == "M"]
      for (pos2 in valid_pos) {
        direction <- c(diff(c(i, pos2[1])), diff(c(j, pos2[2])))
        next_position <- pos2 + direction
        max_pos <- pos2 + direction + direction
        if (max_pos[1] > 0 && max_pos[1] <= nrow(x) && max_pos[2] > 0 && max_pos[2] <= ncol(x)) {
          if (x[next_position[1], next_position[2]] == "A") {
            if (x[(next_position[1] + direction[1]), (next_position[2] + direction[2])] == "S") {
              xmas_count <- xmas_count + 1 
            }
          }
        }
      }
    }
  }
}

xmas_count

# P2

x_mas_count <- 0

for (i in 2:(nrow(x)-1)) {
  for (j in 2:(ncol(x)-1)) {
    if (x[i,j] == "A") {

      ul <- c(i, j) + c(-1, -1) # up-left
      ur <- c(i, j) + c(-1, 1)  # up-right
      dl <- c(i, j) + c(1, -1)  # down-left
      dr <- c(i, j) + c(1, 1)   # down-right

      c1 <- paste(c(x[ul[1], ul[2]], x[i,j], x[dr[1], dr[2]]), collapse = "")
      c2 <- paste(c(x[ur[1], ur[2]], x[i,j], x[dl[1], dl[2]]), collapse = "")

      if ((c1 == "MAS" | c1 == "SAM") & (c2 == "MAS" | c2 == "SAM")) {
        x_mas_count <- x_mas_count + 1
      }
    }
  }
}

x_mas_count
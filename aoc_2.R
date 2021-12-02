solve <- function(x, part1 = TRUE) {
  instr <- x[, 1]
  vals <- x[, 2]

  vals[instr == "up"] <- -vals[instr == "up"]
  forward <- instr == "forward"

  if (part1) {
    depth <- sum(vals[!forward])
  } else {
    vals[!forward] <- cumsum(vals[!forward])
    prev <- which(forward) - 1
    depth <- sum(c(0, vals[prev]) * vals[forward])
  }

  sum(vals[forward]) * depth
}

x <- read.table("data/aoc_2")
# x <- data.frame(
#   c("forward", "down", "forward", "up", "down", "forward"),
#   c(5, 5, 8, 3, 8, 2)
# )

solve(x, part1 = TRUE)
solve(x, part1 = FALSE)


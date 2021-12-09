# Brute force
steps1 <- function(x, pos)
  abs(pos - x)
steps2 <- function(x, pos)
  sapply(abs(pos - x), \(y) sum(seq_len(y)))

solve <- function(x, part1 = TRUE)
  seq_len(max(x)) |>
  sapply(if (part1) steps1 else steps2, x) |>
  colSums() |>
  min()

pos <- scan("data/aoc_7", sep = ",")
c(part1 = solve(pos),
  part2 = solve(pos, part1 = FALSE))

#---- Alternative
solve_alt <- function(x, part1 = TRUE) {
  optimal <- if (part1) median else mean
  x <- abs(x - floor(optimal(x)))
  if (part1) sum(x)
  else sum(unlist(lapply(x, seq_len)))
}

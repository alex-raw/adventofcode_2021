count_increases <- function(x, n) {
  ln <- length(x) + 1
  x <- rowSums(matrix(x, ln, n)[-ln, ] |>
    suppressWarnings() |> as.matrix())
  sum(diff(x) > 0)
}

d <- as.integer(readLines("data/aoc_1"))
count_increases(d, 1)
count_increases(d, 3)

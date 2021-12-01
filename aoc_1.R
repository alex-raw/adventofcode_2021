count_increases <- function(x, n) {
  ln <- length(x)
  x <- matrix(x, ln + 1, n)[1:ln, ] |>
      as.matrix() |> rowSums() |> diff()
  sum(x > 0)
}

d <- as.integer(readLines("data/aoc_1"))
count_increases(d, 1)
count_increases(d, 3)


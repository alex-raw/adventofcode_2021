parse_string <- function(x) {
  x <- chartr("01", "FT", x) |> strsplit("")
  x <- do.call(rbind, x)
  mode(x) <- "logical"
  x
}

vec2dec <- function(x)
  as.integer(x) |>
  paste(collapse = "") |>
  strtoi(base = 2)

solve_1 <- function(x) {
  counts <- colSums(x)
  half <- nrow(x) / 2
  vec2dec(counts >= half) * vec2dec(counts < half)
}

find_hidden <- function(x, scrub = FALSE) {
  for (i in seq_len(ncol(x))) {
    ids <- x[, i]
    is_greater <- sum(ids) >= (nrow(x) / 2)
    flip <- xor(scrub, is_greater) # additional flip for <=
    x <- x[xor(flip, ids), ]       # flip if is greater
    if (!is.matrix(x)) break       # have vector when one is left
  }
  x
}

solve_2 <- function(x) {
  ogr <- find_hidden(x) |> vec2dec()
  co2 <- find_hidden(x, scrub = TRUE) |> vec2dec()
  ogr * co2
}

# x <- parse_string(c("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"))
x <- parse_string(readLines("data/aoc_3"))
solve_1(x)
solve_2(x)

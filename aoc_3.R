parse_string <- function(x) {
  x <- chartr("01", "FT", x) |> strsplit("")
  x <- do.call(rbind, x)
  mode(x) <- "logical"
  x
}

vec2dec <- function(x)
  as.integer(x) |>
  paste(collapse = "") |>
  strtoi(2)

solve_1 <- function(x) {
  counts <- colSums(x)
  half <- nrow(x) / 2
  vec2dec(counts >= half) * vec2dec(counts < half)
}

get_ogr <- function(x, scrub = FALSE) {
  for (i in seq_len(ncol(x))) {
    current <- x[, i]
    b <- sum(current) >= (nrow(x) / 2)
    if (scrub) b <- !b
    x <- if (b) x[current, ] else x[!current, ]
    if (!is.matrix(x)) break
  }
  x
}

solve_2 <- function(x) {
  ogr <- get_ogr(x) |> vec2dec()
  co2 <- get_ogr(x, scrub = TRUE) |> vec2dec()
  ogr * co2
}

x <- parse_string(c("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"))
x <- parse_string(readLines("data/aoc_3"))
solve_1(x)
solve_2(x)

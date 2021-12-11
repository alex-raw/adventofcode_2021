nb_ids <- function(x, nr) {
  v <- c(x - 1, x + 1)
  h <- c(x - nr, x + nr)
  d <- c(h - 1, h + 1)
  ans <- c(x, v, h, d)
  ans[ans > 0]
}

solve1 <- function(x, steps, part2 = FALSE) {
  x <- cbind(NA, rbind(NA, x, NA), NA)
  nr <- nrow(x)
  ln <- length(x)
  flashes <- integer(length = steps)

  for (i in seq_len(steps)) {
    nines <- which(x >= 9L)
    while (length(nines) > 0) {
      x[nines] <- NaN # flashes
      inds <- nb_ids(nines, nr)
      x <- x + tabulate(inds, ln)
      nines <- which(x >= 9L)
    }

    if (part2 & all(is.na(x)))
      return(i)

    flashes[i] <- sum(is.nan(x))
    x <- x + 1
    x[is.nan(x)] <- 0L
  }
  sum(flashes)
}

x <- as.matrix(read.fwf("data/aoc_11", rep(1, 10)))
c(part1 = solve1(x, 100),
  part2 = solve1(x, 10000, part2 = TRUE))

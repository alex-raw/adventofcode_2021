connected_components <- function(x) {
  x <- cbind(-1L, rbind(-1L, x, -1L), -1L) # pad to avoid out of bounds
  old_x <- label <- 0L
  repeat { # repeat because too dumb for part 2 of the algo
    old <- x
    for (j in seq_len(ncol(x)))            # raster
      for (i in seq_len(nrow(x)))          # scanning
        if (x[i, j] != -1L) {              # if it is a basin
          label <- label + 1L              # create new label
          nb <- c(                         # get neighboring elements
              x[i - 1L, j], x[i + 1L, j],
              x[i, j - 1L], x[i, j + 1L])
          nb <- nb[nb > 0L]
          x[i, j] <- if (length(nb) == 0L) # if no neighbor
            label else min(nb)             # assign new else smallest label
        } # step 2 (missing): remember and link label associations
    if (identical(x, old))
      return(x)
  }
}

find_minim <- function(m) # pad 9 to catch minima at the bound
  diff(sign(diff(c(9, m, 9))))

solve1 <- function(x) {
  rows <- t(apply(x, 1L, find_minim))
  cols <- apply(x, 2L, find_minim)
  sum(x[rows == 2L & cols == 2L] + 1L)
}

solve2 <- function(x) {
  y <- array(0L, dim(x))
  y[x == 9L] <- -1L  # mark basin margins
  tabulate(connected_components(y))[-1L] |> # take away counts of zeros
  sort() |> tail(3L) |> prod()
}

x <- as.matrix(read.fwf("data/aoc_9", rep(1, 100)))
c(part1 = solve1(x),
  part2 = solve2(x)) #|> system.time()

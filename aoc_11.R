which_neighbors <- function(x, nr) {
  v <- c(x - 1L, x + 1L)
  h <- c(x - nr, x + nr)
  d <- c(h - 1L, h + 1L)
  c(x, v, h, d)
}

solve1 <- function(x, steps = 0L) {
  x <- cbind(NA, rbind(NA, x, NA), NA)
  flashes <- integer(length = steps)
  i <- 0L

  while (!all(is.na(x))) { # NA is margin, NaN is flash
    x <- x + 1L
    x[is.nan(x)] <- 0L

    repeat {
      nines <- which(x >= 9L)
      if (length(nines) == 0L)
        break
      x[nines] <- NaN
      ids <- which_neighbors(nines, nrow(x))
      x <- x + tabulate(ids, length(x))
      # image(x, main = paste0("i = ", i + 1, "; flashes = ", sum(flashes))); Sys.sleep(.01)
    }

    i <- i + 1L
    if (steps) {
      flashes[i] <- sum(is.nan(x))
      if (i == steps)
        return(sum(flashes))
    }
  }
  return(i)
}

x <- as.matrix(read.fwf("data/aoc_11", rep(1, 10)))
c(part1 = solve1(x, 100L),
  part2 = solve1(x)) #|> system.time()


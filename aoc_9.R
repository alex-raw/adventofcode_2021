parse_grid <- function(path)
  as.matrix(read.fwf("data/aoc_9", rep(1, 100)))

find_minim <- function(m) # pad 9 to catch minima at the bound
  diff(sign(diff(c(9, m, 9))))

connected_components <- function(x) {
  label <- -1L
  old_x <- 0L
  while (!identical(x, old_x)) {
    old_x <- x
    for (j in seq_len(ncol(x))) {        # raster
      for (i in seq_len(nrow(x))) {      # scanning
        if (x[i, j]) {                   # if it is a basin
          neighbors <- c(                # get neighboring elements
            x[i - 1L, j], x[i + 1L, j],
            x[i, j - 1L], x[i, j + 1L])
          neighbors <- neighbors[neighbors < 0L]

          if (length(neighbors) == 0L) { # if no neighbor, uniquely label
            x[i, j] <- label
          } else {
            x[i, j] <- max(neighbors)    # assign smallest label
          }
          label <- label - 1L            # create label, + -> unmarked, - -> marked
        }
      }
    }
  }
  x
}

solve <- function(x, part1 = TRUE) {
  rows <- t(apply(x, 1L, find_minim))
  cols <- apply(x, 2L, find_minim)

  if (part1) return(
                    sum(x[rows == 2L & cols == 2L] + 1L)
  )

  rows <- rows + cols >= 0L
  rows[x == 9L] <- -2L                  # -2s are maxima
  x <- rows >= 0L
  x <- cbind(0L, rbind(0L, x, 0L), 0L)  # pad to avoid out of bounds

  ans <- connected_components(x)
  ans <- table(ans)
  ans[names(ans) != 0] |>
  sort() |> tail(3) |> prod()
}

x <- parse_grid("data/aoc_9")
c(part1 = solve(x),
  part2 = solve(x, part1 = FALSE)) |> system.time()


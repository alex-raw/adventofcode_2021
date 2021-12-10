parse_grid <- function(path) {
  d <- strsplit(readLines(path), "")
  # d <- strsplit(as.character(c(2199943210, 3987894921, 9856789892, 8767896789, 9899965678)), "")
  d <- do.call(rbind, d)
  mode(d) <- "integer"
  d
}

find_minim <- function(m) # pad 9 to catch minima at the bound
  diff(sign(diff(c(9, m, 9))))

links <- function(x) {
  # https://stackoverflow.com/questions/27520310/union-of-intersecting-vectors-in-a-list-in-r
  repeat {
    n = length(x)
    m = array(F, c(n, n))
    for (i in seq_len(n))
      for (j in seq_len(n))
        m[i, j] <- any(x[[i]] %in% x[[j]])

    ans <- apply(m, 2, \(y) Reduce(union, x[y]))
    if (!all(apply(m, 1, sum) == 1))
      x <- unique(ans)
    else
      return(ans)
  }
}

connected_components <- function(x) {
  label <- -1L
  equ <- list()
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
          equ <- c(equ, list(label, neighbors)) # remember equivalent labels
        }
        label <- label - 1L            # create label, + -> unmarked, - -> marked
      }
    }
  }
  equ <- links(unique(equ[lengths(equ) > 1]))
  for (i in seq_along(equ))
    x[x %in% equ[[i]]] <- i
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
solve(x)
solve(x, part1 = FALSE) #|> system.time()


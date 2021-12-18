pad <- \(x, pad = NA)
  x <- cbind(pad, rbind(pad, x, pad), pad)

which_neighbors <- function(x, nr)
  c(x - 1L, x + 1L, x - nr, x + nr)

smallest_neighbors <- function(x) {
  neighbors <- sapply(seq_along(x), which_neighbors, nrow(x))
  out_of_bounds <- neighbors <= 0 | neighbors >= length(x)
  neighbors[out_of_bounds] <- NA
  neighbors
}

min_cost <- function(x, distance, visited, all_nbs) {
  current <- 2 + nrow(x)
  while (any(!visited)) {
    visited[current] <- TRUE
    nbs_inds <- all_nbs[, current]
    nbs_inds <- nbs_inds[!visited[nbs_inds]]
    nbs_inds <- nbs_inds[!is.na(x[nbs_inds])]
    nbs_current <- x[nbs_inds]

    for (i in seq_along(nbs_current)) {
      smaller <- nbs_current[i] < distance[nbs_inds[i]]
      unvisited <- distance[nbs_inds[i]] == Inf
      if (unvisited && smaller)
        distance[nbs_inds[i]] <- nbs_current[i] + distance[current]
    }

    minim <- min(distance[!visited], na.rm = TRUE)
    next_node <- which(distance == minim & !visited)[1]
    current <- next_node
  }
  distance
}

solve1 <- function(x) {
  distance <- pad(array(Inf, dim(x)))
  distance[2, 2] <- 0L
  x <- pad(x)
  visited <- logical(length(x))
  visited[is.na(x)] <- TRUE
  all_nbs <- smallest_neighbors(x)
  distance <- min_cost(x, distance, visited, all_nbs)
  distance[-c(1, nrow(distance)), -c(1, ncol(distance))] |>
    rev() |> head(1)
}

solve2 <- function(x) {
  y <- lapply(1:4, \(n) x + n)
  x <- do.call(rbind, c(list(x), y))
  y <- lapply(1:4, \(n) x + n)
  x <- do.call(cbind, c(list(x), y))
  x <- x %% 9
  x[x == 0] <- 9
  solve1(x)
}

path <- "data/aoc_15_ex"
width <- nchar(readLines(path, n = 1))
x <- as.matrix(read.fwf(path, rep(1, width)))

c(part1 = solve1(x),
  part2 = solve2(x)) |> print() |> system.time()


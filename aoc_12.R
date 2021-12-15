get_next <- function(init, x)
  x[(x == init)[, 2:1]]

add_children <- function(nodes, x) {
  children <- lapply(nodes[, 1], get_next, x)
  nodes <- lapply(seq_len(nrow(nodes)), \(y) nodes[y, ])
  nodes <- rep(nodes, lengths(children))
  cbind(unlist(children), do.call(rbind, nodes))
}

duplicated_at <- function(x, id, n) {
  counts <- tabulate(x, max(id))[id]
  any(counts > n) | sum(counts > 1L) > 1L
}

find_paths <- function(x, reps, small_ids, term) {
  n <- 0L
  nodes <- as.matrix(term[1])
  repeat {
    nodes <- add_children(nodes, x)
    current <- nodes[, 1L]

    no_go <- apply(nodes, 1L, duplicated_at, small_ids, reps)
    nodes <- nodes[!(no_go | current %in% term), ]

    n <- n + sum(current == term[2])
    if (!length(nodes))
      return(n)
  }
}

solve1 <- function(path, reps) {
  x <- as.factor(scan(path, "", sep = "-"))
  start_id <- which(levels(x) == "start")
  end_id   <- which(levels(x) == "end")
  small_ids <- grep("[A-Z]|start|end", levels(x), invert = TRUE)

  x <- matrix(as.integer(x), ncol = 2, byrow = TRUE)
  find_paths(x, reps, small_ids, c(start_id, end_id))
}

c(part1 = solve1("data/aoc_12", 1L),
  part2 = solve1("data/aoc_12", 2L)) |> system.time()


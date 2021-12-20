parse_polymers <- function(path) {
  x <- readLines(path)
  x <- strsplit(gsub(" -> ", "", x), "")
  temp_len <- lengths(x)[1]
  x <- as.integer(as.factor(unlist(x)))
  polymer <- x[1:temp_len]
  rules <- matrix(x[-(1:temp_len)], ncol = 3, byrow = TRUE)
  rules <- xtabs(V3 ~ V1 + V2, data = as.data.frame(rules))
  list(polymer = polymer, rules = rules)
}

make_counter <- function(polymer, rules) {
  counter <- array(0L, dim(rules))
  inds <- cbind(polymer[-length(polymer)], polymer[-1])
  counter[inds] <- counter[inds] + 1L
  counter
}

find_next_inds <- function(rules) {
  i <- arrayInd(seq_along(rules), dim(rules))
  out <- rbind(cbind(i[, 1], rules[i]),
               cbind(rules[i], i[, 2]))
  out[, 1] + ncol(rules) * (out[, 2] - 1)
}

insert <- function(x, y, rules, ids1, ids2) {
  next_x <- array(0L, dim(rules))
  for (i in seq_along(ids1))
    next_x[ids1[i]] <- next_x[ids1[i]] + x[ids2[i]]
  list(next_x, y + next_x)
}

solve1 <- function(polymer, rules, steps) {
  counter <- make_counter(polymer, rules)
  mode(counter) <- "numeric"
  counter <- list(counter, counter)

  ids1 <- find_next_inds(rules)
  ids2 <- rep(seq_along(rules), 2)
  for (i in seq_len(steps - 1))
    counter <- insert(counter[[1]], counter[[2]], rules, ids1, ids2)

  i <- seq_len(ncol(counter[[2]]))
  ans <- sapply(i, \(x) sum(counter[[2]][x == rules]))
  diff(range(ans + tabulate(polymer)))
}

x <- parse_polymers("data/aoc_14") |> browser()

c(part1 = solve1(x$polymer, x$rules, 10),
  part2 = solve1(x$polymer, x$rules, 40)) |> print(digits = 15) |> system.time()


# old part1
count_insertions <- function(lhs, rhs, rules, steps) {
  depth <<- depth + 1L

  middle <- rules[lhs, rhs]
  counter[middle] <<- counter[middle] + 1L

  if (depth >= steps) {
    depth <<- depth - 1L
    return()
  }

  count_insertions(lhs, middle, rules, steps)
  count_insertions(middle, rhs, rules, steps)
  depth <<- depth - 1L
}

solve1 <- function(polymer, rules, steps) {
  for (i in seq(length(polymer) - 1L)) {
    lhs <- polymer[i]
    rhs <- polymer[i + 1L]
    depth <<- 0L
    sums <- numeric(ncol(rules))
    count_insertions(lhs, rhs, rules, steps = steps)
    sums <- counter + sums
  }
  sums
}


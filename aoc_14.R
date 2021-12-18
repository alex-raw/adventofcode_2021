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
  for (i in seq_along(polymer))
    counter[polymer[i], polymer[i + 1]] <- counter[polymer[i], polymer[i + 1]] + 1L
  counter
}

lol <- function(x, counter) {
  mode(x) <- "numeric"
  arg <- x > 0
  ids <- which(arg, arr.ind = TRUE)
  ids <- rbind(cbind(ids[, 1], rules[ids], which(arg)),
               cbind(rules[ids], ids[, 2], which(arg)))
  new_counter <- array(0L, dim(rules))
  for (i in seq(nrow(ids))) {
    ind <- ids[i, 1:2, drop = FALSE]
    new_counter[ind] <- new_counter[ind] + x[ids[i, 3]]
  }
  counter <- counter + new_counter
  list(new_counter, counter)
}

x <- parse_polymers("data/aoc_14")
counter <- make_counter(x$polymer, x$rules)

current <- list(counter, counter)
for (i in 1:39) {
  current <- lol(current[[1]], current[[2]])
}

ans <- sapply(seq_len(ncol(current[[2]])), \(x) sum(current[[2]][x == rules]))
ans <- ans + tabulate(polymer)
diff(range(ans)) |> print(digits = 15)

# old part1
# count_insertions <- function(lhs, rhs, rules, steps) {
#   depth <<- depth + 1L

#   middle <- rules[lhs, rhs]
#   counter[middle] <<- counter[middle] + 1L

#   if (depth >= steps) {
#     depth <<- depth - 1L
#     return()
#   }

#   count_insertions(lhs, middle, rules, steps)
#   count_insertions(middle, rhs, rules, steps)
#   depth <<- depth - 1L
# }

# solve1 <- function(polymer, rules, steps) {
#   for (i in seq(length(polymer) - 1L)) {
#     lhs <- polymer[i]
#     rhs <- polymer[i + 1L]
#     depth <<- 0L
#     sums <- numeric(ncol(rules))
#     count_insertions(lhs, rhs, rules, steps = steps)
#     sums <- counter + sums
#   }
#   sums
# }


parse_signals <- function(path) {
  x <- strsplit(readLines(path), " \\| | ")
  do.call(rbind, x) # Matrix: LHS == x[, 1:10] | RHS == x[, 11:14]
}

solve1 <- function(x, chars) {
  x <- nchar(x[, 11:14])
  counts <- nchar(chars)
  once <- counts[counts %in% which(tabulate(counts) == 1)]
  sum(x %in% once)
}

# Part2
bin_map <- function(x) {
  x <- sapply(strsplit(x, ""), \(y) letters[1:7] %in% y)
  rownames(x) <- letters[1:7]
  x <- addmargins(x)
  x[order(x[, "Sum"]), order(x["Sum", ])]
}

align_matrices <- function(x, ref) {
  for (i in 1:10) {
    swap <- which(ref[, i] != x[, i])
    sums <- x[swap, "Sum"]
    if (length(unique(sums)) == 1) { # are rowsums the same?
      x[swap, ] <- x[rev(swap), ]
      rownames(x)[swap] <- rownames(x)[rev(swap)]
    }
  }
  rownames(x)[order(rownames(ref))]
}

tr_codes <- function(x, chars) {
  new_chars <- align_matrices(bin_map(x), bin_map(chars))
  chartr(x = chars,
         paste(letters[1:7],   collapse = ""),
         paste(new_chars[1:7], collapse = ""))
}

sort_chars <- function(x) {
  dims <- dim(x)
  x <- sapply(strsplit(x, ""), \(x) paste(sort(x), collapse = ""))
  dim(x) <- dims
  as.data.frame(x) # for mapply later
}

solve2 <- function(x, chars) {
  wiring <- apply(x[, 1:10], 1, tr_codes, chars) |>
    sort_chars()
  signal <- t(x[, 11:14]) |>
    sort_chars()
  out <- mapply(match, signal, wiring) - 1 # 0-based indexing with digits
  colSums(out * 10^(3:0)) |>
    sum()
}

chars <- c(`0` = "abcefg", `1` = "cf", `2` = "acdeg", `3` = "acdfg", `4` = "bcdf", `5` = "abdfg", `6` = "abdefg", `7` = "acf", `8` = "abcdefg", `9` = "abcdfg")
x <- parse_signals("data/aoc_8_test")

c(part1 = solve1(x, chars),
  part2 = solve2(x, chars))


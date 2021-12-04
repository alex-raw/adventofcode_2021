parse_bingo <- function(path) list(
  cards = scan(path, skip = 1),
  nums = scan(path, sep = ",", nlines = 1)
)

check_bingo <- function(x, l)
  any(rowSums(x) == l) | any(colSums(x) == l)

play <- function(nums, cards, l) {
  dims <- c(l, l, length(cards) / l^2)
  checks <- array(FALSE, dims)

  for (num in nums) {
    checks[cards == num] <- TRUE
    bingo <- apply(checks, 3, check_bingo, l)
    if (any(bingo)) break
  }

  dim(cards) <- dims
  sum(cards[, , bingo][!checks[, , bingo]]) * num
}

with(parse_bingo("data/aoc_4"), play(nums, cards, 5))

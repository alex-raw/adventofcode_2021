parse_bingo <- function(path) list(
  cards = scan(path, skip = 1),
  nums = scan(path, sep = ",", nlines = 1)
)

check_bingo <- function(x, l)
  any(rowSums(x) == l) | any(colSums(x) == l)

play <- function(nums, cards, l, part1 = TRUE) {
  dims <- c(l, l, length(cards) / l^2)
  checks <- array(FALSE, dims)
  bingo <- logical()

  for (num in nums) {
    checks[cards == num] <- TRUE
    prev_bingo <- bingo # part2
    bingo <- apply(checks, 3, check_bingo, l)
    if (part1 & any(bingo)) break
    if (!part1 & all(bingo)) break
  }

  if (!part1) bingo <- !prev_bingo # part2
  dim(cards) <- dims
  sum(cards[, , bingo][!checks[, , bingo]]) * num
}

with(parse_bingo("data/aoc_4"), play(nums, cards, 5))
with(parse_bingo("data/aoc_4"), play(nums, cards, 5, part1 = FALSE))

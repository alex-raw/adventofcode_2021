check_bingo <- function(x, n)
  colSums(apply(x, 3, rowSums) == n) | colSums(apply(x, 3, colSums) == n)

play <- function(nums, cards, n, first = TRUE) {
  dim(cards) <- c(n, n, length(cards) / n^2)
  checks <- array(FALSE, dim(cards))
  bingo <- logical() # part 2

  for (num in nums) {
    checks[cards == num] <- TRUE
    prev_bingo <- bingo # part2
    bingo <- check_bingo(checks, n)
    if (first & any(bingo))
      break
    else if (all(bingo)) { # part2
      bingo <- !prev_bingo
      break
    }
  }
  num * sum(cards[, , bingo][!checks[, , bingo]])
}

solve <- function(path, first = TRUE) {
  cards <- scan("data/aoc_4", skip = 1)
  nums <- scan("data/aoc_4", nlines = 1, sep = ",")
  c(part1 = play(nums, cards, 5),
    part2 = play(nums, cards, 5, first = FALSE))
}

solve("data/aoc_4")

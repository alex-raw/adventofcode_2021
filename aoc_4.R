check_bingo <- function(x, n)
  colSums(n == marginSums(x, c(1, 3))) | colSums(n == marginSums(x, c(2, 3)))

play <- function(nums, cards, n, first = TRUE) {
  dim(cards) <- c(n, n, length(cards) / n^2)
  checks <- array(FALSE, dim(cards))
  bingo <- logical() # part 2

  for (num in nums) {
    checks[cards == num] <- TRUE
    prev_bingo <- bingo # part2
    bingo <- check_bingo(checks, n)
    if (first) {
      if (any(bingo))
        break
    } else
      if (all(bingo)) { # part2
        bingo <- !prev_bingo
        break
    }
  }
  num * sum(cards[, , bingo][!checks[, , bingo]])
}

solve <- function(path, first = TRUE) {
  cards <- scan(path, skip = 1, quiet = TRUE)
  nums <- scan(path, nlines = 1, sep = ",", quiet = TRUE)
  c(part1 = play(nums, cards, 5),
    part2 = play(nums, cards, 5, first = FALSE))
}

solve("data/aoc_4")

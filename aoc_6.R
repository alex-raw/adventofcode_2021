simulate_fish <- function(fish, days) {
  counter <- as.numeric(tabulate(fish, 9))
  for (i in 1:days) {
    n <- counter[1]
    counter <- c(counter[-1], counter[1])
    if (n) counter[7] <- counter[7] + n
  }
  sum(counter)
}

fish <- 1 + scan("data/aoc_6", sep = ",")
c(part1 = simulate_fish(fish, 80),
  part2 = simulate_fish(fish, 256)) |> print(digits = 18)

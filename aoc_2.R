get_prev <- function(x, f) {
  runs <- diff(c(which(!f), length(x) + 1)) # run lengths
  c(0, rep(x[!f], runs - 1))
}

solve <- function(instr, vals, part1 = TRUE) {
  vals[instr == "up"] <- -vals[instr == "up"]
  forward <- instr == "forward"

  if (part1)
    prod(tapply(vals, forward, sum))
  else { # zoo::na.locf
    vals[!forward] <- cumsum(vals[!forward])
    prev <- get_prev(vals, forward)
    horiz <- vals[forward]
    sum(prev * horiz) * sum(horiz)
  }
}

x <- read.table("data/aoc_2", col.names = c("instr", "vals"))
with(x, c(part1 = solve(instr, vals),
          part2 = solve(instr, vals, part1 = FALSE)
))

count_overlaps <- function(x1, y1, x2, y2) {
  x <- Map(seq, x1, x2)
  y <- Map(seq, y1, y2)
  coord <- do.call(rbind, Map(cbind, x, y))
  nrow(unique(coord[duplicated(coord), ]))
}

solve <- function(path, part1 = TRUE) {
  d <- gsub(",| -> ", " ", readLines(path))
  d <- read.table(text = d, col.names = c("x1", "y1", "x2", "y2"))
  if (part1) d <- subset(d, x1 == x2 | y1 == y2)
  with(d, count_overlaps(x1, y1, x2, y2))
}

c(part1 = solve("data/aoc_5"),
  part2 = solve("data/aoc_5", part1 = FALSE))

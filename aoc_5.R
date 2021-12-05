read_coord <- function(path) {
  x <- gsub(",| -> ", " ", readLines(path))
  read.table(text = x, col.names = c("x1", "y1", "x2", "y2"))
}

count_overlaps <- function(x1, y1, x2, y2) {
  x <- Map(seq, x1, x2)
  y <- Map(seq, y1, y2)
  coord <- unlist(Map(\(a, b) complex(real = a, imaginary = b), x, y))
  coord[duplicated(coord)] |> unique() |> length()
}

solve <- function(path, part1 = TRUE) {
  d <- read_coord(path)
  if (part1) {
    straight <- with(d, x1 == x2 | y1 == y2)
    d <- d[straight, ]
  }
  with(d, count_overlaps(x1, y1, x2, y2))
}

c(solve("data/aoc_5"),
  solve("data/aoc_5", part1 = FALSE))

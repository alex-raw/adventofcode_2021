parse_manual <- function(path) {
  x <- readLines(path)
  ind <- which(!nzchar(x))
  coord <- 1L + scan(text = x[1:(ind - 1)], sep = ",")
  coord <- matrix(coord, ncol = 2, byrow = TRUE)
  instr <- x[seq(ind + 1, length(x))]
  instr <- gsub("fold along ", "", instr)
  instr <- read.table(text = instr, sep = "=",
                      col.names = c("xy", "val"))
  instr$val <- instr$val + 1L
  instr$xy <- instr$xy == "x"

  list(coord = coord, instr = instr)
}

create_sheet <- function(x, y) {
  sheet <- matrix(0L, max(y), max(x))
  for (i in seq_len(length(y)))
    sheet[y[i], x[i]] <- 1L
  sheet
}

flip <- function(a, b, dif) {
  inds <- seq(dif + 1, length(a))
  a[inds] <- a[inds] + rev(b)
  a
}

flip_vec <- function(x, val) {
  bottom <- x[seq(val + 1, length(x))]
  top <- x[1:(val - 1)]
  dif <- length(top) - length(bottom)
  if (dif >= 0) {
    flip(top, bottom, dif)
  } else {
    flip(rev(bottom), rev(top), -dif)
  }
}

fold <- function(sheet, xy, val) {
  sheet <- if (xy) {
    t(apply(sheet, 1, flip_vec, val))
  } else {
    apply(sheet, 2, flip_vec, val)
  }
  sheet
}

solve1 <- function(path, part1 = TRUE) {
  x <- parse_manual(path)
  instr <- x$instr
  coord <- x$coord

  sheet <- create_sheet(coord[, 1], coord[, 2])

  if (part1) {
    sheet <- fold(sheet, instr[1, 1], instr[1, 2])
    return(sum(sheet > 0))
  }

  for (i in seq_len(nrow(instr))) {
    sheet <- fold(sheet, instr[i, 1], instr[i, 2])
  }
  image(sheet > 0)
}

c(part1 = solve1("data/aoc_13"),
  part2 = solve1("data/aoc_13", part1 = FALSE))

# CJCKBAPB

hex_to_bin <- function(x) {
  hex_bin <- unlist(strsplit(readLines("data/aoc_16_hex"), " = "))
  ids <- factor(x, levels = c(0:9, LETTERS[1:6]))
  hex_bin[c(FALSE, TRUE)][ids] |>
    strsplit("") |> unlist() |> as.integer()
}

bin_to_dec <- function(x) {
  x <- as.numeric(x)
  sum(x * 2^rev((seq_along(x) - 1)))
}

make_labels <- \(x, n) rep_len(rep.int(seq_len(n), rep.int(5, n)), length(x) - 6)

header <- c("V", "V", "V", "T", "T", "T")
labs <- do.call(paste0, expand.grid(letters, letters))

find_last <- function(x) {
  inds <- seq.int(7, length(x))
  inds <- inds[seq_along(inds) %% 5 == 1]
  4 + inds[which(x[inds] == 0)[1]]
}

label_packet <- function(x) {
  last <- find_last(x)
  temp <- x[1:last]
  rest <- x[-(1:last)]

  ngroups <- (length(temp) - 6) / 5
  c(header, labs[make_labels(temp, ngroups)], label(rest))
}

label_operator <- function(x) {
  n <- if (x[7]) 11 else 15
  sub_labels <- label(x[-seq_len(7 + n)])

  c(header, "I", rep_len("L", n), sub_labels)
}

label <- function(x) {
  if (length(x) == 0 || all(x == 0))
    return()
  type_id <- bin_to_dec(x[4:6])
  if (type_id == 4) {
    label_packet(x)
  } else {
    label_operator(x)
  }
}

solve1 <- function(x) {
  versions <- x[label(x) == "V"]
  make_labels(x, 3)
  split(versions, (seq_along(versions) - 1) %/% 3) |>
    sapply(bin_to_dec) |> sum()
}

levels_at <- function(x, at) {
  onset <- which(diff(at) == 1)
  out <- diff(c(1, onset + 1, length(x) + 1))
  out <- rep(seq_along(out), out)
}

parse_packet <- function(x) {
  is_literal <- !x$label %in% c("V", "T", "I", "L")

  c(version = bin_to_dec(x$val[1:3]),
    type_id = bin_to_dec(x$val[4:6]),
    value = bin_to_dec(x$val[is_literal]))
}

parse_packets <- function(x) {
  labelled <- label(x)
  trailing <- rep(NA, diff(lengths(list(labelled, x))))
  d <- cbind.data.frame(x, c(labelled, trailing))
  names(d) <- c("val", "label")
  packets <- c(levels_at(labelled, labelled == "V"), trailing)
  parsed <- split(d, packets) |> lapply(parse_packet)
  as.data.frame(do.call(rbind, parsed))
}

solve2 <- function(x) {
  funs <- c(sum, prod, min, max, c, `>`, `<`, `==`)
  x <- parse_packets(x)
  top_level <- levels_at(x$value, x$value == 0)
  return(x)
}

x <-
readLines("data/aoc_16") |>
# "C0015000016115A2E0802F182340" |>
# "9C0141080250320F1802104A08" |>
  strsplit("") |> unlist() |> hex_to_bin()

# c(part1 = solve1(x),
#   part2 = solve2(x)) |> print() |> system.time()

x <- solve2(x)

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

header <- c("V", "V", "V", "T", "T", "T")
funs <- c(sum, prod, min, max, c, `>`, `<`, `==`)

label_ids <- \(x, n)
  rep_len(rep.int(seq_len(n), rep.int(5, n)), length(x) - 6)

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

  c(header, letters[label_ids(temp, ngroups)], label(rest))
}

label_operator <- function(x, type) {
  n <- if (x[7]) 11 else 15
  sub_labels <- label(x[-seq_len(7 + n)])

  c(header, "I", rep_len("L", n), sub_labels)
}

execute_operator <- function(x, type) {
  n <- if (x[7]) 11 else 15
  label(x[-(1:(6 + n))])
} # TODO: fails with diadic functions

label <- function(x) {
  if (length(x) == 0 || all(x == 0))
    return()
  type_id <- bin_to_dec(x[4:6])
  fun <- funs[[type_id + 1]]
  if (type_id == 4) {
    # label_packet(x)
    fun(bin_to_dec(x[-(1:6)]))
  } else {
    # label_operator(x, type_id)
    fun(execute_operator(x))
  }
}

solve1 <- function(x) {
  return(label(x))
  versions <- x[label(x) == "V"]
  split(versions, (seq_along(versions) - 1) %/% 3) |>
    sapply(bin_to_dec) |> sum()
}

x <-
# readLines("data/aoc_16") |>
"C200B40A82" |>
# "9C0141080250320F1802104A08" |>
  strsplit("") |> unlist() |> hex_to_bin()

# c(part1 = solve1(x),
#   part2 = solve2(x)) |> print() |> system.time()
solve1(x)


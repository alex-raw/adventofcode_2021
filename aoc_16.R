hex_to_bin <- function(x) {
  hex_bin <- unlist(strsplit(readLines("data/aoc_16_hex"), " = "))
  ids <- factor(x, levels = c(0:9, LETTERS[1:6]))
  hex_bin[c(FALSE, TRUE)][ids] |>
    strsplit("") |> unlist() |> as.integer()
}

bin_to_dec <- function(x) { # strtoi silently overflows
  x <- as.numeric(x)
  sum(x * 2^seq(length(x) - 1, 0))
}

funs <- c(sum, prod, min, max, c, `>`, `<`, `==`)
funs <- c("sum", "prod", "min", "max", "c", ">", "<", "==")

label_packet <- function(x) {
  firsts <- seq(7, length(x), by = 5)
  last <- 4 + firsts[which(x[firsts] == 0)[1]]
  literals <- x[7:last]
  rest <- x[-(1:last)]
  ln <- length(literals)
  # c(rep(F, ln), label(rest))

  # get literal values
  vals <- tapply(literals, gl(ln / 5, 5), bin_to_dec) |> unname()
  list(vals, label(rest))
}

label_operator <- function(x, type) {
  n <- if (x[7]) 11 else 15
  rest <- x[-seq_len(7 + n)]
  # c(rep(F, n + 1), label(rest)) # + 1 for I
  label(rest)
}

header <- c(T, T, T, F, F, F) # Version number is TRUE
label <- function(x) {
  if (length(x) == 0 || all(x == 0))
    return()
  type_id <- bin_to_dec(x[4:6])
  fun <- funs[[type_id + 1]]
  if (type_id == 4) {
    # c(header, label_packet(x))
    unlist(label_packet(x), recursive = FALSE)
  } else {
    # c(header, label_operator(x, type_id))
    if (fun %in% c("==", "<", ">")) {
      if (!x[7]) {
        n <- 7 + 15 + bin_to_dec(x[seq(8, length = 15)])
        list(fun, label(x[22:n]), label(x[-(1:n)]))
      } else
        list(fun, label_operator(x, type_id))
    } else
      list(fun, label_operator(x, type_id))
  }
}

solve1 <- function(x) {
  versions <- x[is_version(x)]
  ln <- length(versions)
  tapply(versions, gl(ln / 3, 3), bin_to_dec) |> sum()
}

x <-
# readLines("data/aoc_16") |>
# "C200B40A82" |>
"9C0141080250320F1802104A08" |>
  strsplit("") |> unlist() |> hex_to_bin()

# c(part1 = solve1(x),
#   part2 = solve2(x)) |> print() |> system.time()
# solve1(x) #|> system.time()
lol = label(x)

str(lol)


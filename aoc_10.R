rgsub <- function(pattern, replacement, x) {
  ans <- gsub(pattern, replacement, x, perl = TRUE)
  if (identical(x, ans))
    return(ans)
  else
    rgsub(pattern, replacement, ans)
}

replace_with_matching <- function(x) {
  x <- gsub(")|}|>|]", "", x)
  strsplit(chartr("{[(<", "}])>", x), "")
}

get_score <- function(x) {
  score <- 0L
  for (i in seq_along(x))
    score <- (score * 5L) + x[i]
  score
}

solve <- function(x, part1 = TRUE) {
  x <- rgsub("\\(\\)|\\[\\]|<>|{}", "", x) # recursively remove legal
  illegal <- gsub("[\\(\\[{<]", "", x)     # remove remaining open
  illegal <- substring(illegal, 1, 1)      # get first illegal
  corrupted <- nzchar(illegal)             # empty string position means corrupted

  points <- c(`)`= 3, `]` = 57, `}` = 1197, `>` = 25137)
  if (part1)
    return(sum(points[illegal[corrupted]]))

  points[1:4] <- 1:4
  replace_with_matching(x[!corrupted]) |>
    sapply(\(x) get_score(points[rev(x)])) |>
    sort() |> median()
}

x <- readLines("data/aoc_10")
c(part1 = solve(x),
  part2 = solve(x, part1 = FALSE))


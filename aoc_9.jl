using Images

x = split.(readlines("data/aoc_9"), "")
x = parse.(Int, hcat(x...))
sum([x[i] + 1 for i in findlocalminima(x)])
# lol

function local_extr(x)
    [9, x..., 9] |> diff .|> sign |> diff
end

cols = hcat(local_extr.(eachcol(x))...)
rows = hcat(local_extr.(eachrow(x))...)

@. inds = rows' == 2 & cols == 2
sum(x[inds])

function read_bool(path)
    x = split.(readlines(path), "")
    parse.(Bool, reduce(hcat, x))
end

bool2dec(x) = parse(Int, join(convert.(Int, x)), base = 2)

more_ones(x, n) = count(x) >= (n / 2)

find_rate(x) = [more_ones(row, size(x, 2)) for row in eachrow(x)]

function find_hidden(x)
    for i in 1:size(x, 1)
        row = x[i, :]
        flip = !more_ones(row, length(row))
        x = x[:, xor.(flip, row)]
        i += 1
        size(x, 2) == 1 && return x
    end
end

function solve(x; part2 = false)
    f = part2 ? find_hidden : find_rate
    bool2dec(f(x)) * bool2dec(f(.!x))
end

data = read_bool("data/aoc_3")
data = read_bool("data/aoc_3_test")
solve(data)
solve(data, part2 = true)

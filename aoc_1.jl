function count_increases(x)
    sum(diff(x) .> 0)
end

function sum_window(x, n)
    n == 1 && return count_increases(x)

    ln = length(x)
    out = zeros(Int, ln)
    for i in 1:ln - n + 1
        out[i] = sum(x[i:(i + n - 1)])
    end
    return count_increases(out)
end

data = parse.(Int, readlines("data/aoc_1"))
sum_window(data, 1)
sum_window(data, 3)

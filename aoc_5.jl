function parse_coordinates(x)
    x1, y1, x2, y2 = parse.(Int, split(x, r",| -> "))
    xstep = x1 > x2 ? -1 : 1
    ystep = y1 > y2 ? -1 : 1
    CartesianIndex.(x1:xstep:x2, y1:ystep:y2)
end

# function solve(path)
#     d = [parse_coordinates(lines) for lines in readlines(path)]
#     d = [(d...)...]
#     sum([count(==(i), d) for i in unique(d)] .> 1) # crazy slow
# end

# println(solve("data/aoc_5"))

function solve_faster(path)
    coord = parse_coordinates.(readlines(path))
    x = zeros(Int, 1000, 1000)
    for i = coord
        x[i] .+= 1
    end
    sum(x .> 1)
end

println(solve_faster("data/aoc_5"))


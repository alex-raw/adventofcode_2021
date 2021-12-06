function parse_coordinates(x)
    x1, y1, x2, y2 = parse.(Int, split(x, r",| -> "))
    xstep = x1 > x2 ? -1 : 1
    ystep = y1 > y2 ? -1 : 1
    CartesianIndex.(x1:xstep:x2, y1:ystep:y2)
end

function solve(path)
    d = [parse_coordinates(lines) for lines in readlines(path)]
    d = [(d...)...]
    sum([count(==(i), d) for i in unique(d)] .> 1) # crazy slow
end

lol = solve("data/aoc_5")

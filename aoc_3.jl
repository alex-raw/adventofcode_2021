using DelimitedFiles
readdlm("data/aoc_3", "")

data = parse.(Int, readlines("data/aoc_3"); base = 2)

open("data/aoc_3") do file
    for line in eachline(file)
        println(line)
    end
end


lol = [parse.(Bool, split(row, "")) for row in readlines("data/aoc_3")]

       hcat(lol)

chomp

data = readlines("data/aoc_3")
data = map(x -> parse.(Bool, split(x, "")), data)
data = reduce(hcat, data)
data'

first(data, 1)

data[1][1]
split.(data, "")
extract

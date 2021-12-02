using DelimitedFiles

function solve(x; part2 = true)
    horiz, depth, aim = 0, 0, 0
    for i in 1:size(x, 1)
        instr, val = x[i, :]
        if instr == "forward"
            horiz += val
            depth += val * aim
        else
            instr == "down" ? aim += val : aim -= val
        end
    end
    horiz * (part2 ? depth : aim)
end

data = readdlm("data/aoc_2")
println(solve(data, part2 = true))
println(solve(data, part2 = false))

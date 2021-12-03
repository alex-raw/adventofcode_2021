using DelimitedFiles
data = readdlm("data/aoc_2")

function solve(x; part2 = true)
    horiz, depth, aim = 0, 0, 0
    for (instr, val) in eachrow(x)
        if instr == "forward"
            horiz += val
            depth += val * aim
        else
            aim += instr == "down" ? val : -val
        end
    end
    horiz * (part2 ? depth : aim)
end

println(solve(data, part2 = true))
println(solve(data, part2 = false))

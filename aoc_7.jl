using Statistics

function solve(pos; part1 = true)
    optim = floor ∘ (part1 ? median : mean)
    Δ = abs.(pos .- optim(pos))
    !part1 && (Δ = [sum(1:n) for n in Δ])
    sum(Δ)
end

pos = parse.(Int, split(readline("data/aoc_7"), ","))
solve(pos)
solve(pos, part1 = false)

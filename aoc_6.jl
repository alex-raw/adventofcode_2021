function simulate_fish(fish, days)
    counter = [count(==(n), fish) for n in 1:9]
    for _ in 1:days
        n = counter[1]
        counter = circshift(counter, -1)
        counter[7] += n
    end
    sum(counter)
end

fish = 1 .+ parse.(Int, split(readline("data/aoc_6"), ","))
println(simulate_fish(fish, 80), "\n", simulate_fish(fish, 256))

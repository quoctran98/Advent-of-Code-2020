input <- sort(as.numeric(readLines("day10/day10_input.txt")))
joltDiff <- append(input, input[length(input)] + 3) - append(input, 0, 0)
print(sum(joltDiff == 1) * sum(joltDiff == 3))
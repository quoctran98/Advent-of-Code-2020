input <- read.table("day01/day01_input.txt", sep = "\n")$V1

for (i in 1:(length(input) - 1)) {
  for (j in (i + 1):length(input)) {
    if ((input[i] + input[j]) == 2020) {
      print(input[i] * input[j])
    }
  }
}

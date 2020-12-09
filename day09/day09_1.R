input <- as.numeric(read.table("day09/day09_input.txt", sep = "\n")$V1)

isValid <- function (input = input, index, numBefore = 25) {
  searchRange <- (index-numBefore):(index-1)
  allCombos <- expand.grid(input[searchRange], input[searchRange])
  allCombos[,3] <- allCombos[,1] + allCombos[,2]
  return(input[index] %in% allCombos[,3])
}

for (i in 26:length(input)) {
  if (!isValid(input, i)) {
    print(input[i])
    break
  }
}
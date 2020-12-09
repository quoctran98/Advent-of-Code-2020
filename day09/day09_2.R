input <- as.numeric(read.table("day09/day09_input.txt", sep = "\n")$V1)

for (startIndex in 1:length(input)) {
  
  currRange <- 1
  currSum <- 0
  
  while(currSum < 36845998) {
    currSum <- sum(input[startIndex:(startIndex+currRange)])
    currRange <- currRange + 1
  }
  
  if (!(currSum - 36845998)) {
    print(input[startIndex] + input[startIndex + currRange - 1])
    break
  }
}

# so much debugging :( but we did it
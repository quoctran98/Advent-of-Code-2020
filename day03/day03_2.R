library(stringr)

input <- as.character(read.table("day03/day03_input.txt", sep = "\n", comment.char = "")$V1)

fieldWidth <- str_length(input[1])
fieldHeight <- length(input)


howManyTrees <- function (slopeRight, slopeDown) {
  xMove <- slopeRight
  yMove <- slopeDown
  
  xPos <- 1
  yPos <- 1

  treesEncountered <- 0
  while (yPos < fieldHeight) {
    xPos <- (xPos +  xMove) %% fieldWidth
    if (xPos == 0) {xPos <- fieldWidth}
    yPos <- yPos + yMove
    if (str_sub(input[yPos], xPos, xPos) == "#") {
      treesEncountered <- treesEncountered + 1
    }
  }
  return(treesEncountered)
}

print(howManyTrees(1,1) * howManyTrees(3,1) * howManyTrees(5,1) * howManyTrees(7,1) * howManyTrees(1,2))
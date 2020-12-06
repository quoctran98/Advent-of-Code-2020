library(stringr)

input <- as.character(read.table("day03/day03_input.txt", sep = "\n", comment.char = "")$V1)

fieldWidth <- str_length(input[1])
fieldHeight <- length(input)

xMove <- 3
yMove <- 1

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

print(treesEncountered)
library(stringr)

input <- as.character(read.table("day02/day02_input.txt", sep = "\n")$V1)

validPassword <- function (password) {
  condition <- unlist(str_split(password, ":"))[1]
  pos1 <- as.numeric(unlist(str_split(condition, "-"))[1])
  pos2 <- as.numeric(unlist(str_split(unlist(str_split(condition, "-"))[2], " "))[1])
  reqLetter <- unlist(str_split(condition, " "))[2]
  
  phrase <- unlist(str_split(password, ":"))[2]
  phrase <- str_sub(phrase, start = 2)
  
  validPositions <- 0
  if (str_sub(phrase, pos1, pos1) == reqLetter) {
    validPositions <- validPositions + 1
  }
  if (str_sub(phrase, pos2, pos2) == reqLetter) {
    validPositions <- validPositions + 1
  }

  return(validPositions == 1)
}

numValid <- 0
for (password in input) {
  numValid <- numValid + validPassword(password)
}
print(numValid)

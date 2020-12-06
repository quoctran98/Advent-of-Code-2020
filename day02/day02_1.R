library(stringr)

input <- as.character(read.table("day02/day02_input.txt", sep = "\n")$V1)

validPassword <- function (password) {
  condition <- unlist(str_split(password, ":"))[1]
  minLetter <- as.numeric(unlist(str_split(condition, "-"))[1])
  maxLetter <- as.numeric(unlist(str_split(unlist(str_split(condition, "-"))[2], " "))[1])
  reqLetter <- unlist(str_split(condition, " "))[2]
  
  phrase <- unlist(str_split(password, ":"))[2]
  phrase <- str_sub(phrase, start = 2)
 
  condNum <- str_count(phrase, reqLetter)
  
  if (condNum >= minLetter & condNum <= maxLetter) {
    return(T)
  } else {
    return(F)
  }
}

numValid <- 0
for (password in input) {
  numValid <- numValid + validPassword(password)
}
print(numValid)

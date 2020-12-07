library(readtext)
library(stringr)

input <- readtext("day04/day04_input.txt")$text
input <- str_split(input, "\n\n")[[1]]

validPassport <- function (passport) {
  hasCID <- str_detect(passport, "cid")
  nFields <- str_count(passport, ":")
  
  
  
  if (nFields == 8) {
    return(T)
  } else if (!hasCID & nFields == 7) {
    return(T)
  } else {
    return(F)
  }
}

nValid <- 0
for (passport in input) {
  if (validPassport(passport)) {
    nValid <- nValid + 1
  }
}



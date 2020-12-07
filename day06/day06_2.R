library(readtext)
library(stringr)

input <- readtext("day06/day06_input.txt")$text
input <- str_split(input, "\n\n")[[1]]

handleGroup <- function (answers) {
  answers <- str_split(answers, "\n")[[1]]
  nPeople <- length(answers)
  
  answers <- paste(answers, collapse = "")

  allYes <- 0
  for (let in letters) {
    if (str_count(answers, let) == nPeople) {
      allYes <- allYes + 1
    }
  }
  return(allYes)
}

totalSum <- 0
for (group in input) {
  totalSum <- totalSum + handleGroup(group)
} 
print(totalSum)
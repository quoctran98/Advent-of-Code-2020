library(readtext)
library(stringr)

input <- readtext("day06/day06_input.txt")$text
input <- str_split(input, "\n\n")[[1]]

handleGroup <- function (answers) {
  answers <- str_split(answers, "\n")[[1]]
  answers <- paste(answers, collapse = "")
  answers <- str_split(answers, "")[[1]]
  return(length(unique(answers))) # assuming no weird non letters or w/e
}

totalSum <- 0
for (group in input) {
  totalSum <- totalSum + handleGroup(group)
} 
print(totalSum)
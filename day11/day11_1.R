library(stringr)
input <- readLines("day11/day11_input.txt")

inputCol <- str_length(input[1])
inputRow <- length(input)

input <- unlist(str_split(input, ""))
input <- matrix(data = input, nrow = inputRow, ncol = inputCol, byrow = T)



updateSeats <- function (currSeats) {
  tempNew <- currSeats # DO NOT MODIFY CURRSEATS
  for (rowN in 1:nrow(currSeats)) {
    for (colN in 1:ncol(currSeats)) {

      seat <- currSeats[rowN, colN]
      
      nOcc <- 0
      adjR <- (rowN-1):(rowN+1)
      adjR <- adjR[adjR > 0 & adjR <= inputRow]
      adjC <- (colN-1):(colN+1)
      adjC <- adjC[adjC > 0 & adjC <= inputCol]
      for (r in adjR) {
        for (c in adjC) {
          if (r == rowN & c == colN) {
            # idk do nothing
          } else if (currSeats[r,c] == "#") {
            nOcc <- nOcc + 1
          }
        }
      }

      if (seat == "#" & nOcc >= 4) {
        tempNew[rowN, colN] <- "L"
      } else if (seat == "L" & nOcc == 0) {
        tempNew[rowN, colN] <- "#"
      }
    }
  }
  return(tempNew)
}

prevSeats <- input
while (T) {
  newSeats <<- updateSeats(prevSeats)
  if (!(F %in% (prevSeats == newSeats))) {
    print(sum(match(newSeats, "#"), na.rm = T))
    break
  }
  prevSeats <<- newSeats
}


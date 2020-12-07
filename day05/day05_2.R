library(stringr)

input <- as.character(read.table("day05/day05_input.txt", sep = "\n")$V1)

parseSeat <- function(seat) {
  minRow <- 0
  maxRow <- 127
  for (i in 1:7) {
    if (str_sub(seat, i, i) == "F") {
      newRange <- floor((maxRow - minRow)/2)
      maxRow <- minRow + newRange
    } else {
      newRange <- floor((maxRow - minRow)/2)
      minRow <- maxRow - newRange
    }
  }
  
  minSeat <- 0
  maxSeat <- 7
  for (i in 8:10) {
    if (str_sub(seat, i, i) == "L") {
      newRange <- floor((maxSeat - minSeat)/2)
      maxSeat <- minSeat + newRange
    } else {
      newRange <- floor((maxSeat - minSeat)/2)
      minSeat <- maxSeat - newRange
    }
  }
  
  return((minRow * 8) + minSeat)
}

allSeats <- c()
for (seat in input) {
  allSeats <- append(allSeats, parseSeat(seat))
}

allSeats <- sort(allSeats)
possibleSeats <- allSeats[1]:allSeats[length(allSeats)]
possibleSeats[!(possibleSeats %in% allSeats)]


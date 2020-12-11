library(stringr)
input <- readLines("day11/day11_input.txt")

inputCol <- str_length(input[1])
inputRow <- length(input)

input <- unlist(str_split(input, ""))
input <- matrix(data = input, nrow = inputRow, ncol = inputCol, byrow = T)

whatsTheSeat <- function (allSeats, thisSeat, dirLook) {
  if (sum(dirLook <= 0) >= 1) {
    return(0)
  } else if (dirLook[1] > nrow(allSeats)) {
    return(0)
  } else if (dirLook[2] > ncol(allSeats)) {
    return(0)
  } else if (sum(thisSeat == dirLook) == 2) {
    return(0)
  }

  if (allSeats[dirLook[1],dirLook[2]] == "#") {
    return(1)
  } else if (allSeats[dirLook[1],dirLook[2]] == "L") {
    return(0)
  } else if (allSeats[dirLook[1],dirLook[2]] == ".") {
    deltaLook <- dirLook - thisSeat
    deltaLook <- deltaLook / abs(deltaLook)
    deltaLook[is.nan(deltaLook)] <- 0 # WHAT AN ABSURD WORKAROUND
    return(whatsTheSeat(allSeats, thisSeat, dirLook = dirLook + deltaLook))
  }
  print("uh oh")
}

updateSeats <- function (currSeats) {
  tempNew <- currSeats # DO NOT MODIFY CURRSEATS
  for (rowN in 1:nrow(currSeats)) {
    for (colN in 1:ncol(currSeats)) {
      seat <- currSeats[rowN, colN]
      nOcc <- 0
      adjR <- (rowN-1):(rowN+1)
      adjC <- (colN-1):(colN+1)
      for (r in adjR) {
        for (c in adjC) {
          nOcc <- nOcc + whatsTheSeat(currSeats, c(rowN,colN), c(r,c))
        }
      }
      
      if (seat == "#" & nOcc >= 5) {
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

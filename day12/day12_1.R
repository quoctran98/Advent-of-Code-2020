library(stringr)
input <- readLines("day12/day12_input.txt")

# [x(+E-W), y(+N-S), dir(0E,90N,180W,270S)]

moveShip <-  function (pos, instr) {
  command <- str_sub(instr, 1, 1)
  value <- as.numeric(str_sub(instr, start = 2))
  
  if (command == "N") {
    pos[2] <- pos[2] + value
  } else if (command == "S") {
    pos[2] <- pos[2] - value
  } else if (command == "E") {
    pos[1] <- pos[1] + value
  } else if (command == "W") {
    pos[1] <- pos[1] - value
  } else if (command == "L") {
    pos[3] <- (pos[3] + value) %% 360
  } else if (command == "R") {
    pos[3] <- (pos[3] - value) %% 360
  } else if (command == "F") { 
    # all directions seem to be on axis
    # but i bet pt 2 wont be
    if (pos[3] == 0) {
      pos[1] <- pos[1] + value
    } else if (pos[3] == 90) {
      pos[2] <- pos[2] + value
    } else if (pos[3] == 180) {
      pos[1] <- pos[1] - value
    } else if (pos[3] == 270) {
      pos[2] <- pos[2] - value
    } else {
      print("UH OH AN OFF-AXIS DIRECTION")
    }
    # this could be so much more efficient and elegant
  }
  return(pos)
}

ship <- c(0,0,0)
for (i in input) {
  ship <- moveShip(ship, i)
}

print(abs(ship[1]) + abs(ship[2]))

library(stringr)
input <- readLines("day12/day12_input.txt")
# [x(+E-W), y(+N-S), dir(0E,90N,180W,270S)]

# clockwise
rotationVector <- c("1,1", "1,-1", "-1,-1", "-1,1")

moveWaypoint <- function (pos, instr) {
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
    
    
    # I HATED DOING MATH ON THIS
    # if this goes wrong it's gonna be impossible to debug
    # IT WAS :(
    # i think i solved it -- it was a lot of diff problems (mostly with rotation)
    # i didnt...
    # okay im back after eating pizza. im going to scrap this and restart
    # yeah this works better, my orig solution had 4 layers of nested if statements
    # sick only took 8 tries...
  } else if (command == "R") {
    
    currQuad <- sign(pos)
    currQuad[currQuad == 0] <- 1 # weird workaround
    
    if (value == 180) {
      pos <- -1 * pos
    } else if (value == 90) {
      newDir <- (match(paste(currQuad, collapse = ","), rotationVector) + 1) %% 4
      if(newDir == 0){newDir<-4}
      pos <- abs(pos[2:1]) * as.numeric(str_split(rotationVector[newDir],",")[[1]])
      
    } else if (value == 270) {
      newDir <- (match(paste(currQuad, collapse = ","), rotationVector) - 1) %% 4
      if(newDir == 0){newDir<-4}
      pos <- abs(pos[2:1]) * as.numeric(str_split(rotationVector[newDir],",")[[1]])
    }
      
    # LEFT
    } else if (command == "L") {
      
      currQuad <- sign(pos)
      currQuad[currQuad == 0] <- 1
      
      if (value == 180) {
        pos <- -1 * pos
      } else if (value == 270) {
        # same as R90
        newDir <- (match(paste(currQuad, collapse = ","), rotationVector) + 1) %% 4
        if(newDir == 0){newDir<-4}
        pos <- abs(pos[2:1]) * as.numeric(str_split(rotationVector[newDir],",")[[1]])
        
      } else if (value == 90) {
        # same as R270
        newDir <- (match(paste(currQuad, collapse = ","), rotationVector) - 1) %% 4
        if(newDir == 0){newDir<-4}
        pos <- abs(pos[2:1]) * as.numeric(str_split(rotationVector[newDir],",")[[1]])
      }
    }
  return(pos)
}

ship <- c(0,0)
waypoint <- c(10, 1) # relative to ship NEVER ABSOLUTE
for (i in input) {
  if (str_sub(i, 1, 1) == "F") {
    ship <- ship + (waypoint * as.numeric(str_sub(i, start = 2)))
  } else {
    waypoint <- moveWaypoint(waypoint, i)
  }
}

print(abs(ship[1]) + abs(ship[2]))

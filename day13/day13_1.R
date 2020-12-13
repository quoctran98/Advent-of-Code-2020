library(stringr)
input <- readLines("day13/day13_input.txt")

arrival <- as.numeric(input[1])
busses <- as.numeric(str_split(str_remove_all(input[2],",x"), ",")[[1]])

closestTime <- ceiling(arrival/busses)*busses
print((min(closestTime) - arrival) * busses[match(min(closestTime), closestTime)])
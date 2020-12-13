library(stringr)
library(numbers)
input <- readLines("day13/day13_input.txt")

busses <- as.numeric(str_split(str_replace_all(input[2],"x", "0"), ",")[[1]])
  
# it's clear to me that i can't (or shouldn't) brute force this
# oh dude these are all primes some math trick lies ahead
# CHINESE REMAINDER THEOREM: thanks https://www.dave4math.com/mathematics/chinese-remainder-theorem/
  
busNumber <- busses[busses!=0]
busOffset <- (busNumber - (match(busses[busses!=0], busses) - 1)) %% busNumber
busOffset[1] <- 0

# i failed at implementing the CRT but packages exist for a reason!
print(format(chinese(busOffset ,busNumber), scientific = F))
  
  

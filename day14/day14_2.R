library(stringr)
library(gtools)
input <- readLines("day14/day14_input.txt")

# using this from https://stackoverflow.com/questions/12892348/convert-binary-string-to-binary-or-decimal-value
# the base "strtoi" function returns NA sometimes with my input (BUT ONLY SOMETIMES??)
base2decimal = function(base_number, base = 2) {
  split_base = strsplit(as.character(base_number), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}

writeMasked <- function (address, mask) {
  maskedBin <- sapply(str_split(paste(rev(intToBits(address))), ""), `[[`, 2)
  # ^ thanks https://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r
  while (length(maskedBin) < 36) {maskedBin <- append(maskedBin, "0", after = 0)}
  maskedBin[mask != "0"] <- mask[mask != "0"]
  
  # i really like this solution actually :)
  allAddresses <- c()
  possibleBits <- permutations(2, sum(maskedBin == "X"), v = c(0,1), repeats.allowed = T)
  for (row in 1:nrow(possibleBits)) {
    thisAddress <- maskedBin
    thisAddress[thisAddress == "X"] <- possibleBits[row,]
    allAddresses <- append(allAddresses, base2decimal(paste(thisAddress, collapse = ""), base = 2))
  }
  
  return(allAddresses)
}

# mask is vector; val is dec int
mask <- rep.int("X", 36)
# i think there's an input that makes a really large vector with lots of NA placeholders
# so now values and addresses are stored separately
memoryVal <- NULL 
memoryAdd <- NULL 

for (command in input) {
  if (str_detect(command, "mask")) {
    mask <- str_split(str_split(command," = ")[[1]][2], "")[[1]]
  } else if (str_detect(command, "mem")) {
    
    address <- as.numeric(str_remove(str_sub(str_split(command," = ")[[1]][1], 5), "]"))
    value <- as.numeric(str_split(command," = ")[[1]][2])
    allAddresses <- writeMasked(address, mask)
    
    for (add in allAddresses) {
      if (add %in% memoryAdd) {
        memoryVal[match(add, memoryAdd)] <- value
      } else {
        memoryVal <- append(memoryVal, value)
        memoryAdd <- append(memoryAdd, add)
      }
    }
    
  } else {
    print("uh oh")
  }
}

print(format(sum(memoryVal, na.rm = T), scientific = F))

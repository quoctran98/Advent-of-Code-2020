library(stringr)
input <- readLines("day14/day14_input.txt")

# using this from https://stackoverflow.com/questions/12892348/convert-binary-string-to-binary-or-decimal-value
# the base "strtoi" function returns NA sometimes with my input (BUT ONLY SOMETIMES?)
base2decimal = function(base_number, base = 2) {
  split_base = strsplit(as.character(base_number), split = "")
  return(sapply(split_base, function(x) sum(as.numeric(x) * base^(rev(seq_along(x) - 1)))))
}

writeMasked <- function (value, mask) {
  maskedBin <- sapply(str_split(paste(rev(intToBits(value))), ""), `[[`, 2)
  # ^ thanks https://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r
  while (length(maskedBin) < 36) {maskedBin <- append(maskedBin, "0", after = 0)}
  maskedBin[mask != "X"] <- mask[mask != "X"]
  return(base2decimal(paste(maskedBin, collapse = ""), base = 2))
}

# memory is array of dec int; mask is vector; val is dec int
mask <- rep.int("X", 36)
memory <- NULL # there's no 0 address in my input, so we're good :)

for (command in input) {
  if (str_detect(command, "mask")) {
    mask <- str_split(str_split(command," = ")[[1]][2], "")[[1]]
    
  } else if (str_detect(command, "mem")) {
    address <- as.numeric(str_remove(str_sub(str_split(command," = ")[[1]][1], 5), "]"))
    value <- as.numeric(str_split(command," = ")[[1]][2])
    memory[address] <- writeMasked(value, mask)
    
  } else {
    print("uh oh")
  }
}

print(format(sum(memory, na.rm = T), scientific = F))

library(stringr)

input <- as.character(read.table("day07/day07_input.txt", sep = "\n")$V1)

# processing all the bags into a big list
bagRules <- list()
for (rule in input) { # all of these can be vectorized but nah ill just loop
  outBag <- str_split(rule, " bags contain")[[1]][1]
  # processing bags inside
  inBags <- str_split(rule, " bags contain ")[[1]][2]
  if (inBags == "no other bags.") {
    bagRules[[outBag]] <- F
  } else {
    inBags <- str_sub(inBags, start = 1, end = str_length(inBags) - 1)
    inBags <- str_replace_all(inBags, " bags", "")
    inBags <- str_replace_all(inBags, " bag", "") # bad workaround :(
    inBags <- str_split(inBags, ", ")[[1]]
    numberedRules <- list()
    for (inBag in inBags) { # numbers are necessary now but i assume they will be later on
      num <- as.numeric(str_split(inBag, " ", n = 2)[[1]][1])
      color <- str_split(inBag, " ", n = 2)[[1]][2]
      numberedRules[[color]] <- num
    }
    bagRules[[outBag]] <- numberedRules
  }
}

hasGold <- function (bags) {
  if ("shiny gold" %in% bags) {
    return(T)
  } else if (length(bags) == 0){
    return(F)
  } else {
    unresolvedBags <- c()
    for (bag in bags){
      unresolvedBags <- append(unresolvedBags, names(bagRules[[bag]]))
    }
    unresolvedBags <- unresolvedBags[unresolvedBags != F]
    
    # god im so scared of recursion
    return(hasGold(unresolvedBags))
  }
}

# extraneous i think
allColors <- names(bagRules)
allColors <- append(allColors, unlist(lapply(bagRules, function(x) return(names(x)))))
allColors <- unique(allColors)
allColors <- allColors[allColors != "shiny gold"] # TRICKY

validBags <- 0
for (bagColor in allColors) {
  validBags <- validBags + hasGold(bagColor)
}
print(validBags)

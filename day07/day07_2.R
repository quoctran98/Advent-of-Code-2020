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
    for (inBag in inBags) { # i was right!
      num <- as.numeric(str_split(inBag, " ", n = 2)[[1]][1])
      color <- str_split(inBag, " ", n = 2)[[1]][2]
      numberedRules[[color]] <- num
    }
    bagRules[[outBag]] <- numberedRules
  }
}
rm(color, inBag, inBags, num, outBag, rule, numberedRules)

bags <- "shiny gold"
bagCount <- 1
globalCount <- 0

countBags <- function (bags, bagCount, globalCount = 0) {
  if (length(bags) == 0){
    return(globalCount)
  } else {

    # one bag at a time instead of by level
    bags <- append(bags, names(bagRules[[bags[1]]]))
    bagCount <- append(bagCount, (unlist(bagRules[[bags[1]]]) * bagCount[1]))
    
    globalCount <- globalCount + (sum(unlist(bagRules[[bags[1]]])) * bagCount[1])
    
    bags <- bags[-1]
    bagCount <- bagCount[-1]
    bagCount <- bagCount[bagCount != 0] # I FORGOT TO REMOVE ZEROES!

    # god im so scared of recursion
    return(countBags(bags = bags, bagCount = bagCount, globalCount = globalCount))
  }
}
print(countBags("shiny gold", 1))

input <- sort(as.numeric(readLines("day10/day10_input.txt")))
joltDiff <- append(input, input[length(input)] + 3) - append(input, 0, 0) # up from PREV joltage

# so some meta commentary:
# last night i wrote a function similar to npaths but it brute forced the whole thing
# that wasnt feasible then someone on the subreddit said that there were REQUIRED nodes
# so here's my thinking as i solved this while waiting for my yeast transformation:

# diffs of 3 are required! the one its on and the one BEFORE it too
# so let's look at the chains of ones between the threes
# 0 or 1 1's before = required
# 2 1's before 
# SCRAP THIS! BRUTE FORCE SMALL CHUNKS TO GET TO THE 3

nPaths <- function (chain, subChain = chain[1], maxDiff = 3) {

  nextChoices <- chain[chain > max(subChain)]
  nextChoices <- nextChoices[nextChoices <= (max(subChain) + 3)]
  
  if (subChain[length(subChain)] == chain[length(chain)]) {
    # chain term success A CASE I FORGOT ABOUT
    return(1)
  }
  
  if (length(nextChoices) == 0) {
    # chain term fail
    return(0)
  } else if (nextChoices[1] == chain[length(chain)]) {
    # chain term success
    return(1)
  }
  
  # chain continues
  currPaths <- 0
  for (choice in nextChoices) {
    currPaths <- currPaths + nPaths(chain = chain, subChain = append(subChain, choice), maxDiff = maxDiffs)
  }
  return(currPaths)
}


totalChains <- 1
reqNodes <- c(1:length(joltDiff)-1)[joltDiff == 3] #INDICES

for (endNode in 1:length(reqNodes)) {
  if (endNode == 1) {
    thisPath <- nPaths(input[1:reqNodes[endNode]], subChain = 0)
    totalChains <- totalChains * thisPath
  } else {
    thisPath <- nPaths(input[reqNodes[endNode-1]:reqNodes[endNode]])
    totalChains <- totalChains * thisPath
  }
}

print(format(totalChains, scientific = F))


library(stringr)

input <- as.character(read.table("day08/day08_input.txt", sep = "\n")$V1)

evalCommand <- function (allInstr, globalVal = 0, doneExec = c(), currExec = 1) {
  if (currExec %in% doneExec) { # stuck in loop
    return(F)
  } else if (currExec > length(allInstr)) { # program terminates
    return(globalVal)
  }
  commandString <- allInstr[currExec]
  command <- str_sub(commandString, 1, 3)
  value <- as.numeric(str_sub(commandString, start = 5))
  if (command == "acc") {
    return(evalCommand(allInstr = allInstr, 
                       globalVal = globalVal + value, 
                       doneExec = append(doneExec, currExec), 
                       currExec = currExec + 1))
  } else if (command == "jmp") {
    return(evalCommand(allInstr = allInstr, 
                       globalVal = globalVal, 
                       doneExec = append(doneExec, currExec), 
                       currExec = currExec + value))
  } else {
    return(evalCommand(allInstr = allInstr, 
                       globalVal = globalVal, 
                       doneExec = append(doneExec, currExec), 
                       currExec = currExec + 1))
  }
}

# time to brute force this while i eat dinner

nopPositions <- c()
jmpPositions <- c()

for (instrNum in 1:length(input)) {
  command <- str_sub(input[instrNum], 1, 3)
  if (command == "nop") {
    nopPositions <- append(nopPositions, instrNum)
  } else if (command == "jmp") {
    jmpPositions <- append(jmpPositions, instrNum)
  }
}

for (nop in nopPositions) {
  oldCommand <- input[nop]
  newCommand <- str_replace(oldCommand, "nop", "jmp")
  
  newInput <- input
  newInput[nop] <- newCommand

  if (evalCommand(newInput) != F) {
    print(evalCommand(newInput))
  }
}

for (jmp in jmpPositions) {
  oldCommand <- input[jmp]
  newCommand <- str_replace(oldCommand, "jmp", "nop")
  
  newInput <- input
  newInput[jmp] <- newCommand
  
  if (evalCommand(newInput) != F) {
    print(evalCommand(newInput))
  }
}

# lmao brute forcing was super fast

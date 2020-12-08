library(stringr)

input <- as.character(read.table("day08/day08_input.txt", sep = "\n")$V1)

evalCommand <- function (allInstr, globalVal = 0, doneExec = c(), currExec = 1) {
  if (currExec %in% doneExec) {
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

print(evalCommand(input))
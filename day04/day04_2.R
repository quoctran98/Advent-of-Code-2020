library(readtext)
library(stringr)

input <- readtext("day04/day04_input.txt")$text
input <- str_split(input, "\n\n")[[1]]

# remember to numerickify the input
checkBYR <- function (byr) {return(byr >= 1920 & byr <= 2002)}
checkIYR <- function (iyr) {return(iyr >= 2010 & iyr <= 2020)}
checkEYR <- function (eyr) {return(eyr >= 2020 & eyr <= 2030)}

checkHGT <- function (hgt) {
  if (str_detect(hgt, "cm")) { # should be good enough
    if(str_length(hgt) != 5) {return(F)}
    hgtCM <- as.numeric(str_sub(hgt, 1, 3))
    return(hgtCM >= 150 & hgtCM <= 193)
  } else if (str_detect(hgt, "in")) {
    if(str_length(hgt) != 4) {return(F)}
    hgtIN <- as.numeric(str_sub(hgt, 1, 2))
    return(hgtIN >= 59 & hgtIN <= 76)
  } else {
    return(F)
  }
}

checkHCL <- function (hcl) {
  if (str_length(hcl) == 7 & str_sub(hcl, 1, 1) == "#") {
    alphanum <- str_sub(hcl, 2, 7)
    return(str_count(alphanum, "a|b|c|d|e|f|0|1|2|3|4|5|6|7|8|9") == 6)
  } else {
    return(F)
  }
}

checkECL <- function (ecl) {return(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"))}

# keep as string!
checkPID <- function (pid) {
  return(str_count(pid, "0|1|2|3|4|5|6|7|8|9") == 9)
  
}

validPassport <- function (passport) {
  
  passportFields <- str_split(passport, "\\s|\n")[[1]]
  
  hasCID <- str_detect(passport, "cid")
  nFields <- length(passportFields)
  
  stillGood <- T
  
  if (nFields == 8) {
    stillGood <- T
  } else if (!hasCID & nFields == 7) {
    stillGood <- T
  } else {
    stillGood <- F
  }
  
  if (!stillGood) {return(F)}
  
  for (field in passportFields) {
    fieldName <- str_split(field, ":")[[1]][1]
    fieldValue <- str_split(field, ":")[[1]][2]
    
    validField <-  switch(fieldName, 
                          "byr" = checkBYR(as.numeric(fieldValue)), 
                          "iyr" = checkIYR(as.numeric(fieldValue)), 
                          "eyr" = checkEYR(as.numeric(fieldValue)),
                          "hgt" = checkHGT(fieldValue),
                          "hcl" = checkHCL(fieldValue),
                          "ecl" = checkECL(as.character(fieldValue)),
                          "pid" = checkPID(as.character(fieldValue)),
                          "cid" = TRUE)
    
    if (!validField) {return(F)}
  }
  return(T)
}

nValid <- 0
for (passport in input) {
  if (validPassport(passport)) {
    nValid <- nValid + 1
  }
}
print(nValid)
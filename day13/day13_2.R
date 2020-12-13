  library(stringr)
  input <- readLines("day13/day13_input.txt")
  #input <- readLines("day13/test.txt")
  
  busses <- as.numeric(str_split(str_replace_all(input[2],"x", "0"), ",")[[1]])
  
  # it's clear to me that i can't (or shouldn't) brute force this
  # oh dude these are all primes some math trick lies ahead
  # CHINESE REMAINDER THEOREM: thanks https://www.dave4math.com/mathematics/chinese-remainder-theorem/
  
  n <- busses[busses!=0]
  a <- n - (match(busses[busses!=0], busses) - 1)
  a[1] <- 0
  a <- a %% n

  N <- prod(n)
  x <- 0
  
  for (i in 1:length(a)) {
    n_with_bar <- N/n[i]
    
    u <- 1
    while ((n_with_bar*u) %% n[i] != 1) {
      u <- u + 1
    }
    
    x <- x + (a[i] * n_with_bar * u)
  }
  
  ans <- 100000000000000
  while (ans %% N != x %% N) {
    ans <- ans + n[1]
  }
  # this last step hangs, so i gave up and used: https://www.dcode.fr/chinese-remainder
  
  print(format(ans, scientific = F))
  
  

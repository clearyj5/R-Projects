M<-matrix(rnorm(100000),nrow=1000, ncol=100)
M

#part1A
rowMeans <- apply(M, 1, mean) #find row means using mean apply function and mean function

#part1B
colMeans <- apply(M, 2, mean) #find col means using mean apply function and mean function

#part1C
sumAndSqRowEntries = function(x){
  a <- 0
  b <- 0
  for(i in 1:length(x)){
    a <- x[i]*x[i]
    b <- b+a
  }
  
  return(b)
}
sumSqRow <- apply(M, 1, sumAndSqRowEntries) #find sum of squared row entris using apply and our written function

#part1D
sumExpRowEntries = function(x){
  a <- 0
  b <- 0
  for(i in 1:length(x)){
    a <- exp(x[i])+1
    b <- b+a
  }
  
  return(b)
}
sumExpRows <- apply(M, 1, sumExpRowEntries)


#part2
#function that takes a number either 1 or 2 as input depending on whether they want to carry out on rows or columns
matrixComputer = function(y){
  
  if (y == 2) {
    means <- apply(M, y, mean)
    stdev <- apply(M, y, sd)
    
    meanNames <- c()
    sdNames <- c()
    
    name <- "Column"
    for (i in 1:ncol(M)){
      m <- paste(name, i, "Mean")
      meanNames <- c(meanNames, m)
      
      s <- paste(name, i, "St Dev")
      sdNames <- c(sdNames, s)
    }
  }
  else {
    means <- apply(M, 1, mean)
    stdev <- apply(M, 1, sd)
    
    meanNames <- c()
    sdNames <- c()
    
    name <- "Row"
    for (i in 1:nrow(M)){
      m <- paste(name, i, "Mean")
      meanNames <- c(meanNames, m)
      
      s <- paste(name, i, "St Dev")
      sdNames <- c(sdNames, s)
    }
  }
  
  names(means) <- meanNames
  names(stdev) <- sdNames
  
  print(means)
  print(stdev)
}

matrixComputer(2)


animals <- c("dogs", "horses", "zebras")
dogs <- c("are man's best friend", "have fur")
horses <- c("sleep standing up", "have four legs")
zebras <- c("have four legs", "are stripey")

count <- 1
while(count <= 100){
  
  animal_number <- sample(1:3, size=1, replace=TRUE)
  fact_number <- sample(1:2, size=1, replace=TRUE)
  
  if (animal_number == 1) {
    fact <- paste("Did you know that", animals[animal_number], dogs[fact_number], "?", "\n")
    cat(fact)
    count <- count + 1
  } else if (animal_number == 2) {
    fact <- paste("Did you know that", animals[animal_number], horses[fact_number], "?", "\n")
    cat(fact)
    count <- count + 1
  } else {
    fact <- paste("Did you know that", animals[animal_number], zebras[fact_number], "?", "\n")
    cat(fact)
    count <- count + 1
  }
}

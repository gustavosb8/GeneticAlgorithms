tournament <- function(pop,fitness,pop.size,dimension,t.size = 4){
  new.pop <- matrix(rep(NA,pop.size*dimension), nrow=pop.size)
  for(i in 1:pop.size){
    idx <- sample(1:pop.size, t.size) 
    pos <- which.min(fitness[idx])
    new.pop[i,] <- pop[idx[pos],]
  }
  return(new.pop)
}
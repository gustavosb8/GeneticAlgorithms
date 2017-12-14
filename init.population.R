init.population <- function(func,lb,ub,pop.size,dimension){
   pop <- matrix(runif(pop.size*dimension),nrow=pop.size)
   fitness <- rep(NA,pop.size)
   pop <- lb + pop*(ub-lb)
   fitness <- apply(pop,1,func)
   return(list(pop = pop, fit = fitness))
}
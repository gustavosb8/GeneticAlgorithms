GA <- function(func, lb, ub, pop.size = 10, dimension = 10, iterations = 100, pc= 0.6, pm = 0.005, sel, t.size = 4, elitism = TRUE){
  source('init.population.R')
  source('simple.crossover.R')
  source('uniform.mutation.R')
  source('selection.R')
  source('roullete.wheel.R')
  source('tournament.R')
  #
 
  tmp.pop <- matrix(rep(NA,pop.size*dimension), nrow = pop.size)
  init <- init.population(func,lb,ub,pop.size,dimension)
  pop <- init$pop
  fitness <- init$fit
  print("Running...")
  for(i in 1:iterations){
    #print(i)
    tmp.pop <- selection(pop,pop.size,dimension,fitness,sel,t.size)
    tmp.pop <- simple.crossover(lb, ub, pop.size, dimension, tmp.pop,pc)
    tmp.pop <- uniform.mutation(func,lb,ub,tmp.pop,pop.size,dimension,pm)
    tmp.fitness <- tmp.pop$fit
    tmp.pop <- tmp.pop$pop
    if (elitism == TRUE){
      best.tmp <- min(tmp.fitness)
      best.old <- min(fitness)
      if (best.old < best.tmp){
        idx <- which.min(fitness)
        idx.worst <- which.max(tmp.fitness)
        tmp.pop[idx.worst,] <- pop[idx,]
        tmp.fitness[idx.worst] <- fitness[idx]
      }
    } 
    fitness <- tmp.fitness
    pop <- tmp.pop
  }
  return(list(pop = pop, fit = fitness))
}
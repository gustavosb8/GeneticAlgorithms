uniform.mutation <- function(func,lb,ub,pop,pop.size,dimension,pm){
    r <- matrix(runif(pop.size*dimension),nrow=pop.size)
    fitness <- rep(NA,pop.size)
    #it gets the index of genes being mutated
    idx <- which(r < pm,arr.ind=TRUE)
    tmp.pop <- pop
    index <- sample(1:dimension, 2, replace = FALSE);
    tmp.pop[idx[,2],index[1]] <- pop[idx[,2], index[2]]
    tmp.pop[idx[,2],index[2]] <- pop[idx[,2], index[1]]
    pop <- tmp.pop
    fitness <- apply(pop,1,func)
    return(list(pop = pop, fit = fitness))
  }


### UNIFORM MUTATION ###########################################################
#
# uniform.mutation <- function(func,lb,ub,pop,pop.size,dimension,pm){
#   r <- matrix(runif(pop.size*dimension),nrow=pop.size)
#   fitness <- rep(NA,pop.size)
#   #it gets the index of genes being mutated 
#   idx <- which(r < pm,arr.ind=TRUE)
#   pop[idx] <- lb[idx[,2]] + runif(nrow(idx)) * (ub[idx[,2]] - lb[idx[,2]])
#   fitness <- apply(pop,1,func)
#   return(list(pop = pop, fit = fitness))
# }
#
################################################################################

### ORDER CHANGING MUTATION#####################################################
#
# uniform.mutation <- function(func,lb,ub,pop,pop.size,dimension,pm){
#   r <- matrix(runif(pop.size*dimension),nrow=pop.size)
#   fitness <- rep(NA,pop.size)
#   #it gets the index of genes being mutated 
#   idx <- which(r < pm,arr.ind=TRUE)
#   tmp.pop <- pop
#   index <- sample(1:dimension, 2, replace = FALSE);
#   tmp.pop[idx[,2],index[1]] <- pop[idx[,2], index[2]]
#   tmp.pop[idx[,2],index[2]] <- pop[idx[,2], index[1]]
#   pop <- tmp.pop
#   fitness <- apply(pop,1,func)
#   return(list(pop = pop, fit = fitness))
# }
#
################################################################################




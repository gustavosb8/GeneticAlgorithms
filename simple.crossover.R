simple.crossover <- function(lb,ub,pop.size,dimension,pop,pc){
    new.pop <- matrix(rep(NA,pop.size*dimension), nrow = pop.size)
    r <- runif(pop.size)
    idx <- (r < pc)
    pop <- pop[idx,]
    #Declaração dos vetores prole
    tmp.pop.size <- nrow(pop)
    for(i in seq(1,pop.size, by = 2)){

      indivs <- sample(1:tmp.pop.size,2,replace = FALSE)

      k <- runif(1);
      offspring1 <- pop[indivs[1], ] + k*(pop[indivs[2], ] - pop[indivs[1],  ])

      k <- runif(1);
      offspring2 <- pop[indivs[2], ] + k*(pop[indivs[1], ] - pop[indivs[2],  ])


      new.pop[i,]   <- offspring1
      new.pop[i+1,] <- offspring2
    }

    return(pop = new.pop)
  }

#### Discret Crossover###########################################################################
##
# simple.crossover <- function(lb,ub,pop.size,dimension,pop,pc){
#   new.pop <- matrix(rep(NA,pop.size*dimension), nrow = pop.size)
#   r <- runif(pop.size)
#   idx <- (r < pc)
#   pop <- pop[idx,]
#   
#   for(i in seq(1,pop.size, by = 2)){
#     k <- sample(1:nrow(pop),2, replace = FALSE)
#     #Declaração dos vetores prole
#     offspring1 <- pop[k[1],]
#     offspring2 <- pop[k[2],]
#     
#     #Sorteio dos genes para cruzamento
#     indivs <- sample(1:dimension, 5,replace = FALSE)     
#     offspring1[indivs]  <- offspring2[indivs]
#     
#     #Novo Sorteio dos genes para cruzamento
#     indivs <- sample(1:dimension, 5,replace = FALSE)
#     offspring2[indivs]  <- offspring1[indivs]
#     
#     new.pop[i,]   <- offspring1
#     new.pop[i+1,] <- offspring2
#   }
#   
#   return(pop = new.pop)
# }
#################################################################################################


#### Aritmetic Crossover###########################################################################
##
# simple.crossover <- function(lb,ub,pop.size,dimension,pop,pc){
#   new.pop <- matrix(rep(NA,pop.size*dimension), nrow = pop.size)
#   r <- runif(pop.size)
#   idx <- (r < pc)
#   pop <- pop[idx,]
#   #Declaração dos vetores prole
#   tmp.pop.size <- nrow(pop)
#   for(i in seq(1,pop.size, by = 2)){
#     
#     indivs <- sample(1:tmp.pop.size,2,replace = FALSE)
#     
#     k <- runif(1);
#     offspring1 <- pop[indivs[1], ] + k*(pop[indivs[2], ] - pop[indivs[1],  ])
#     
#     k <- runif(1);
#     offspring2 <- pop[indivs[2], ] + k*(pop[indivs[1], ] - pop[indivs[2],  ])
#     
#     
#     new.pop[i,]   <- offspring1
#     new.pop[i+1,] <- offspring2
#   }
#   
#   return(pop = new.pop)
# }
#
#################################################################################################


#### SIMPLE CROSSOVER ###########################################################################
##
# simple.crossover <- function(lb,ub,pop.size,dimension,pop,pc){
#   new.pop <- matrix(rep(NA,pop.size*dimension), nrow = pop.size)
#   r <- runif(pop.size)
#   idx <- (r < pc)
#   pop <- pop[idx,]
#   tmp.pop.size <- nrow(pop)
#   for(i in seq(1,pop.size, by = 2)){
#     indivs <- sample(1:tmp.pop.size,2,replace = FALSE)
#     cross.point  <- sample(2:(dimension-1),1)
#     offspring1   <- c(pop[indivs[1],1:cross.point],pop[indivs[2],(cross.point+1):dimension])
#     offspring2   <- c(pop[indivs[2],1:cross.point],pop[indivs[1],(cross.point+1):dimension])
#     new.pop[i,]   <- offspring1
#     new.pop[i+1,] <- offspring2
#   }
#   return(pop = new.pop)
# }
#
##################################################################################################

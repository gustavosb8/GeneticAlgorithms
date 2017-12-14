##source('GA.R')
##source('Rosenbrock.R')

source('C:/Users/Gustavo Bastos/Desktop/AG-Implementation/GA.R')

source('C:/Users/Gustavo Bastos/Desktop/AG-Implementation/Rosenbrock.R')

it <- 1000
pop.size <- 30
dim <- 30
lb <- rep(-5,dim)
ub <- rep(5,dim)
func <- Rosenbrock
execs <- 50
soma <- 0

result <-  vector("list",execs)
best <- rep(NA,execs)

for(i in 1:execs){
  cat("exec = ",i,"\n")
  result[[i]] <- GA(func, lb, ub, pop.size = pop.size, dimension = dim, iterations = it, pc= 0.8, pm = 0.05, sel = 2, elitism = TRUE)
  best[i] <- min(result[[i]]$fit)
  soma <- soma + best[i]
}

cat("\nMédia: ",soma/execs,"\n")
cat("\nDesvio Padrão: ", sd(best), "\n");

cat("\nT-Teste: ")
t.test(best)

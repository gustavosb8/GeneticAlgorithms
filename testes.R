##testes


# NOT RUN {
require(graphics)

t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Classical example: Student's sleep data
plot(extra ~ group, data = sleep)
## Traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))
## Formula interface
t.test(extra ~ group, data = sleep)
# }


cat(runif(1))

cross.point  <- sample(2:(10-1),1)
cat(cross.point)

cat(5:(10))

sample(2:(10-1),2)

dim <- 30
lb <- rep(-5,dim)
ub <- rep(5,dim)
cat(lb)
cat(ub)

pc= 0.6
pm = 0.005
r <- runif(pop.size)
idx <- (r < pc)
cat(idx)
dimension = 10
cat(pop[idx,])
seq(1,pop.size, by = 2)
new.pop <- matrix(rep(NA,pop.size*dimension), nrow = pop.size)
r <- runif(pop.size)

sample(1:10,replace = FALSE)
sample(1:tmp.pop.size,2,replace = FALSE)

indivs <- sample(1:dimension, 5,replace = FALSE)
dimension <- 10
cat(sample(1:dimension, 5,replace = FALSE))

cat(pop[1,10])

cat(pop[1,])
cat(pop[2,])


for(i in seq(1,pop.size)){  
  cat(i," ",length(pop[i, ]) ,"\n")
}

idx <- (r < pc)
pop <- pop[idx,]
tmp.pop.size <- nrow(pop)
init <- init.population(func,lb,ub,pop.size,dimension)
pop <- init$pop
tmp.pop.size <- nrow(pop)
cat(sample(1:tmp.pop.size,(pop.size/2),replace = FALSE))
cat(indivs <- sample(1:tmp.pop.size,(pop.size/2),replace = FALSE))
cross.point  <- sample(2:(dimension-1),1)
offspring1   <- c(pop[indivs[1],1:cross.point],pop[indivs[2],(cross.point+1):dimension])
cat(pop)

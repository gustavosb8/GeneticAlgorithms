Rosenbrock <- function(x){
  s <- 0
  for (i in 1:(length(x)-1)){
     s <- s + 100*(x[i+1]-x[i]^2)^2+(x[i]-1)^2  
  } 
  return(s)
}
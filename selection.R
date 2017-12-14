selection <- function(pop,pop.size,dimension,fitness,sel,t.size){
   if (sel == 1)
      new.pop <- roulette.wheel(pop,fitness,pop.size,dimension)
   else
      new.pop <- tournament(pop,fitness,pop.size,dimension,t.size)
   return(new.pop)
}
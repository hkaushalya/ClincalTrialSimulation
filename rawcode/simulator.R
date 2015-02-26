rm(list=ls())

N <- 20  # Number of patients in the trail
pop <- seq(1, N)


RandomGroup<-function(n, pop) {
  grp <- c()

  if (length(pop)<n) {
    grp = sample(pop, length(pop), replace = F)  
  } else
  {
    grp = sample(pop, size=n, replace =F)
  }
  
  # remove the selected people from population
  cat (pop)
  print("\n")
  cat(grp)
  print("\n")
  
  return (grp)
  #msg <- paste0("selected:", grp , " remaining pop:", cat(pop), "\n")
  #print(msg)
}


for (i in 1:10) {
  print(paste0("i === " , i))
  grp <- RandomGroup(3, pop)
  pop <- setdiff(pop,grp)
  
}

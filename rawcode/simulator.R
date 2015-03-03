rm(list=ls())

set.seed(1234)
gN <- 100  # Number of patients in the trail
gPOP <- seq(1, gN)
gSAMPLE_SIZE <- 3
gDOSE_LEVELS <- seq(25,500, 50)
gMIN_DOSE_SEARCH_STEP <- 5   # this will be minimal dose level seperation
gDOSE_BREAKS <- 4                  # number of dose intervals for a given dose range

#When a DLT is found, refine the dose level to find a better MTD
# Here maxDose will be the DLT for and minDose will be the MTD (dose before DLT)
RefineDose <- function(minDose, maxDose)
{
    #ToDo: Want to make this int
    #This could be some function. For now lets split to constant intervals
    di <- (maxDose - minDose) / gDOSE_BREAKS
    doses <- seq(from=minDose, to=maxDose, by=floor(di))
    
    return (doses)
}

#Pick a random group from pop without replacement
RandomGroup<-function(sampleSize, pop) {
  grp <- c()

  if (length(pop) < sampleSize) {
    grp = sample(pop, length(pop), replace = F)  
  } else
  {
    grp = sample(pop, size=sampleSize, replace =F)
  }
  return (grp)
}

#### Patient Drop Out Simulator
Dropout <- function()
{
  # How do we make this random
  #lambda <- 1
  #p <- dpois(1, lambda)
  
  drop_prob <- runif(1)
  print(paste0("drop_prob=", drop_prob))
  if (drop_prob>0.8)  {
    return TRUE
  } else {
    return FALSE
  }
}

Simulate<- function(Npat, doseLevels)
{

  if (missing(Npat)) {
    print(paste0("Simulate: input Npat not found" ))
  }
  if (missing(doseLevels)) {
    print(paste0("Simulate: input doseLevels not found" ))
  }
  
  DLT <- 0
  MTD <- 0
  
  for (dl in 1:length(doseLevels)) {
    grp <- RandomGroup(gSAMPLE_SIZE, Npat)
    Npat <- setdiff(Npat, grp)
    
    #simulate drop outs with very low prob
    if (Dropout) {
      grp_plus <- RandomGroup(gSAMPLE_SIZE, Npat)
      Npat <- setdiff(Npat, grp_plus)
      grp <- grp[1:(length(g)-1)]  # drop the last patiend in this list for now
      grp <- c(grp, grp_plus)       # new list of patients taking the current dose
      print(paste0("A patient drops out simulated."))
    }

    dltProbs <- runif(length(grp))
    print (paste0("========== dl=",dl, "=============="))
    print(paste0("Npat=", length(Npat)))
    print(dltProbs)
    
    
    if ( sum(dltProbs> 0.65) >= 2) { #2/3 pateints show toxic levels and DLT reached (0.8 is arbitray for now)
      # MTD is reached
      DLT <- doseLevels[dl]
      MTD <- DLT
      if ( (dl-1) != 1) {
        MTD <- doseLevels[dl-1]
      }
      print("\n")
      print (paste0("DLT Reached@MTD/DLT:", MTD, "/",DLT))
      return (DLT) 
    
      } else {
        MTD <- doseLevels[dl]
      }
  } # for
  
  # If no DLT reached in the loop, means no DLT condition found
  print("No DLT conditions found! Last dose tested was:")
  print(paste0("MTD=", MTD, " DLT=", DLT))
  
}

################### Main Routine

Simulate(gPOP, gDOSE_LEVELS)

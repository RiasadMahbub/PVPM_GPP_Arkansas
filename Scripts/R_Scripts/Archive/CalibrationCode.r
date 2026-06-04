# Paper: A dynamic-leaf light use efficiency model for improving gross primary production estimation
#Inputs:
# GPPsim: numeric vector of simulated GPP values
# GPPEC: numeric vector of observed GPP values

agreement_index <- function(GPPsim, GPPEC) {
  numerator <- sum((GPPsim - GPPEC)^2)
  mean_GPPEC <- mean(GPPEC)
  denominator <- sum((abs(GPPsim - mean_GPPEC) + abs(GPPEC - mean_GPPEC))^2)
  d <- 1 - (numerator / denominator)
  return(d)
}

### Functions to estimate the gs 
calc_ga <-function(u, ustar) {
  ((u/(ustar^2)+ 6.2*(ustar^(-2/3))))^-1
}


### function that calculates GPP using VPM model
calculate_gpp_vpm <- function(ts, ws, luemax, fpar, par) {
  # Constants
  conversion_factor <- 12.011
  
  # Calculate GPP using VPM model
  gpp <- ts * ws * luemax * fpar * par * conversion_factor
  
  return(gpp)
}



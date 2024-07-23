## Calculate Sensible heat flux

## air density (rho) = 1.225 kg/m^3 (https://macinstruments.com/blog/what-is-the-density-of-air-at-stp/)
## Specific heat of air (Cp) = 1004.67 J/kgK (Zhang et al., 2020)
## ga = conductance


calc_H<-function(rho, Cp, ga, Ts, Ta){
  rho * Cp* ga*(Ts - Ta)
}

x<-c(1,2,3,4,5,6,7,7,8)


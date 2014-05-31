# vpd.R
# calculate vapro pressure deficit from RH and temperature data

vp.sat.groff <- function(C){
  T = C + 273.15 
  lew <-  10.79574 * (1-273.16/T)  - 5.02800 * log10( T/273.16 ) + 1.50475E-4 * (1.0 - 10^(-8.2969 * ( T/273.16 - 1.0)) )      + 0.42873E-3 * (10^(4.76955*(1-273.16/T)) - 1) + 0.78614
  
return(10^lew)
}

vpd <- function(rh,C){
  vpsat <- vp.sat.groff(C)
  return(vpsat -  vpsat * rh/100)
}


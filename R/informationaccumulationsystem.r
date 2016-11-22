ias <- function(y,fac,Delta) {
  Om0 <- fac*Delta
  OmX <- 0.25*fac*Delta
  y <- c( (1-Delta)*y[1] + (Om0*y[1] + OmX*y[2])*(1-abs(y[1])) ,
          (1-Delta)*y[2] + (OmX*y[1] + Om0*y[2])*(1-abs(y[2])) )
  return(y)
}
fix_ias <- function(init_y,fac,Delta) {
  repeat {
    y <- ias(init_y,fac)
    if (sum(abs(init_y - y)) < 10^-12) break
    else init_y <- y
  }
  return(y)
}
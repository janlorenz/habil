sx <- 1
x <- seq(1,10,by=sx)
st <- 0.01
t <- seq(-20+st/2,20-st/2,by=st)

intFourier <- function(x,phi,t) {
  st <- t[2]-t[1]
  f <- Re(1/2/pi*sum(phi*exp(-complex(imaginary = t*x)))*st)
  return(f)
}

LevySkewAlphaStablePdf <- function(x,a,b,c,mu) {
  PHI = tan(pi*a/2);
  ibs = complex(imaginary=b*sign(t))
  ct = abs(c*t);
  lct = log(ct);
  cta = ct^a;
  ct1a = ct^(1-a);
  lctcta = lct*cta;
  FXphi = exp( -cta*(1+PHI*ibs*(ct1a-1)) + complex(imaginary = mu*t))
  X <- rep(NA,length(x))
  for (n in 1:length(x)) X[n] = intFourier(x[n],FXphi,t)
  return(X)
}

b1 <- 0.1178
b2 <- -0.9049
c1 <- 0.05342
c2 <- -0.8388
c3 <- 4.401
a <- 4/3

overlapsupport <- 30;
x_ext <- c(seq(x[1]-overlapsupport*sx, x[1]-sx, by=sx), x, seq(x[10]+sx, x[10]+overlapsupport*sx, by=sx))

confinedLevySkewAlphaStablePdf <- function(mu) {
  b <- b1*mu + b2
  c <- c1*mu^2 + c2*mu + c3
  X <- LevySkewAlphaStablePdf(x_ext,a,b,c,mu)
  R = c(sum(X[1:overlapsupport+1]), 
        X[(overlapsupport+2):(overlapsupport+length(x)-1)], 
        sum(X[(overlapsupport+length(x)):(length(X))]))
  return(R)
}





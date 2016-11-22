powermean <- function(x,p) {
  if (p==0) {return(exp(mean(log(x),na.rm=T)))}
  else {return(mean(x^p, na.rm=T)^(1/p))}
}
WoCIndicator <- function(S,tr) {
  S <- sort(S)
  n <- length(S)
  median_ind <- ceiling((n+1)/2)
  return( min( max(which(S<=tr)) , n-min(which(S>=tr))+1 ) / median_ind )
}
popBias <- function(S,tr=1) {(mean(S) - tr)^2}
groupDiv <- function(S) {mean((S-mean(S))^2)}
MSE <- function(S,tr) {mean((S-tr)^2)}
normS <- function(S,tr) {S/tr} 
standardScoreTruth <- function(S,tr) {(tr-mean(S))/sd(S)}

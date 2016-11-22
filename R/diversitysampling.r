collError <- function(x, truth, exponent=2) abs(x-truth)^exponent
SEM <- function(x) sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))

specialSample <- function(x, size = 20, samplingstrategy = "diversity", replace = FALSE) {
  s <- rep(NA,size)
  inds <- sample.int(length(x),size=2, replace=replace)
  s[1:2] <- x[inds]
  if (!replace) { x <- x[-inds] }
  for (i in 3:size) {
    if (samplingstrategy == "diversity") {
      ind <- sample(which(x > mean(s,na.rm=TRUE)+SEM(s) | x < mean(s,na.rm=TRUE)-SEM(s)), 1, replace=replace)
    } 
    if (samplingstrategy == "conformity") {
      ind <- sample(which(x < mean(s,na.rm=TRUE)+SEM(s) | x > mean(s,na.rm=TRUE)-SEM(s)), 1, replace=replace)
    }
    s[i] <- x[ind]
    if (!replace) { x <- x[-ind] }
  }
  return(s)
}

make_n_Samples <- function(x, groupsize, n=1000, replace=FALSE, aggregation.fun="mean") {
  test <- data_frame(normal=rep(NA,n), diversity=rep(NA,n))
  for (j in 1:n) {
    test[j,"normal"] <- do.call(aggregation.fun,list( sample(x,groupsize,replace=replace) ))
    test[j,"diversity"] <- do.call(aggregation.fun, list( 
      specialSample(x,groupsize,samplingstrategy = "diversity",replace=replace) ))
    # test[j,"conformity"] <- mean(specialSample(x,groupsize,samplingstrategy = "conformity"))
  }
  return(test)
}
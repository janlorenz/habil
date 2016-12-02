specialSample <- function(x, size = 20, samplingstrategy = "diversity", replace = FALSE) {
  if (samplingstrategy=="random") {
    if (is.character(x)) {
      s  <- do.call(x,list(size))
    } else {
      s <- x[sample(length(x), size, replace = replace)]
    }
  } else {
    s <- rep(NA,size)
    if (is.character(x)) {
      s[1:2] <- do.call(x,list(2))
    } else {
      inds <- sample.int(length(x),size=2, replace=replace)
      s[1:2] <- x[inds]
      if (!replace) { x <- x[-inds] }
    }
    for (i in 3:size) {
      if (is.character(x)) {
        if (samplingstrategy == "diversity") {
          repeat {
            S <- do.call(x,list(1)) 
            if (S > mean(s,na.rm=TRUE)+SEM(s) | S < mean(s,na.rm=TRUE)-SEM(s)) break
          }
        } 
        if (samplingstrategy == "conformity") {
          repeat {
            S <- do.call(x,list(1)) 
            if (S < mean(s,na.rm=TRUE)+SEM(s) & S > mean(s,na.rm=TRUE)-SEM(s)) break
          }
        }
        s[i] <- S
      } else {
        if (samplingstrategy == "diversity") {
          ind <- sample(which(x > mean(s,na.rm=TRUE)+SEM(s) | x < mean(s,na.rm=TRUE)-SEM(s)), 1, replace=replace)
        } 
        if (samplingstrategy == "conformity") {
          ind <- sample(which(x < mean(s,na.rm=TRUE)+SEM(s) & x > mean(s,na.rm=TRUE)-SEM(s)), 1, replace=replace)
        }
        s[i] <- x[ind]
        if (!replace) { x <- x[-ind] }
      }
    }
  }
  return(s)
}

make_n_Samples <- function(x, crowdsize, n=1000, replace=FALSE, aggregation.fun="mean") {
  # makes n test samples of size crowdsize with normal, diversity, and conformity sampling from the sample x
  test <- data_frame(diversity=rep(NA,n), random=rep(NA,n), conformity=rep(NA,n))
  for (j in 1:n) {
    test[j,"diversity"] <- do.call(aggregation.fun, list( 
      specialSample(x,crowdsize,samplingstrategy = "diversity",replace=replace) ))
    test[j,"random"] <- do.call(aggregation.fun, list( 
      specialSample(x,crowdsize,samplingstrategy = "random",replace=replace) ))
    # test[j,"conformity"] <- do.call(aggregation.fun, list( 
    # specialSample(x,crowdsize,samplingstrategy = "conformity",replace=replace) ))
  }
  test$crowdsize <- crowdsize
  test$aggregation.fun <- aggregation.fun
  return(test)
}

arrange_test <- function(test,truth) {
  gather(test, key=key, value=value, diversity, random) %>% 
    mutate(collErr = collError(value, truth, exponent=1), 
           key = factor(key,levels = c("random","diversity")))
}


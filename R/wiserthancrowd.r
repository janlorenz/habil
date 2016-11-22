## help functions for probability density functions
H <- function(x) 0.5*sign(x)+0.5 # Heaviside-Function
densityAbs <- function(x,f) H(x)*f + H(x)*rev(f) # works for f symmetric around zero only
convolution <- function(f,g) convolve(f, rev(g), type = "o") # usual convolution according to ?convolve

## For theory with normal distributions
prob_X_closer_zero_than_Y <- function(x,fX,fY) { 
  # x, fX, fY quantify the density functions of random variables X and Y
  # x must be a regular seq symmetric around 0
  dx <- x[2]-x[1]
  pdf_absfX_minus_absfY <- convolution(densityAbs(x,fX), rev(densityAbs(x,fY))
                                       )[(length(x)-(length(x)-1)/2):(length(x)+(length(x)-1)/2)]*dx
  return(dx*(sum(pdf_absfX_minus_absfY[x<0]) + 0.5*pdf_absfX_minus_absfY[x==0]))
}

prob_1_better_k_normal <- function(b, k) {
  # for normally distributed estimates
  # consider w.o.l.g. that the truth is zero, standard deviation of estimates is 1
  # there is a bias b
  # compute the probability that one estimate is better (i.e. closer to truth) than the mean of k
  if (k==Inf) {return(abs(pnorm(b, b, 1)-pnorm(-b,b,1)))}
  else {
    dx <- 0.01
    x <- seq(-7,7,dx)
    fx <- dnorm(x, mean = b)
    fxk <- dnorm(x, mean = b, sd = 1/sqrt(k))
    return(prob_X_closer_zero_than_Y(x,fx,fxk))
  }
}

optimal_k_normal <- function(b,initial_k=1) {
  # for normally distributed estimates
  # consider w.o.l.g. that the truth is zero, standard deviation of estimates is 1
  # there is a bias b
  # compute the groupsize k such that the probability to find an expert is lowest
  # use initial_k if you know that optimal_k is higher
  if (b==0) {return(Inf)}
  else {
    k <- initial_k
    P <- prob_1_better_k_normal(b, k) 
    P_new <- prob_1_better_k_normal(b, k+1) 
    while (P_new < P) {
      k <- k+1
      P <- P_new
      P_new <- prob_1_better_k_normal(b, k+1) 
    }
    return(list(k=k,P=P))
  }
}

## For empirical samples and aggregation by the median

weights_median_n_k <- function(n,k) {
  # computes weight vector for a sorted sample of length n
  # each weight is the probability that this value is the median of k random draws 
  # with replacement from the sample 
  # The sample vector must be sorted to apply the weights properly!
  # Note: For k even the median is interpreted as a random draw from the two median estimates
  P <- rep(0,n) # Will become the probabilities of each number to be the median of n draws
  for (i in 1:n) {
    # Original Idea: Prob. X_1=S[i]   Probs. X2,...<=S[i]   Probs. X_j,...>=S[i]
    # P[i] <-               1/n     *    (i/n)^((n-1)/2)  * ((n-i+1)/n)^((n-1)/2)
    # Compute with logs to avoid P being zero
    P[i] <- -log(n) + ((k-1)/2)*(log(i)-log(n)) +  ((k-1)/2)*(log(n-i+1)-log(n)) 
  }
  P <- exp(P - max(P) + exp(4)) # shift P before exponentiation such that exponential is in "good" range
  P <- P/sum(P) # The shift gets leveled out here through normalization
  return(P) # P is a weight vector for the probabilities of the values of S to be the median
}

prob_1_better_k_median_S <- function(S,k,truth) {
  # computes the probability of a random draw from S to be closer to the truth then the median of k random draws
  P <- weights_median_n_k(length(S),k)
  St <- abs(sort(S)-truth)
  # P[order(St)] represents the probabilities to achieve the best, 2nd, 3rd, 4th, ... best answer as the median in that order
  # (0:(length(P)-1))/length(P) ) represents the probabilities to draw a single estimate which is lower in the same order
  # Thus, sum( P[order(St)]*(0:(length(P)-1))/length(P) ) is the desired probability
  return(sum( P[order(St)]*(0:(length(P)-1))/length(P) ))
}

optimal_k_median_S <- function(S,truth,initial_k=1) {
  # for a sample S and truth
  # compute the groupsize k such that the probability to find an expert is lowest
  # use initial_k if you know that optimal_k is higher
  if (median(S)==truth) {return(Inf)}
  else {
    k <- initial_k
    P <- prob_1_better_k_median_S(S, k, truth) 
    P_new <- prob_1_better_k_median_S(S, k+1, truth) 
    while (P_new < P) {
      k <- k+1
      P <- P_new
      P_new <- prob_1_better_k_median_S(S, k+1, truth)  
    }
    return(list(k=k,P=P))
  }
}
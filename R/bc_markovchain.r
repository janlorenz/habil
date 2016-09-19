## FUNCTIONS
# Variables 
# bins: number bins as discretization of opinion space
# eps: bound of confidence in [0,1]
# e = eps*bins: bound of confidence in terms of the number of bins
# d: distance of two opinions
# p: probability of random opinion replacement

# compute the probability of communication for agents of classes i and j with d=|j-i|, 
# when bound of confidence is e
probCommDist = function(e,d) {
  pr=rep(0,length(e));
  for (k in 1:length(e)) {
    if (e[k] >= d+1) {pr[k] = 1;}
    else {
      if (e[k] <= d-1) {pr[k] = 0;} 
      else {
        if (e[k] >= d) {pr[k]= e[k]-d + (d+1-e[k])*(e[k]-d+1)/2;}
        else {pr[k] = (e[k]-(d-1))^2/2;}
      }
    }
  }
  return(pr);
}

probComm = function(i,k,e) {
  pr=rep(0,length(k));
  for (l in 1:length(k)) {pr[l] = probCommDist(e,abs(i-k[l]));}
  return(pr)
}

# Compute probability to adjust to j when having i and hearing k
probAdjust = function(j,i,k) {
  pr=length(k);
  for (l in 1:length(k)) {pr[l] = max(1-abs((i+k[l])/2-j),0);}
  return(pr);
}

trans_dw = function(P,e) {
  # DW transition matrix
  n = length(P);
  Tr = matrix(0,nrow=n,ncol=n);
  bincenters = seq(1/n,1,by=1/n)-0.5/n;
  for (i in 1:n) { for (j in setdiff(intersect(1:n,(i-ceiling(e)):(i+ceiling(e))),i)) {
    kinds = intersect(1:n ,(i+2*(j-i)-1):(i+2*(j-i)+1));
    if (length(kinds)>0) {Tr[i,j] = sum(probComm(i,kinds,e)*probAdjust(j,i,kinds)*P[kinds]);}
  }}
  for (i in 1:n) {Tr[i,i] = 1-sum(Tr[i,]);}
  return(Tr);
}

random_Pstart = function(n) {
  P=runif(length(P_init))
  return(P/sum(P))
}

stableP = function(P_init=rep(1/201,201),P=random_Pstart(length(P_init)),eps=0.2,p=0.12,prec=10^-5) {
  # Iterates the landscape until it stablizes
  # P=runif(length(P_init)) is to avoid a hyper-symmertric initial vector, 
  # which could lead to fixed points with small basin
  t = 0 
  repeat {
    t = t+1
    Pnew = P%*%trans_dw(P,eps*length(P_init)) # adaptation
    Pnew = p*P_init + (1-p)*Pnew   # opinion replacement
    if (sum(abs(Pnew-P))<prec) {
      return(Pnew)
      break
    } else {
      P = Pnew
    }
  }
}

# M = matrix(P_init, nrow=length(P_init), ncol=length(E))
# for (m in c(0.09,0.12,0.2)/2) {
#   save(M,file=paste0("R/bc_opinion_pattern_diagram_m",m,".RData"))
# }

# m=0.12
# load(paste0("R/bc_opinion_pattern_diagram_m",m,".RData"))
# M = cbind(M,matrix(P_init, nrow=length(P_init), ncol=25))
# image(M)
# save(M,file=paste0("R/bc_opinion_pattern_diagram_m",m,".RData"))


E = seq(0.1,0.4,by=0.002)
P_init = rep(1/101,101)
for (m in c(0.09,0.12,0.2)/2) {
  load(paste0("R/bc_opinion_pattern_diagram_m",m,".RData"))
  for (i in 1:length(E)) {
    print(i)
    M[,i] = stableP(P_init = P_init, P = M[,i], eps = E[i], p = m, prec=10^-14)
    save(M,file=paste0("R/bc_opinion_pattern_diagram_m",m,".RData"))
  }
}

# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(reshape2)
# E = seq(0.1,0.4,by=0.002)
# Ops = seq(0,1,length.out = 101)
# load(paste0("bc_opinion_pattern_diagram_m",0.12,".RData"))
# R <- melt(M)
# names(R) <- c("opinion","confidencebound","value")
# R$opinion <- Ops[R$opinion]
# R$confidencebound <- E[R$confidencebound]

# R <- tbl_df(matrix(NA, nrow=prod(dim(M)), ncol=3))
# names(R) <- c("opinion","confidencebound","freq")
# k <- 0
# for (i in 1:dim(M)[1]) {
#   for (j in 1:dim(M)[2]) {
#     k <- k+1
#     R[k,] <- c(Ops[i],E[j],M[i,j])
#   }
# }
# image(t(M),col = rainbow(50))
# d <- gather(tbl_df(M))
# ggplot(gather(tbl_df(M))
# (M)
# tbl_df(M)

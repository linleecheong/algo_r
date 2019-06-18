# The F-function on p.16 of my written notes
Ffun <- function(a,b,mu,si,x){
  xs <- (x-mu)/si
  as <- (a-mu)/si
  bs <- (b-mu)/si

  f <- function(y) (1/y-1/y^3+3/y^5-15/y^7);

  if (as>4 & bs>4){
    F <- 1-(exp((as^2-bs^2)/2)*f(bs) - exp((as^2-xs^2)/2)*f(xs))/
      ( exp((as^2-bs^2)/2)*f(bs) - f(as) )
  } else if (as< -4 & bs< -4) {
    F <- ( exp((bs^2-xs^2)/2)*f(-xs) - exp((bs^2-as^2)/2)*f(-as))/
      ( f(-bs)-exp((bs^2-as^2)/2)*f(-as) )
  } else {
    denom <- pnorm(bs)-pnorm(as)
    if (denom < 0.00001){
      F <- (xs-as)/(bs-as)
    } else {
      F <- (pnorm(xs)-pnorm(as))/denom
    }
  }
  return(F)
}

numSim <- 500  # number of simulations
Reject_zrand <- rep(0,numSim)
Reject_zmax <- rep(0,numSim)
Reject_taylor <- rep(0,numSim)

for (i in 1:numSim){
  # Generate 5 random variables from N(0,1)
  Zs <- rnorm(5)

  # Reject zrand if zrand>1.645
  Reject_zrand[i] <- (Zs[sample(5,1)]>qnorm(0.95))

  # Reject zmax if zmax>1.645
  jmax <- which(Zs==max(Zs))
  Reject_zmax[i] <- (Zs[jmax]>qnorm(0.95))

  # Taylor method: conditional p-value given that jmax is selected
  # Uncomment the following codes when you do part b

  # Compute Gamma and u
  Gamma <- matrix(0, nrow = 5, ncol = 5)
  Gamma[, jmax] <- 1
  for(cur_col in seq(1, length(Zs))) {
    if(cur_col != jmax) {
      Gamma[cur_col, cur_col] <- -1
    }
  }
  Gamma <- Gamma[-jmax, ]
  u <-  rep(0, 5)
  u <- u[-jmax]

  # Compute Vup, Vlo
  v <- rep(0,5)
  v[jmax] <- 1
  rho <- (Gamma%*%v)/(sum(v^2))

  if (sum(rho<0)==0){
    Vup <- Inf
  } else {
    Vup <- min( (u - Gamma %*% Zs + rho %*% t(v) %*% Zs ) / rho )
  }
  if (sum(rho>0)==0){
    Vlo <- -Inf
  } else {
    Vlo <- max( (u - Gamma %*% Zs + rho %*% t(v) %*% Zs ) / rho)
  }
  Reject_taylor[i] <- (Ffun(Vlo,Vup,0,1,Zs[jmax])>0.95)
}

print(c(mean(Reject_zrand),mean(Reject_zmax),mean(Reject_taylor)))


# find mean
reject_taylor_list <- NULL
for(j in 1:200){
for (i in 1:numSim){
  # Generate 5 random variables from N(0,1)
  Zs <- rnorm(5)

  # Reject zrand if zrand>1.645
  Reject_zrand[i] <- (Zs[sample(5,1)]>qnorm(0.95))

  # Reject zmax if zmax>1.645
  jmax <- which(Zs==max(Zs))
  Reject_zmax[i] <- (Zs[jmax]>qnorm(0.95))

  # Taylor method: conditional p-value given that jmax is selected
  # Uncomment the following codes when you do part b

  # Compute Gamma and u
  Gamma <- matrix(0, nrow = 5, ncol = 5)
  Gamma[, jmax] <- 1
  for(cur_col in seq(1, length(Zs))) {
    if(cur_col != jmax) {
      Gamma[cur_col, cur_col] <- -1
    }
  }
  Gamma <- Gamma[-jmax, ]
  u <-  rep(0, 5)
  u <- u[-jmax]

  # Compute Vup, Vlo
  v <- rep(0,5)
  v[jmax] <- 1
  rho <- (Gamma%*%v)/(sum(v^2))

  if (sum(rho<0)==0){
    Vup <- Inf
  } else {
    Vup <- min( (u - Gamma %*% Zs + rho %*% t(v) %*% Zs ) / rho )
  }

  if (sum(rho>0)==0){
    Vlo <- -Inf
  } else {
    Vlo <- max( (u - Gamma %*% Zs + rho %*% t(v) %*% Zs ) / rho)
  }
  Reject_taylor[i] <- (Ffun(Vlo, Vup, 0, 1, Zs[jmax]) > 0.95)
}

#print(c(mean(Reject_zrand),mean(Reject_zmax),mean(Reject_taylor)))
reject_taylor_list[j] <- mean(Reject_taylor)
}

print(mean(reject_taylor_list))

# other method
numSim <- 500  # number of simulations
Reject_zrand <- rep(0,numSim)
Reject_zmax <- rep(0,numSim)
Reject_taylor <- rep(0,numSim)

reject_taylor_list <- NULL
for(j in 1:500){
for (i in 1:numSim){
  # Generate 5 random variables from N(0,1)
  Zs <- rnorm(5)

  # Reject zrand if zrand>1.645
  Reject_zrand[i] <- (Zs[sample(5,1)]>qnorm(0.95))

  # Reject zmax if zmax>1.645
  jmax <- which(Zs==max(Zs))
  Reject_zmax[i] <- (Zs[jmax]>qnorm(0.95))

  # Taylor method: conditional p-value given that jmax is selected
  # Uncomment the following codes when you do part b

  # Compute Gamma and u
  Gamma <- matrix(0, nrow = 5, ncol = 5)
  Gamma[, jmax] <- 1
  for(cur_col in seq(1, length(Zs))) {
    if(cur_col != jmax) {
      Gamma[cur_col, cur_col] <- -1
    }
  }
  #Gamma <- Gamma[-jmax, ]
  Gamma[jmax,] <- 0
  u <-  rep(0, 5)
  # print(jmax)
  # print(Gamma)
  # Compute Vup, Vlo
  v <- rep(0,5)
  v[jmax] <- 1
  rho <- (Gamma %*% v) / (sum(v ^ 2))

  if (sum(rho < 0) == 0){
    Vup <- Inf
  } else {
    Vup <- which.max( ((u - Gamma %*% Zs + rho %*% t(v) %*% Zs ) / rho )[-jmax])
  }

  if (sum(rho>0)==0){
    Vlo <- -Inf
  } else {
    Vlo <-  which.min(((u - Gamma %*% Zs + rho %*% t(v) %*% Zs ) / rho)[-jmax])
  }
  Reject_taylor[i] <- (Ffun(Vlo,Vup,0,1,Zs[jmax])>0.95)
}

#print(c(mean(Reject_zrand),mean(Reject_zmax),mean(Reject_taylor)))
reject_taylor_list[j] <- mean(Reject_taylor)
}

print(mean(reject_taylor_list))

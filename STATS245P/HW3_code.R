# For delta method, see the hand-written notes for Week 6
# For HPD credible set, see pages 31-33 of the hand-written notes

# Constants
THETA_MAX = 1000
THETA_MIN = -1000

# Cauchy density with scale = 1
# Likelihood function
# For example:
xvec <- c(4, 5.5, 7.5, 4.5, 3)
theta <- 1
Likelihood(theta,xvec)
Likelihood <- function(theta, xvec){
  n <- length(xvec)
  Likelihood <- 1
  for (i in 1:n){
    Likelihood <- Likelihood / (1 + (theta - xvec[i]) ^ 2)
    print(Likelihood)
  }
  return(Likelihood)
}

# Part a
# first derivative of the log-likelihood
dlogL <- function(theta,xvec){
  n <- length(xvec)
  dlogL <- 0
  for (i in 1:n){
    dlogL <- dlogL + 2*(xvec[i] - theta)/ (1 + (xvec[i]-theta)**2)
  }
  return(dlogL)
}

# second derivative of the log-likelihood
d2logL <- function(theta,xvec){
  n <- length(xvec)
  d2logL <- 0
  for (i in 1:n){
    alpha <- theta - xvec[i]
    d2logL <- d2logL - 4 * alpha * (alpha **2 - alpha + 1) / ( (1 + alpha ** 2) ** 2)
  }
  return(d2logL)
}

# Part b
# Given the mle of theta, theta.mle,
# estimate the confidence interval of theta by delta method
# For example
xvec <- c(4, 5.5, 7.5, 4.5, 3)
f <- function(theta) dlogL(theta,xvec)
theta.mle <- uniroot(f,c(THETA_MIN,THETA_MAX))$root
# ConfInterval(0.05,xvec,theta.mle) 

ConfInterval <- function(alpha, xvec, theta.mle){
  # don't need delta method, per office hours
  zscore <- qnorm(1 - alpha / 2)
  sigma <-  1 / sqrt(d2logL(theta.mle, xvec))
  lower <-  theta.mle - zscore * sqrt ( sigma / length(xvec))
  upper <-  theta.mle + zscore * sqrt ( sigma / length(xvec))
  return(c(lower,upper))
}

# Part c: HPD credible set
# Likelihood of theta when xvec is given 
L.theta <- function(theta){
  return(Likelihood(theta,xvec))
} 

# Integration of Likelihood function
# The denominator of the posterior density
Integral <- function(xvec){
  Integral <- integrate(L.theta, 0, Inf)
  return(Integral$value)
}

# Given k, find the solutions (assumed to be only 2) of 
# posterior density function = k
# posterior probability of a set C
require(rootSolve)
P.C <- function(k,xvec,theta.mle){
  denom <- Integral(xvec)
  post.density <- function(theta) L.theta(theta)/denom
  f <- function(theta) post.density(theta)  - k
  lower <- uniroot(f,c(THETA_MIN,theta.mle))$root
  upper <- uniroot(f,c(theta.mle,THETA_MAX))$root
  Integral <- integrate(post.density, lower, upper)
  return(list(prob=Integral$value,interval=c(lower,upper)))
}

CredibleSet <- function(alpha,xvec,theta.mle){
  ChooseK <- function(k){
    # Your code here
    # Find theta such that pi-theta is the highest 1-alpha proportion 
    # such that pi-theta is biggest
    
    # can use posterior probability in chooseK
    # Find alpha quantile of posterior probability distribution
    # Find theta that has the largest posterior probability
    # Contrained to theta that has the probability larger than 1 - alpha
    
    # 2 approaches: frequentist fixed theta and find probability
    # Here is Bayesian, theta underlying parameter has an underlying distribution of its own
    # Here think of theta after you see the data
    # Find region of theta that has largest density, and that region has probabiliy 1 - alpha
    
    # Use P.C. function to choose K.
    # For loop go through a bunch of options for K under we find it
    # P.C. returns the integral probability
    
    # Just one line of code to fill in for choose K
    # No for loop
    # Make new function that takes param K, and in the new function, you call the function PC
    # We only want the probability from the list
    # Gives probability theta > k
    # Need to substract by 1 - alpha
    z <- P.C(k, xvec, theta.mle)
    return(z$prob - (1 - alpha))
  } 
  eps <- 1/1000000000
  lower <- eps
  upper <- L.theta(theta.mle) / Integral(xvec) - eps
  k.alpha <- uniroot(ChooseK, c(lower, upper))$root
  return(P.C(k.alpha, xvec, theta.mle)$interval)
}

# Part d
xvec <- c(4, 5.5, 7.5, 4.5, 3)
alpha <- 0.05
theta <- 1
theta.mle <- uniroot(f,c(THETA_MIN,THETA_MAX))$root
# Compute 95% HPD credible set 

zz <- CredibleSet(alpha, xvec, theta.mle)
print(zz)

# Compute 95% confidence interval
ConfInterval(alpha, xvec, theta.mle)




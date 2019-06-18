require("MASS")
require("data.table")

numSample <- 20
p <- 8 # number of predictors

AIC <- function(y, X) {
  index.all <- 1 : ncol(X)
  Xdata <- data.table(X) # convert matrix X to data.frame object
  null = lm(y ~ -1, data = Xdata)
  full = lm(y ~ . -1, data = Xdata)
  model.aic = step(null, scope = list(lower = null, upper = full), direction = "forward", trace = 0)
  B = coefficients(model.aic)
  index.select <- index.all[names(Xdata) %in% names(B)]
  return(list(index.select = index.select, beta.select = B))
}

DoVariableSelection <- TRUE

# create vector v first four entries are one and second four are zeros
v <- c(rep(1, 4), rep(0, 4))

# create column vector of decreasing values (0.8, ..., 0.1)
beta <- matrix(seq(0.8, 0.1, by = -0.1), ncol = 1)

theta <- sum(v * beta)

numSimulations = 200;
Accepts <- rep(0, numSimulations)

for (i in 1:numSimulations) {

  # Generate X and y for linear regresssion

  # column vector of zeros
  mu <- matrix(rep(0, p), ncol = 1)

  # row vector of six zeros and a 0.8
  m <- c(rep(0, p-2), 0.8)

  # is this some random sigma that was made up?
  SIGMA <- diag(p) + matrix(c(rep(0, 7), 0.8, rep(m,7), rep(0,7)), ncol = p)


  X <- mvrnorm(numSample, mu, SIGMA)
  y <- X %*% beta + rnorm(numSample)

  if (DoVariableSelection){
    model.select <- AIC(y,X)
    index.select <- model.select$index.select
    beta.select <- model.select$beta.select
    betahat <- rep(0,p)
    betahat[index.select] <- beta.select
    XS <- X[ , index.select]
    vs <- v[index.select]
    if (length(index.select) == 0) next  # for the case that no variables are selected
  } else {

    betahat <- solve(t(X)%*%X,t(X)%*%y)
    XS <- X
    vs <- v
  }

  thetahat <- sum(v * betahat)
  sd <- sqrt(sum(vs * solve(t(XS) %*% XS, vs)))
  lower <- thetahat + qnorm(0.025) * sd
  upper <- thetahat + qnorm(0.975) * sd
  Accepts[i] = (theta >= lower & theta <= upper)
}

print(mean(Accepts))



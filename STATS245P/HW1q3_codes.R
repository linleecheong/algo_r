require(data.table)
address.head <- 'http://www.stanford.edu/~xing/statfinbook/_BookData/'
address.tail <- 'Chap01/w_logret_3automanu.txt'
address <- paste(address.head,address.tail,sep='')
DT <- fread(address)

n <- nrow(DT)
model <- lm(GM~Toyota+Ford,DT)
summary(model)
beta.hat <- model$coefficients

# Your codes for computing 95% confidence intervals
confint(model, 'Toyota', 0.95)
confint(model, 'Ford', 0.95)
confint(model, '(Intercept)', 0.95)

# part b
X <- cbind(rep(1,n),as.matrix(DT[,2:3,with=FALSE]))
y <- as.matrix(DT$GM)
Xbeta.hat <- X%*%beta.hat
eps.hat <- y - Xbeta.hat
s2 <- sum(eps.hat^2)/(n - 3) # sample variance
C <- solve(t(X)%*%X)
B <- 1000
teststat.stars <- matrix(0,B,3)
beta.stars <- matrix(0, B, 3)
for (b in 1:B){
  eps.star <- sample(eps.hat,n,replace=TRUE)
  y.star <- Xbeta.hat + eps.star
  beta.star <- C %*% (t(X)%*%y.star)
  s2.star <- sum(eps.star^2)/(n - 3)
  teststat.stars[b,] <- (beta.star - beta.hat)/sqrt(s2.star*diag(C))
  # bootstrap directly beta.stars
  beta.stars[b, ] <- beta.star
}

# Your codes here for computing 95% confidence intervals
# directly bootstrap beta.star
apply(beta.stars, 2, FUN = function(x) quantile(x, probs = c(0.025, 0.975)))
# fail to reject H0 for beta0 and betaToyota, and accept for betaFord, since
# betaFord 95% confidence interval does not include 0.

quantile_t_star <- apply(teststat.stars, 2, FUN = function(x) quantile(x, probs = c(0.025, 0.975)))
quantile_b_star <- apply(beta.stars, 2, FUN = function(x) quantile(x, probs = c(0.025, 0.975)))
beta.hat + (beta.hat)*sqrt(n)/quantile_t_star[2, ]
beta.hat - (beta.hat)*sqrt(n)/quantile_t_star[1, ]

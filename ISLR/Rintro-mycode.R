set.seed(2016)
#Problem 1

N=100
beta0=25
beta1=2
X1=rnorm(n=N,mean=20,sd=3)
hist(X)
error=rnorm(n=N,mean=0,sd=15)
Y=beta0+beta1*X1+error
plot(X1,Y)
abline(beta0,beta1,col='red')

#Problem 2
fit=lm(Y~X1)
summary(fit)
abline(fit,col='blue',lwd=3)

#Problem 3
set.seed(2016)
beta1.est=double(N)

for(i in 1:1000){
  X1=rnorm(n=N,mean=20,sd=3)
  error=rnorm(n=N,mean=0,sd=15)
  Y=beta0+beta1*X1+error
  fit=lm(Y~X1)
  beta1.est[i]=coef(fit)[2]
}
print(sd(beta1.est))

#Problem 5
beta2=1
set.seed(109)
X1=rnorm(N,mean=20,sd=3)
X2=rnorm(N,mean=20,sd=3)
error=rnorm(N,mean=0,sd=15)
Y=beta0+beta1*X1+beta2*X2+error
fit=lm(Y~X1)
fit5=lm(Y~X1+X2)
cor(X1,X2)

X1=rnorm(N,mean=20,sd=3)
Z=rnorm(N,mean=20,sd=3)
X2=0.7*X1+Z
cor(X1,X2)
Y=beta0+beta1*X1+beta2*X2+error
fit=lm(Y~X1)
fit6=lm(Y~X1+X2)
summary(fit6)

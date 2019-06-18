require(ISLR)
require(boot)

?cv.glm
plot(mpg~horsepower,data=Auto)

#LOOCV
glm.fit<-glm(mpg~horsepower,data=Auto)
cv.glm(Auto,glm.fit)$delta #pretty slow, brute force 
#first is raw leave-one-out, second is bias-corrected version of it (training set is slightly smaller)

##let's write a simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2) #element by element division
}

## Now we try it out
loocv(glm.fit)

#vector to collect errors
cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d]=loocv(glm.fit)
}

plot(degree,cv.error,type='b')

##10-fold CV
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type='b',col='red')

## Bootstrap
#Sampling version of statistics, when it is really hard to develop theoretical versions
#Standard error of alpha
#Section 5.2 example: non-linear formula for picking optimal combination of two investments

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X,Portfolio$Y)

#What is the SE of alpha?
alpha.fn=function(data,index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

#confirm we get back the same number
set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

load("5.R.RData")
linreg<-lm(Y~X1+X2,data=Xy)

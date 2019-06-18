library(MASS)
#4question b
par(mfrow=c(2,2))
plot(indus,tax,main='plot of industry vs tax in Boston dataset',col='purple',pch=16)
plot(nox,age,main='plot of nox vs age in Boston dataset',col='purple',pch=16)
plot(ptratio, rad,main='plot of ptratio vs age in Boston dataset',col='purple',pch=16)
plot(dis,lstat,main='plot of dis vs lstat in Boston dataset',col='purple',pch=16)

#4c
par(mfrow=c(2,2))
plot(zn,crim,col='purple',pch=16) #,main='average higher crime when zn is close to zero')
plot(indus,crim,col='purple',pch=16)#,main='average higher crime if indus is around 17 to 19')
plot(chas,crim,col='purple',pch=16)#,main='average higher crime if chas is 0')
plot(nox,crim,col='purple',pch=16)
plot(rm,crim,col='purple',pch=16)
plot(age,crim,col='purple',pch=16) #relationship
plot(dis,crim,col='purple',pch=16)#relationship
plot(rad,crim,col='purple',pch=16)#relationship
plot(tax,crim,col='purple',pch=16)#relationship
plot(ptratio,crim,col='purple',pch=16)
plot(black,crim,col='purple',pch=16)
plot(lstat,crim,col='purple',pch=16)#relationship
plot(medv,crim,col='purple',pch=16)#relationship

#4d
par(mfrow=c(1,1))
plot(crim)
which.max(crim)
plot(tax)
higher.tax.first <- order(tax,decreasing=TRUE)
tax[higher.tax.first]
plot(ptratio)

#Homework 1 Question 5a
library(caTools)
set.seed(2016)
spl<-sample.split(Boston$crim, SplitRatio=0.5)
train<-subset(Boston,spl==TRUE)
test<-subset(Boston,spl==FALSE)

#fit model
fit<-lm(crim~.-Y,data=train)
#predict model
train.predicted<-predict(fit)
test.predicted<-predict(fit,newdata=test)
#sum of squared errors
train.sse<-(train.predicted-train$crim)^2
test.sse<-(test.predicted-test$crim)^2
#rmse=sqrt(sse/N)
train.rmse<-sqrt(sum(train.sse)/length(train.sse))
test.rmse<-sqrt(sum(test.sse)/length(test.sse))

#homework 1 Question 6
#find the median crime
summary(crim)
#generate the variable in both training and test data
train$Y<-ifelse(train$crim>0.25650,1,0)
test$Y<-ifelse(test$crim>0.25650,1,0)
#fit the model
logfit<-glm(Y~.-crim,data=train,family='binomial')
#check for predictor importance
summary(logfit)
#generate predictions on training data
logfit.pred<-predict(logfit,type='response')
table(train$Y,logfit.pred>0.5)
mean((logfit.pred>0.5)!=train$Y)
#generate predictions for test data
logfit.test.pred<-predict(logfit,newdata=test,type='response')
table(test$Y,logfit.test.pred>0.5)
mean((logfit.test.pred>0.5)!=test$Y)

library(ISLR)
names(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family='binomial')
summary(glm.fit)
#no variables are significant
#suggests that the variables are correlated
#does not mean we wont' have reasonable predictions
#Null deviance=deviance from mean
#Residual deviance=what's unexplained from model, which is not much here
glm.probs<-predict(glm.fit,type='response')
glm.probs[1:5]
glm.pred<-ifelse(glm.probs>0.5,'Up','Down')
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)

#Make training and testing set
train=Year<2005
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family='binomial',subset=train)
glm.probs<-predict(glm.fit,newdata=Smarket[!train,],type='response')
glm.pred<-ifelse(glm.probs>0.5,'Up','Down')
Direction.2005<-Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#fit a smaller model
train=Year<2005
glm.fit<-glm(Direction~Lag1+Lag2,data=Smarket,family='binomial',subset=train)
glm.probs<-predict(glm.fit,newdata=Smarket[!train,],type='response')
glm.pred<-ifelse(glm.probs>0.5,'Up','Down')
Direction.2005<-Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#smaller model appears to have improved by reducing overfitting
summary(glm.fit)

################################
#LINEAR DISCRIMINANT ANALYSIS
################################
library(ISLR)
library(MASS)

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
lda.fit
#prior probabilities=proportions of up and downs in the dataset
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
#lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

############################################################
#K-Nearest-Neighbor classification
###########################################################
library(class)
?knn
attach(Smarket)
ls()
objects(2)
Xlag=cbind(Lag1,Lag2)
Xlag[1:5,]
train=Year<2005
knn.pred<-knn(Xlag[train,],Xlag[!train,],Direction[train],k=2)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

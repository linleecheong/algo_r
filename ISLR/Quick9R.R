error<-rep(0,100)

for(currentIter in 1:100){
x1_1=matrix(rnorm(1000*5,mean = 1),1000,5)
x1_0=matrix(rnorm(1000*5,mean = 0),1000,5)
x1<-cbind(x1_1,x1_0)
x1<-data.frame(x1,y=1)
x0=matrix(rnorm(1000*10),1000,10)
x0<-data.frame(x0,y=0)
train1<-sample(1:nrow(x1),size = 50)
test1<-(1:nrow(x1))[-train1]
train0<-sample(1:nrow(x0),size = 50)
test0<-(1:nrow(x0))[-train0]

library(e1071)
trainDF<-data.frame(rbind(x1[train1,],x0[train0,]))
testDF<-data.frame(rbind(x1[-train1,],x0[-train0,]))
trainDF$y<-as.factor(trainDF$y)
testDF$y<-as.factor(testDF$y)
#svmfit<-svm(y~.,data=trainDF,kernel='linear')
logfit<-glm(y~.,data=trainDF,family='binomial')
ypred<-predict(svmfit,newdata=testDF,type='class')
results<-table(testDF$y,ypred)
error[currentIter]<-(results[1,2]+results[2,1])/nrow(testDF)
}
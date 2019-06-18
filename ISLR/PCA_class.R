meaload("C:/Users/licheong/Documents/Classes_ISLR/body.RData")

par(mfrow=c(2,3),ask=TRUE)
vnames=colnames(X)
for(i in vnames)hist(X[,i],main=i,xlab="")

vnames=colnames(Y)
for(i in vnames)hist(Y[,i],main=i,xlab="")

par(mfrow=c(1,1),ask=FALSE)
summary(Y$Gender)
hist(cor(X))

with(Y, plot(Weight, Height, 
             main = "How many dimensions?",
             asp = 1))

mypca=princomp(Y[,2:3])
with(mypca,{
  p1 = center - loadings[,1]*sdev[1]
  p2 = center + loadings[,1]*sdev[1]
  lines(c(p1[1],p2[1]), c(p1[2],p2[2]), col = 2, lwd = 3)
  p1 = center - loadings[,2]*sdev[2]
  p2 = center + loadings[,2]*sdev[2]
  lines(c(p1[1],p2[1]), c(p1[2],p2[2]), col = 2, lwd = 3)})

plot(-mypca$scores[,1],-mypca$scores[,2])

mypca$sdev^2/sum(mypca$sdev^2)



####SCALED VERSION
var(Y$Height)
var(Y$Weight)
mypca=princomp(scale(Y[,2:3]))
mypca$loadings
mypca$sdev^2/sum(mypca$sdev^2)

###PROBLEM#3
xpca=princomp(scale(X))
plot(xpca,main='Scree plot')
round(xpca$loadings[,1:4],2)
xpca$loadings
xpca$sdev^2/sum(xpca$sdev^2)

####PROBLEM#4
set.seed(1337)
test=sort(sample(1:nrow(X),200))
train=(1:nrow(X))[-test]
mydf<-data.frame(gender=Y$Gender,x=xpca$scores[,1:4])
pcr4=glm(gender~.,family=binomial,subset=train,data=mydf)
summary(pcr4)

table(ifelse(mydf$gender[test], 
             "True Male", "True Female"),
      ifelse(predict(pcr4, newdata=mydf[test,], type="response") > .5,
             "Predicted Male", "Predicted Female"))
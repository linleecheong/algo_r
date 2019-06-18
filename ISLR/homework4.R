x1<-c(3,2,4,1,2,4,4)
x2<-c(4,2,4,4,1,3,1)
y<-c('red','red','red','red','blue','blue','blue')

plot(x1,x2,col=y,pch=19)
grid()
abline(-0.5,1,col='green')

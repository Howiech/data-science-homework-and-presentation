dat=read.csv("/Users/underdog/Downloads/AlbumSales.csv")
y=dat[,1]
x1=dat[,2]
x2=dat[,3]
x3=dat[,4]
eq1=as.formula(Sales~Adv)
ID <- seq_len(length(y))
k = 10
fold =sample(rep(1:k, length.out = length(y)))

cv.errors=NULL
for (i in 1:k) { #i=1
  
  testset <- ID[fold == i]
  trainset <- ID[fold != i]
  trainmodel <- lm(eq1, data=dat[trainset,])
  test.fit= predict(trainmodel,newdata=dat[testset,])
  test.errors=dat[testset,1]-test.fit
  
  cv.errors=c(cv.errors,test.errors)
  
}
show(cv.errors)
KFOLDMAPE=mean(abs(cv.errors/y))
show(KFOLDMAPE)


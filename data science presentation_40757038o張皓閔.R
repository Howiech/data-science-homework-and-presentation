#資料科學presentation_0427

#1. Explain what are you going to do to extract the 10-fold predictions for K-Fold CV.
library(caret)
source("/NTNUBA/R scripts/src/trainingSamples.R")
dataset=read.csv("/NTNUBA/R scripts/data/CCS.csv",stringsAsFactors = TRUE)

dataset$subSample = trainingSamples(dataset, Training=0.7, Validation=0.3)
table(dataset$subSample)
EQ=as.formula(MonthGive ~ DonPerYear + AveDonAmt + AveIncEA + SomeUnivP+LastDonAmt+YearsGive)
methods=c("glm","rpart","rf","svmRadial","nnet")

output=train(EQ,
             data=dataset, 
             subset=subSample=="Training",
             method=methods[1],
             trControl = trainControl(method = c("cv","boot")[1],
                                      #number=10,
                                      savePredictions =TRUE)
)
dataValid=subset(dataset,subSample=="Validation") 

dataTrain=subset(dataset,subSample=='Training')

pred =predict(output,newdata=dataValid,type=c("raw","prob")[1])



#1.Explain what are you going to do to extract the 10-fold predictions for K-Fold CV.
######
#glm
output$pred
output$bestTune
p1 <- output$pred
bt1 <- output$bestTune
o1=subset(p1, p1[,4]==bt1[1,1])#result
oo1 <- order(o1$rowIndex)

#rpart
output.rpart.cv=train(EQ,
                      data=dataset, 
                      subset=subSample=="Training",
                      method=methods[2],
                      trControl = trainControl(method = c("cv","boot")[1],
                                      #number=10,
                                      savePredictions =TRUE)
)
p2 <- output.rpart.cv$pred
bt2 <- output.rpart.cv$bestTune
o2=subset(p2, p2[,4]==bt2[1,1])#result
oo2 <- order(o2$rowIndex)
newData.2.cv=o2[oo2,]
rownames(newData.2.cv)=seq(nrow(newData.2.cv))
#rf
output.rf.cv=train(EQ,
                   data=dataset, 
                   subset=subSample=="Training",
                   method=methods[3],
                   trControl = trainControl(method = c("cv","boot")[1],
                                      #number=10,
                                      savePredictions =TRUE)
)
p3 <- output.rf.cv$pred
bt3 <-output.rf.cv$bestTune
o3=subset(p3, p3[,4] == bt3[1,1])

#svmRadial
output.svm.cv=train(EQ,
                   data=dataset, 
                   subset=subSample=="Training",
                   method=methods[4],
                   trControl = trainControl(method = c("cv","boot")[1],
                                      #number=10,
                                      savePredictions =TRUE)
)

p4 <- output.svm.cv$pred
bt4 <- output.svm.cv$bestTune
o4=subset(p4, p4[,4] == bt4[1,1]& p4[,5] == bt4[1,2])

#nnet
output.nnet.cv=train(EQ,
                     data=dataset, 
                     subset=subSample=="Training",
                     method=methods[5],
                     trControl = trainControl(method = c("cv","boot")[1],
                                      #number=10,
                                      savePredictions =TRUE)
)
p5 <- output.nnet.cv$pred
bt5 <- output.nnet.cv$bestTune
o5=subset(p5, p5[,4] == bt5[1,1] & p5[,5] == bt5[1,2])

#####
#2. Explain what are you going to do to extract the predictions for boot.
#####
#glm
output.boot=train(EQ,
                  data=dataset, 
                  subset=subSample=="Training",
                  method=methods[1],
                  trControl = trainControl(method = c("cv","boot")[2],
                                      #number=10,
                                      savePredictions =TRUE)
)
pr1 <- output.boot$pred
best1 <- output.boot$bestTune
bo1 <- pr1[!duplicated(pr1[,3]),]

#rpart
output.rpart.boot=train(EQ,
                        data=dataset, 
                        subset=subSample=="Training",
                        method=methods[2],
                        trControl = trainControl(method = c("cv","boot")[2],
                                              #number=10,
                                               savePredictions =TRUE)
)
pr2 <- output.rpart.boot$pred
best2 <- output.rpart.boot$bestTune
tenbo2 <- pr2[!duplicated(pr2[,c(3,4)]),]
bo2 = subset(tenbo2, tenbo2[,4] == best2[1,1])

#rf
output.rf.boot=train(EQ,
                     data=dataset, 
                     subset=subSample=="Training",
                     method=methods[3],
                     trControl = trainControl(method = c("cv","boot")[2],
                                      #number=10,
                                      savePredictions =TRUE)
)
pr3 <- output.rf.boot$pred
best3 <- output.rf.boot$bestTune
tenbo3 <- pr3[!duplicated(pr3[,c(3,4)]),]
bo3 = subset(tenbo3, tenbo3[,4] == best3[1,1])

#svm
output.svm.boot=train(EQ,
                      data=dataset, 
                      subset=subSample=="Training",
                      method=methods[4],
                      trControl = trainControl(method = c("cv","boot")[2],
                                      #number=10,
                                      savePredictions =TRUE)
)
pr4 <- output.svm.boot$pred
best4 <- output.svm.boot$bestTune
tenbo4 <- pr4[!duplicated(pr4[,c(3,4,5)]),]
bo4 = subset(tenbo4, tenbo4[,4] == best4[1,1] & tenbo4[,5] == best4[1,2])
duplicated(bo4[,3])
which(duplicated(bo4$rowIndex))
length(which(duplicated(bo4$rowIndex)))
#nnet
output.nnet.boot=train(EQ,
                       data=dataset, 
                       subset=subSample=="Training",
                       method=methods[5],
                       trControl = trainControl(method = c("cv","boot")[2],
                                      #number=10,
                                      savePredictions =TRUE)
)
pr5 <- output.nnet.boot$pred
best5 <- output.nnet.boot$bestTune
tenbo5 <- pr5[!duplicated(pr5[,c(3,4,5)]),]
bo5 = subset(tenbo5, tenbo5[,4] == best5[1,1] & tenbo5[,5] == best5[1,2])

#####
#3. Given SVM, compare the performance of two re-sampling method: "cv" vs. "boot". 
#   Using both training data and validation to show your results.
#####

#training

CFMatrix.tab.svm.train.cv  =table(Pred = o4[,1], Actual = o4[,2])
CFMatrix.tab.svm.train.boot=table(Pred = bo4[,1], Actual=bo4[,2])
confusionMatrix(CFMatrix.tab.svm.train.cv, positive=c("No","Yes")[2])   #accuracy:0.7188 kappa:0.4375
confusionMatrix(CFMatrix.tab.svm.train.boot, positive=c("No","Yes")[2]) #accuracy:0.7036 kappa0.4072

#valid
pred.svm.cv = predict(output.svm.cv,newdata = dataValid,type=c('raw','prob')[1])
pred.svm.boot=predict(output.svm.boot,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.svm.dv.cv=table(Pred =pred.svm.cv, Actual = dataValid[,"MonthGive"])
CFMatrix.tab.svm.dv.boot=table(Pred = pred.svm.boot, Actual = dataValid[,'MonthGive'])
confusionMatrix(CFMatrix.tab.svm.dv.cv, positive=c("No","Yes")[2])   #accuracy:0.7146 kappa:0.4297
confusionMatrix(CFMatrix.tab.svm.dv.boot, positive=c("No","Yes")[2]) #accuracy:0.7167 kappa:0.4338

#####
#4. Among the above-mentioned five methods, do  "cv" and "boot" select the same model?
#####
#glm
CFMatrix.tab.glm.train.cv =table(Pred = o1[,1], Actual = o1[,2])
CFMatrix.tab.glm.train.boot=table(Pred = bo1[,1], Actual = bo1[,2])
pred.glm.cv= predict(output,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.glm.dv.cv =table(Pred = pred.glm.cv, Actual = dataValid[,"MonthGive"])
pred.glm.boot= predict(output.boot,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.glm.dv.boot=table(Pred=pred.glm.boot,Actual= dataValid[,"MonthGive"])

confusionMatrix(CFMatrix.tab.glm.train.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.glm.train.boot, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.glm.dv.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.glm.dv.boot, positive=c("No","Yes")[2])

#rpart
CFMatrix.tab.rpart.train.cv =table(Pred = o2[,1], Actual = o2[,2])
CFMatrix.tab.rpart.train.boot =table(Pred = bo2[,1], Actual = bo2[,2])
pred.rpart.cv= predict(output.rpart.cv,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.rpart.dv.cv =table(Pred = pred.rpart.cv, Actual = dataValid[,"MonthGive"])
pred.rpart.boot= predict(output.rpart.boot,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.rpart.dv.boot=table(Pred=pred.rpart.boot,Actual= dataValid[,"MonthGive"])

confusionMatrix(CFMatrix.tab.rpart.train.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.rpart.train.boot, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.rpart.dv.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.rpart.dv.boot, positive=c("No","Yes")[2])

#rf
CFMatrix.tab.rf.train.cv =table(Pred = o3[,1], Actual = o3[,2])
CFMatrix.tab.rf.train.boot =table(Pred = bo3[,1], Actual = bo3[,2])
pred.rf.cv= predict(output.rf.cv,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.rf.dv.cv =table(Pred = pred.rf.cv, Actual = dataValid[,"MonthGive"])
pred.rf.boot= predict(output.rf.boot,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.rf.dv.boot=table(Pred=pred.rf.boot,Actual= dataValid[,"MonthGive"])

confusionMatrix(CFMatrix.tab.rf.train.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.rf.train.boot, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.rf.dv.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.rf.dv.boot, positive=c("No","Yes")[2])

#svm
CFMatrix.tab.svm.train.cv  =table(Pred = o4[,1], Actual = o4[,2])
CFMatrix.tab.svm.train.boot=table(Pred = bo4[,1], Actual=bo4[,2])
pred.svm.cv = predict(output.svm.cv,newdata = dataValid,type=c('raw','prob')[1])
pred.svm.boot=predict(output.svm.boot,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.svm.dv.cv=table(Pred =pred.svm.cv, Actual = dataValid[,"MonthGive"])
CFMatrix.tab.svm.dv.boot=table(Pred = pred.svm.boot, Actual = dataValid[,'MonthGive'])

confusionMatrix(CFMatrix.tab.svm.train.cv, positive=c("No","Yes")[2])   #accuracy:0.7188 kappa:0.4375
confusionMatrix(CFMatrix.tab.svm.train.boot, positive=c("No","Yes")[2]) #accuracy:0.7036 kappa0.4072
confusionMatrix(CFMatrix.tab.svm.dv.cv, positive=c("No","Yes")[2])   #accuracy:0.7146 kappa:0.4297
confusionMatrix(CFMatrix.tab.svm.dv.boot, positive=c("No","Yes")[2]) #accuracy:0.7167 kappa:0.4338

#nnet
CFMatrix.tab.nnet.train.cv =table(Pred = o5[,1], Actual = o5[,2])
CFMatrix.tab.nnet.train.boot =table(Pred = bo5[,1], Actual = bo5[,2])
pred.nnet.cv= predict(output.nnet.cv,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.nnet.dv.cv =table(Pred = pred.nnet.cv, Actual = dataValid[,"MonthGive"])
pred.nnet.boot= predict(output.nnet.boot,newdata=dataValid,type=c('raw','prob')[1])
CFMatrix.tab.nnet.dv.boot=table(Pred=pred.nnet.boot,Actual= dataValid[,"MonthGive"])

confusionMatrix(CFMatrix.tab.nnet.train.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.nnet.train.boot, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.nnet.dv.cv, positive=c("No","Yes")[2])
confusionMatrix(CFMatrix.tab.nnet.dv.boot, positive=c("No","Yes")[2])

#cv
confusionMatrix(CFMatrix.tab.glm.dv.cv, positive=c("No","Yes")[2])     #accuracy:0.6771 kappa:0.3518
confusionMatrix(CFMatrix.tab.rpart.dv.cv, positive=c("No","Yes")[2])   #accuracy:0.6646 kappa:0.3352
confusionMatrix(CFMatrix.tab.rf.dv.cv, positive=c("No","Yes")[2])      #accuracy:0.6688 kappa:0.3391
confusionMatrix(CFMatrix.tab.svm.dv.cv, positive=c("No","Yes")[2])     #accuracy:0.7146 kappa:0.4297
confusionMatrix(CFMatrix.tab.nnet.dv.cv, positive=c("No","Yes")[2])    #accuracy:0.6729 kappa:0.3491
#boot
confusionMatrix(CFMatrix.tab.glm.dv.boot, positive=c("No","Yes")[2])   #accuracy:0.6771 kappa:0.3518
confusionMatrix(CFMatrix.tab.rpart.dv.boot, positive=c("No","Yes")[2]) #accuracy:0.6646 kappa:0.3352
confusionMatrix(CFMatrix.tab.rf.dv.boot, positive=c("No","Yes")[2])    #accuracy:0.6792 kappa:0.3599
confusionMatrix(CFMatrix.tab.svm.dv.boot, positive=c("No","Yes")[2])   #accuracy:0.7167 kappa:0.4338
confusionMatrix(CFMatrix.tab.nnet.dv.boot, positive=c("No","Yes")[2])  #accuracy:0.4896 kappa:0
#####

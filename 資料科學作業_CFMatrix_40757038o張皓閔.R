library(rpart)
source("/NTNUBA/R scripts/src/trainingSamples.R")
dataset=read.csv("/NTNUBA/R scripts/data/Project_data.csv",stringsAsFactors = TRUE)

dataset$subSample = trainingSamples(dataset, Training=0.6, Validation=0.15)
table(dataset$subSample)

EQ=as.formula(performance_group~yrs_employed+test_score+manager_hire+group_size+customers)

#treemodel
treeModel=rpart(EQ,
                data=dataset, subset=subSample=="Training", cp=0.01)
treeModel$variable.importance

dataTest=subset(dataset,subSample=="Testing")

pred1 =predict(treeModel,newdata=dataTest,type="class")
CFMatrix.tree=table(Pred =pred1, Actual = dataTest[,"performance_group"])

#rfmodel
library(randomForest)

rfModel <- randomForest(EQ, 
                        data=dataset, subset=subSample=="Training",ntrees=500,importance = TRUE)
pred2 =predict(rfModel,newdata=dataTest,type="class")

importance(rfModel)
sort(importance(rfModel)[,3],decreasing = TRUE)

CFMatrix.rf=table(Pred =pred2, Actual = dataTest[,'performance_group'])

#nnetmodel
library(nnet)
nnetAvg = caret::avNNet(EQ,data=dataset, subset=subSample=="Training",
                        repeats = 10,
                        size = 5,
                        decay = 5e-4,
                        trace = FALSE,
                        maxit = 500, 
                        MaxNWts = 50)
pred3 =predict(nnetAvg, newdata=dataTest,type="class")
CFMatrix.nnetAvg=table(Pred =pred3 ,Acual = dataTest[,"performance_group"])

#SVM
library(kernlab)
dataTrain=subset(dataset,subSample=="Training")
svmModel <- ksvm(EQ,
                 data=dataTrain,kpar = "automatic",
                 C = 1, epsilon = 0.1)
pred4 =predict(svmModel, newdata=dataTest,type="response")
CFMatrix.svm=table(Pred =pred4 ,Actual = dataTest[,"performance_group"])
#tree
caret::confusionMatrix(CFMatrix.tree, positive="No")
caret::confusionMatrix(CFMatrix.tree, positive="Yes")
#rf
caret::confusionMatrix(CFMatrix.rf, positive="No")
caret::confusionMatrix(CFMatrix.rf, positive="Yes")
#avgnnet
caret::confusionMatrix(CFMatrix.nnetAvg, positive="No")
caret::confusionMatrix(CFMatrix.nnetAvg, positive="Yes")
#svm
caret::confusionMatrix(CFMatrix.svm, positive="No")
caret::confusionMatrix(CFMatrix.svm, positive="Yes")

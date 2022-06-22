dat=read.csv("/NTNUBA/R scripts/data/Project_data.csv", stringsAsFactors = TRUE)
head(dat)
ID=sample(1:571,470)
TrainData <- dat[ID,]
newData=dat[-ID,]
x0=TrainData[,2]
ifelse(x0=="Top",1,0)
eq1=as.formula(x0~yrs_employed+test_score+city)
output1=glm(eq1, family="binomial",data=TrainData)
summary(output1)

ifelse(x0=="Bottom",1,0)
eq2=as.formula(x0~yrs_employed+test_score+city)
output2=glm(eq2, family="binomial",data=TrainData)
summary(output2)

ifelse(x0=="Middle",1,0)
eq3=as.formula(x0~yrs_employed+test_score+city)
output3=glm(eq3, family="binomial",data=TrainData)
summary(output3)

ifelse(x0=="Top",1,0)
eq4=as.formula(x0~yrs_employed+manager_hire+test_score+group_size+concern_flag+mobile_flag+customers+high_hours_flag+transfers+reduced_schedule+city)
output4=glm(eq4, family="binomial",data=TrainData)
summary(output4)

ifelse(x0=="Top",1,0)
eq8=as.formula(x0~yrs_employed+test_score+manager_hire+group_size+transfers)
output8=glm(eq8, family="binomial",data=TrainData)
summary(output8)

ifelse(x0=="Top",1,0)
eq9=as.formula(x0~manager_hire+group_size+transfers+transfers*manager_hire+manager_hire*group_size)
output9=glm(eq9, family="binomial",data=TrainData)
summary(output9)

ifelse(x0=="Top",1,0)
eq10=as.formula(x0~manager_hire+yrs_employed+manager_hire*yrs_employed)
output10=glm(eq10, family="binomial",data=TrainData)
summary(output10)

#after importing test and training dataset
chmura<- subset(Train_final1,select = -c(MJobId,SocCode,Title,DtStart,Company))
chmura<-na.omit(chmura)
chmura_test<- subset(Test_final1,select = -c(MJobId,SocCode,Title,DtStart,Company))
chmura_test<-na.omit(chmura_test)
#making a temporary subset
chmura_temp<-subset(chmura,select=-c(Wage))
chmura_temp_1<-subset(chmura_test,select=-c(Wage))
#subsetting columns for converting to factor
col_chmura<-colnames(chmura_temp)
col_chmura_1<-colnames(chmura_temp_1)
chmura[col_chmura]<-lapply(chmura[col_chmura],factor)
chmura_test[col_chmura_1]<-lapply(chmura_test[col_chmura_1],factor)

#training model on training set
mychLR <- lm(Wage ~.-Wage, data=chmura)
out <- capture.output(summary(mychLR))
#saving training results
library(broom)
tidy_lmfit <- tidy(mychLR)
write.csv(tidy_lmfit, "regression_training.csv")
summary(mychLR)
#predicting on test results
mychLRPredict <- predict(mychLR, newdata=chmura_test, type="response")
#calculating all errors
install.packages('DMwR')
library(DMwR)
DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

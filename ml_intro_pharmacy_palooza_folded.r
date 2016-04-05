#Install libraries
library("ggplot2")
library(caTools)
library(dplyr)
library(tidyr)
#Read data

dataPath = "C:/Users/dickm/Documents/Projects/ML/Source/UPMC/Pharmacy/visit_train_panda.csv"
visits = read.csv(dataPath)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++PRE-PROCESSING DATA+++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

visits$VisitID = as.factor(visits$VisitID)
visits$Hospital = as.factor(visits$Hospital)
visits$Race = as.factor(visits$Race)
visits$Gender = as.factor(visits$Gender)
visits$DXCODE = as.factor(visits$DXCODE)
visits$FC = as.factor(visits$FC)
visits$DOC = as.factor(visits$DOC)
visits$ArriveDate = as.Date(visits$ArriveDate, "%m/%d/%Y")
visits$DischargeDate = as.Date(visits$DischargeDate, "%m/%d/%Y")
visits$ArriveDateDOW = as.factor(weekdays(visits$ArriveDate))
visits$DischargeDateDOW = as.factor(weekdays(visits$DischargeDate))


#++++++++++++++++++++Folding+++++++++++++++++++++++++++++++#
visits.selected = unique(visits[,c(-3,-4,-8,-10,-13,-14,-15,-17)]) #Remove non-feature fields.
visits.selected$HasDXCODE = 1

#For some strange reason this did the grouping.
#Removed the visitID to avoid overfit.
visits.folded = spread(visits.selected, DXCODE, HasDXCODE, fill = 0, drop = TRUE)[,-1]


#Group folded data-set 
visits.features = 
  visits.folded %>% 
  group_by(VisitID, Hospital, Age, Race, Gender, ArriveDate, DischargeDate, LOS, ArriveDateDOW, DischargeDateDOW) %>% 
  summarize_each(funs(sum))



#Verify grouping
#id which codes
tail(sort(table(visits$DXCODE)))

#250.00 is popular
visits.folded %>% filter((`250.00` > 0) & (`200.50` > 0)) %>% select(ArriveDate, Age, Gender, LOS, `250.00`, `004.1`, `200.50`, Age) %>% top_n(5)

#++++++++++++++++++++End folding+++++++++++++++++++++++++++++++#

# Split
set.seed(144)
spl = sample.split(visits.folded$LOS, 0.75)
train = subset(visits.folded, spl == TRUE)
test = subset(visits.folded, spl == FALSE)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++END PROCESSING DATA+++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++TRAIN MODELS+++++++++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#+++++++++++++++++++++MODEL 2/4 - LINEAR REGRESSION++++++++++++++++++++++++++#
fit.lm = lm(LOS~., data=train)
#RMSE
sqrt(sum((fit.lm$fitted.values- train$LOS)^2)/nrow(train))

#SST
1 - (sum((fit.lm$fitted.values - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

#Evaluate, without folding the RMSE was > 6.2
pred.lm = predict(fit.lm, newdata=test)

#RMSE
sqrt(sum((pred.lm - test$LOS)^2)/nrow(test))

#SST
1 - (sum((pred.lm - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++END TRAIN MODELS+++++++++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


#++++++++++++++++++++SAVE MODEL - 1 hour to train++++++++++++++++++++++#
# RMSE 3.4 on train. 4.7 on test
#saving model because it took a long time to build
save(fit.lm, file = 'C:/Users/dickm/Documents/Projects/ML/DevProjects/ml-upmce-palooza-r/fit.lm.rda')

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++END SAVE MODEL ++++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++EVAL MODEL ON UNSEEN++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

visits.val = read.csv("C:/Users/dickm/Documents/Projects/ML/Source/UPMC/Pharmacy/visit_test_panda_dxcode_factors.csv")

#++++++++++++++++++++PRE-PROCESSING DATA+++++++++++++++++++++++++#
visits.val$VisitID = as.factor(visits.val$VisitID)
visits.val$Hospital = as.factor(visits.val$Hospital)
visits.val$Race = as.factor(visits.val$Race)
visits.val$Gender = as.factor(visits.val$Gender)
visits.val$DXCODE = as.factor(visits.val$DXCODE)
visits.val$FC = as.factor(visits.val$FC)
visits.val$DOC = as.factor(visits.val$DOC)
visits.val$ArriveDate = as.Date(visits.val$ArriveDate, "%Y-%m-%d") # changed format of dates
visits.val$DischargeDate = as.Date(visits.val$DischargeDate, "%Y-%m-%d")  #changed the format of dates
visits.val$ArriveDateDOW = as.factor(weekdays(visits.val$ArriveDate))
visits.val$DischargeDateDOW = as.factor(weekdays(visits.val$DischargeDate))

#Get uniue DXCODE from training set 
train.DXCODES = data.frame(DXCODE=as.factor(colnames(train)[seq(8,ncol(train))]))

#Added this because I got an error with the on prediction
#Remove DXCODE's that are in test but not in train. DXCODE is sparsely populated
visits.val$DXCODE[which(!(visits.val$DXCODE %in% unique(train.DXCODES$DXCODE)))] = NA
#Add in random sample of missing data
visits.val$DXCODE[is.na(visits.val$DXCODE)] = sample(visits.val$DXCODE[!is.na(visits.val$DXCODE)], sum(is.na(visits.val$DXCODE)))


#++++++++++++++++++++Folding+++++++++++++++++++++++++++++++#
visits.val.selected = unique(visits.val[,c(-3,-4,-8,-10,-13,-14,-15,-17)]) #Remove non-feature fields.
visits.val.selected$HasDXCODE = 1

#For some strange reason this did the grouping.
#Removed the visitID to avoid overfit.
visits.val.folded = spread(visits.val.selected, DXCODE, HasDXCODE, fill = 0, drop = TRUE)[,-1]

#Initializing columns in training set but not in validatoin set
visits.val.folded.recordsToAdd = 
  train[as.character(train.DXCODES$DXCODE)][which(!(train.DXCODES$DXCODE %in% colnames(visits.val.folded)))][seq(1,nrow(visits.val.folded)),]
visits.val.folded.recordsToAdd[seq(1,ncol(visits.val.folded.recordsToAdd))] = 0
visits.val.folded.recordsToAddColNames = colnames(visits.val.folded.recordsToAdd)

#add columns and update column names
visits.val.folded = cbind(visits.val.folded, visits.val.folded.recordsToAdd)
#colnames(visits.val.folded) = c(visits.val.folded.colnames, visits.val.folded.recordsToAddColNames)


#Verify grouping
#id which codes
tail(sort(table(visits.val$DXCODE)))

#296.7 is popular
visits.val.folded %>% filter((`296.7` > 0) & (`206.90` > 0)) %>% select(ArriveDate, Age, Gender, LOS, `296.7`, `206.90`, `003.0`, Age) %>% top_n(5)

#++++++++++++++++++++END PRE-PROCESSING +++++++++++++++++++++++++#



#Evalue Method 1 - Linear model+++++++++++++++++++++++++++++++++++++++++#
#result the RMSE was over 9.00. Ouch!!!
pred.val.lm = predict(fit.lm, newdata=visits.val.folded)

#RMSE
sqrt(sum((pred.val.lm - visits.val.folded$LOS)^2)/nrow(visits.val.folded))

#SST
1 - (sum((pred.val.lm - visits.val.folded$LOS)^2)/sum((mean(visits.val.folded$LOS) - visits.val.folded$LOS)^2))






#++++++++++++++Evalue Method 5 - Linear model with CV+++++++++++++++++++++++++++++++++++++++++#
install.packages("DAAG")
library("DAAG")
ptm = proc.time()
fit.cvlm = cv.lm(data = visits.folded, m=3, form.lm = formula(LOS~.))
proc.time() - ptm

#RMSE
sqrt(sum((fit.cvlm$Predicted - visits.folded$LOS)^2)/nrow(visits.folded))

#SST
1 - (sum((fit.cvlm$Predicted - visits.folded$LOS)^2)/sum((mean(visits.folded$LOS) - visits.folded$LOS)^2))

#CV
#CV - RMSE
sqrt(sum((fit.cvlm$cvpred - train$LOS)^2)/nrow(visits.folded))

#CV - SST
1 - (sum((fit.cvlm$cvpred - train$LOS)^2)/sum((mean(visits.folded$LOS) - visits.folded$LOS)^2))

pred.cvlm = predict(fit.cvlm$Predicted, newdata=test)

#RMSE
sqrt(sum((predLMDOW - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLMDOW - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))
#++++++++++++++++++++VALIDATOIN DATA SET++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pred.val.cvlm = predict(fit.cvlm$Predicted, newdata=visits.val.folded)

#RMSE
sqrt(sum((pred.cvlm - visits.val.folded$LOS)^2)/nrow(visits.val.folded))

#SST
1 - (sum((pred.cvlm - visits.val.folded$LOS)^2)/sum((mean(visits.val.folded$LOS) - visits.val.folded$LOS)^2))



#++++++++++++++END Evalue Method 5 - Linear model with CV+++++++++++++++++++++++++++++++++++++++++#






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 6 - LASSO with or without standard set
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("glmnet")
library("glmnet")
#Set up the sparse matrices with dummy variables
#fix names priort to matrix
visits.folded.fixedNames = visits.folded
visits.val.folded.fixedNames = visits.val.folded

colnames(visits.folded.fixedNames) = make.names(colnames(visits.folded))
colnames(visits.val.folded.fixedNames) = make.names(colnames(visits.val.folded))


visits.matrix = as.matrix(sparse.model.matrix(~., visits.folded.fixedNames[-6]))
visits.val.matrix = as.matrix(sparse.model.matrix(~., visits.val.folded.fixedNames[-6]))

#Train model
ptm = proc.time()
fit.LASSO = glmnet(y = visits.folded.fixedNames$LOS, x = visits.matrix, standardize = TRUE)
proc.time() - ptm


plot(fit.LASSO)

pred.LASSO = predict(fit.cvLASSO, visits.val.matrix)

#RMSE
sqrt(sum((pred.LASSO - visits.val.folded.fixedNames$LOS)^2)/nrow(visits.val.folded.fixedNames))

#SST
1 - (sum((pred.LASSO - visits.val.folded.fixedNames$LOS)^2)/sum((mean(visits.val.folded.fixedNames$LOS) - visits.val.folded.fixedNames$LOS)^2))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 7 - CV LASSO
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("glmnet")
library("glmnet")
#Set up the sparse matrices with dummy variables
#fix names priort to matrix
visits.folded.fixedNames = visits.folded
visits.val.folded.fixedNames = visits.val.folded

colnames(visits.folded.fixedNames) = make.names(colnames(visits.folded))
colnames(visits.val.folded.fixedNames) = make.names(colnames(visits.val.folded))


visits.matrix = as.matrix(sparse.model.matrix(~., visits.folded.fixedNames[-6]))
visits.val.matrix = as.matrix(sparse.model.matrix(~., visits.val.folded.fixedNames[-6]))

#Train model
ptm = proc.time()
fit.cvLASSO = cv.glmnet(y = visits.folded.fixedNames$LOS, x = visits.matrix)
proc.time() - ptm

#++++++++++++++++++++SAVE MODEL - 1 hour to train++++++++++++++++++++++#
# RMSE 5.94 on val
#saving model because it took a 10 hours to build
save(fit.cvLASSO, file = 'C:/Users/dickm/Documents/Projects/ML/DevProjects/ml-upmce-palooza-r/fit.cvLASSO.rda')

plot(fit.cvLASSO)

pred.cvLASSO = predict(fit.cvLASSO, visits.val.matrix)

#RMSE
sqrt(sum((pred.cvLASSO - visits.val.folded.fixedNames$LOS)^2)/nrow(visits.val.folded.fixedNames))

#SST
1 - (sum((pred.cvLASSO - visits.val.folded.fixedNames$LOS)^2)/sum((mean(visits.val.folded.fixedNames$LOS) - visits.val.folded.fixedNames$LOS)^2))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++++++++END EVAL+++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#







#get data. I preporecessed wiht python to go from space to comma delim.

#add libraries
#library("data.table")
#library("plyr")

#++++++++Read Data+++++++++++++++++++
#unified columns - 
#dataPath = "http://sparkdl04:50070/webhdfs/v1/palooza/data/visit_train_panda.csv?op=OPEN"
dataPath = "C:/Users/dickm/Documents/Projects/ML/Source/UPMC/Pharmacy/visit_train_panda.csv"
visits = read.csv(dataPath)

#++++++++++++++++++Transforms++++++++++++++++#
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

#++++++++++++++++++Extract features++++++++++++++++#


#Derive Train and Test Data
#install.packages("caTools")
library("caTools")
set.seed(144)
spl = sample.split(visits$LOS, 0.75)
train = subset(visits, spl == TRUE)
test = subset(visits, spl == FALSE)




#Idea to construct models based on removing outliers.

#Remove outliers
visits.regular = data.frame(visits[which(visits$LOS <= 14),])
visits.outlier = data.frame(visits[which(visits$LOS > 14),])

train.regular = data.frame(visits[which(visits$LOS <= 14),])

#++++++++++++++++++++++++++++++++Pre-Process++++++++++++++++++++++++++++++++++++++++++++++
#Remove DXCODE's that are in test but not in train. DXCODE is sparsely populated
test$DXCODE[which(!(test$DXCODE %in% unique(train$DXCODE)))] = NA
#Add in random sample of missing data
test$DXCODE[is.na(test$DXCODE)] = sample(test$DXCODE[!is.na(test$DXCODE)], sum(is.na(test$DXCODE)))



#+++++++++++++++++++++++++++++Train Data+++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Non-linear
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Method 1 - random forest on method 1 variables
#install.packages("randomForest")
library('randomForest')
ptm = proc.time() # how long does it take to train the model
modelRF = randomForest(LOS~Race+Gender+Age+Hospital, data=train, ntree = 200)
proc.time() - ptm

#Evaluate random forest in sample 
importance(modelRF)
varImpPlot(modelRF)

#RMSE, 6.37 with 200 trees
sqrt(sum((modelRF$predicted - train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelRF$predicted - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

#Evaluate random forest Test
predRF = predict(modelRF, newdata = test)

#RMSE
sqrt(sum((predRF - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predRF - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))

#Method 3 - random forest on method DOW only arrival since we will only know that.
# Took 1.5 hours with with 100 trees
ptm = proc.time() # how long does it take to train the model
modelRFDOW = randomForest(LOS~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, data=train, ntree = 200)
proc.time() - ptm

#Evaluate random forest in sample 
importance(modelRFDOW)
varImpPlot(modelRFDOW)

#RMSE, 6.37 with 200 trees
sqrt(sum((modelRFDOW$predicted - train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelRFDOW$predicted - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

#Evaluate random forest Test
predRFDOW = predict(modelRFDOW, newdata = test)

#RMSE
sqrt(sum((predRFDOW - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predRFDOW - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))


#*************************************************
# Save model
#*************************************************
#Took almost two hours to build so I just wanna save
#OPtion one rda file
basePath = "C:/Users/dickm/Documents/Projects/ML/DevProjects/ml-upmce-palooza-r"
#path = "C:/Users/dickm/Documents/Projects/ML/DevProjects/ml-upmce-palooza-r/modelRFDOW.rda"
#basePath = "C:/Users/dickm/Documents/Projects/ML/DevProjects/repos/ml-upmce-palooza-r"
pathRDA = paste(basePath, "modelRFDOW.rda", sep="/")
pathXML = paste(basePath, "modelRFDOW.xml", sep = "/")

save(modelRFDOW, file=pathRDA)

#now read and test
rm(modelRFDOW)
load(path)
importance(modelRFDOW)


#Option 2 PMML
library("pmml")
saveXML(pmml(modelRFDOW), pathXML)
#rm(modelRFDOW)
#TODO figure out how to get it out.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Linear
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Method 5 - linear regression with CV
install.packages("DAAG")
library("DAAG")
modelLMDOWCV = cv.lm(data = train, m=3, form.lm = formula(LOS~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW))
#RMSE
sqrt(sum((modelLMDOWCV$Predicted - train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelLMDOWCV$Predicted - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))
#CV
#RMSE
sqrt(sum((modelLMDOWCV$cvpred - train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelLMDOWCV$cvpred - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

predLMDOW = predict(modelLMDOWCV$Predicted, newdata=test)

#RMSE
sqrt(sum((predLMDOW - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLMDOW - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))




#Method 2 - linear regression (compare to spark)

modelLM = lm(LOS~Race+Gender+Age+Hospital, data=train)
#RMSE
sqrt(sum((modelLM$fitted.values- train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelLM$fitted.values - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

predLM = predict(modelLM, newdata=test)

#RMSE
sqrt(sum((predLM - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLM - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))


#Method 4 - linear regression with DOW and DXCODE

modelLMDOW = lm(LOS~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, data=train)
#RMSE
sqrt(sum((modelLMDOW$fitted.values- train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelLMDOW$fitted.values - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

predLMDOW = predict(modelLMDOW, newdata=test)

#RMSE
sqrt(sum((predLMDOW - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLMDOW - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))


#saving model because it took a long time to build
save(modelLMDOW, file = paste(basePath, "modelLMDOW.rda", sep = "/"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 6 - LASSO with or without standard set
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("glmnet")
library("glmnet")
#Set up the sparse matrices with dummy variables
modelMatrixLASSO = as.matrix(sparse.model.matrix(~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, train))
modelMatrixTestLASSO = as.matrix(sparse.model.matrix(~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, test))

#Train model
modelLASSO = glmnet(y = train$LOS, x = modelMatrixLASSO, standardize = TRUE)

plot(modelLASSO)

predLASSO = predict(modelLASSO, modelMatrixTestLASSO)

#RMSE
sqrt(sum((predLASSO - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLASSO - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 7 - CV LASSO with or without standard set
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("glmnet")
library("glmnet")
#Set up the sparse matrices with dummy variables
modelMatrixLASSO = as.matrix(sparse.model.matrix(~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, train))
modelMatrixTestLASSO = as.matrix(sparse.model.matrix(~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, test))


#train model 
modelLASSOCV = cv.glmnet(y = train$LOS, x = modelMatrixLASSO)

plot(modelLASSOCV)

predLASSOCV = predict(modelLASSOCV, modelMatrixTestLASSO)

#RMSE
sqrt(sum((predLASSOCV - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLASSOCV - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 8 - SVM
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("e1071")
library("e1071")

#train model 
fit.svm = svm(LOS~Race+Gender+Age+Hospital, data=train)


pred.svm = predict(fit.svm, test)

#RMSE
sqrt(sum((pred.svm - test$LOS)^2)/nrow(test))

#SST
1 - (sum((pred.svm - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 9 - SVM non-outlier
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("e1071")
library("e1071")

#train model 
ptm = proc.time()
fit.svmR = svm(LOS~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, data=train.regular)
proc.time() - ptm


pred.svmR = predict(fit.svm, test)

#RMSE
sqrt(sum((pred.svmR - test$LOS)^2)/nrow(test))

#SST
1 - (sum((pred.svmR - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Method 9 - SVM non-outlier and no dxcode
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#install.packages("e1071")
library("e1071")

#train model 
ptm = proc.time()
fit.svmS = svm(LOS~Race+Gender+Age+Hospital+ArriveDateDOW, data=train.regular)
proc.time() - ptm


pred.svmS = predict(fit.svm, test)

#RMSE
sqrt(sum((pred.svmS - test$LOS)^2)/nrow(test))

#SST
1 - (sum((pred.svmS - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))

#++++++++++++++++++++++++++++++++++++++++++++++++++
# Validate on palooza data using Method 7 (RMSE)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Read saved LASSO CV model
load("C:/Users/dickm/Documents/Projects/ML/DevProjects/repos/ml-upmce-palooza-r/modelLSSOCV.rda")

#Read in palooza val data (modified version with same factors as training)
#visitsVal = read.csv("C:/Users/dickm/Documents/UPMC/visit_test_panda_dxcode_factors.csv")
visitsVal = read.csv("C:/Users/dickm/Documents/Projects/ML/Source/UPMC/Pharmacy/visit_test_panda_dxcode_factors.csv")

#Pre-process data
visitsVal$VisitID = as.factor(visitsVal$VisitID)
visitsVal$Hospital = as.factor(visitsVal$Hospital)
visitsVal$Race = as.factor(visitsVal$Race)
visitsVal$Gender = as.factor(visitsVal$Gender)
visitsVal$DXCODE = as.factor(visitsVal$DXCODE)
visitsVal$FC = as.factor(visitsVal$FC)
visitsVal$DOC = as.factor(visitsVal$DOC)
visitsVal$ArriveDate = as.Date(visitsVal$ArriveDate, "%Y-%m-%d") # changed format of dates
visitsVal$DischargeDate = as.Date(visitsVal$DischargeDate, "%Y-%m-%d")  #changed the format of dates
visitsVal$ArriveDateDOW = as.factor(weekdays(visitsVal$ArriveDate))
visitsVal$DischargeDateDOW = as.factor(weekdays(visitsVal$DischargeDate))

#Added this because I got an error with the on prediction
#Remove DXCODE's that are in test but not in train. DXCODE is sparsely populated
visitsVal$DXCODE[which(!(visitsVal$DXCODE %in% unique(train$DXCODE)))] = NA
#Add in random sample of missing data
visitsVal$DXCODE[is.na(visitsVal$DXCODE)] = sample(visitsVal$DXCODE[!is.na(visitsVal$DXCODE)], sum(is.na(visitsVal$DXCODE)))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Need to ensure the factor info in data frame is same as trainin
levels(visitsVal$DXCODE) = levels(train$DXCODE)

#Get matrix input for LSSO model
valMatrix = as.matrix(sparse.model.matrix(~DXCODE+Race+Gender+Age+Hospital+ArriveDateDOW, visitsVal))

#Predict
predValLASSOCV = predict(modelLASSOCV, valMatrix)

#RMSE
sqrt(sum((predValLASSOCV - visitsVal$LOS)^2)/nrow(visitsVal))

#SST
1 - (sum((predValLASSOCV - visitsVal$LOS)^2)/sum((mean(visitsVal$LOS) - visitsVal$LOS)^2))




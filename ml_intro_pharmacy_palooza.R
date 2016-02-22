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

#++++++++++++++++++++Data Visiualization+++++++++++++++++++++++++++++++#

#simple hist on LOS and pie on hospital
#Try see drugs with highest mean LOS
tail(sort(tapply(train$LOS, train$DXCODE, mean))) 

#install.packages("ggplot2")
library("ggplot2")
#Plot with trend line.
plot = ggplot(data=train, aes(x=train$Age,y=train$LOS)) + geom_point() + geom_smooth(method = "lm", se=FALSE, color="red")
plot = plot + geom_point(aes(colour=train$Hospital, size=4))
plot + guides(size=FALSE) # remove 

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
modelRFDOW = randomForest(LOS~Race+Gender+Age+Hospital+ArriveDateDOW, data=train, ntree = 200)
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
basePath = "C:/Users/dickm/Documents/Projects/ML/DevProjects/ml-intro-spark-regression"
#path = "C:/Users/dickm/Documents/Projects/ML/DevProjects/ml-intro-spark-regression/modelRFDOW.rda"
basePath = "C:/Users/dickm/Documents/Projects/ML/DevProjects/repos/ml-intro-spark-regression"
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
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Linear
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++



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


#Method 4 - linear regression with DOW

modelLMDOW = lm(LOS~Race+Gender+Age+Hospital+ArriveDateDOW, data=train)
#RMSE
sqrt(sum((modelLMDOW$fitted.values- train$LOS)^2)/nrow(train))

#SST
1 - (sum((modelLMDOW$fitted.values - train$LOS)^2)/sum((mean(train$LOS) - train$LOS)^2))

predLMDOW = predict(modelLMDOW, newdata=test)

#RMSE
sqrt(sum((predLMDOW - test$LOS)^2)/nrow(test))

#SST
1 - (sum((predLMDOW - test$LOS)^2)/sum((mean(test$LOS) - test$LOS)^2))


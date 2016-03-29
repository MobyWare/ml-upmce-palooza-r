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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++SAVE MODEL - 1 hour to train++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
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

#Pre-process data
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


#Added this because I got an error with the on prediction
#Remove DXCODE's that are in test but not in train. DXCODE is sparsely populated
visits.val$DXCODE[which(!(visits.val$DXCODE %in% unique(train$DXCODE)))] = NA
#Add in random sample of missing data
visits.val$DXCODE[is.na(visits.val$DXCODE)] = sample(visits.val$DXCODE[!is.na(visits.val$DXCODE)], sum(is.na(visits.val$DXCODE)))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Need to ensure the factor info in data frame is same as trainin
levels(visits.val$DXCODE) = levels(train$DXCODE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++++++++END EVAL+++++++++++++++++++++++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#







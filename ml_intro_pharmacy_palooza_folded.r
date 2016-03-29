#Install libararies
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
visits.selected = unique(visits)[,c(-3,-4,-8,-13,-14,-15)]
visits.selected$HasDXCODE = 1
visits.folded = spread(visits.selected, DXCODE, HasDXCODE, fill = 0, drop = TRUE)
#visits.folded = visits[,c(-3,-4,-8,-12,-13,-14)]

#Group folded data-set 
visits.features = 
  visits.folded %>% 
  group_by(VisitID, Hospital, Age, Race, Gender, ArriveDate, DischargeDate, LOS, ArriveDateDOW, DischargeDateDOW) %>% 
  summarize_each(funs(sum))



#Verify grouping
#id which codes
tail(sort(table(visits$DXCODE)))

#250.00 is popular
head(visits.features %>% filter(X250.00==TRUE) %>% select(VisitID, X250.00, X003.9))

#++++++++++++++++++++End folding+++++++++++++++++++++++++++++++#

# Split
set.seed(144)
spl = sample.split(visits.features$LOS, 0.75)
train = subset(visits.features, spl == TRUE)
test = subset(visits.features, spl == FALSE)


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










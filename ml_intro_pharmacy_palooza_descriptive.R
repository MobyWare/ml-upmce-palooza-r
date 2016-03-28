#Install libararies
library("ggplot2")
library(caTools)
library(dplyr)
library(tidyr)
#Read data

dataPath = "C:/Users/dickm/Documents/Projects/ML/Source/UPMC/Pharmacy/visit_train_panda.csv"
visits = read.csv(dataPath)

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


#Remove outliers
visits.regular = data.frame(visits[which(visits$LOS <= 14),])
visits.outlier = data.frame(visits[which(visits$LOS > 14),])
qplot(y = LOS, x = Hospital, data=visits_regular, geom = "boxplot")


#++++++++++++++++++++Data Visiualization+++++++++++++++++++++++++++++++#
#install.packages("ggplot2")
library("ggplot2")


#simple hist on LOS and pie on hospital
#Try see drugs with highest mean LOS: 

tail(sort(tapply(visits$LOS, visits$DXCODE, mean))) 
#Results : E965.0

## More visualizations
#Plot with trend line.
plot = ggplot(data=visits, aes(x=visits$Age,y=visits$LOS)) + geom_point() + geom_smooth(method = "lm", se=FALSE, color="red")
plot = plot + geom_point(aes(colour=visits$Hospital, size=1))
plot + guides(size=FALSE) # remove 

plot = ggplot(data=visits.regular, aes(x=visits.regular$Age,y=visits.regular$LOS)) + geom_point() + geom_smooth(method = "lm", se=FALSE, color="red")
plot = plot + geom_point(aes(colour=visits.regular$Hospital, size=1))
plot + guides(size=FALSE) # remove 



#++++++++++++++++++++END VIZ+++++++++++++++++++++++++++++++#

#++++++++++++++++++++Folding+++++++++++++++++++++++++++++++#
visits.small = subset(visits, spl == TRUE)
spl = sample.split(visits$LOS, 0.1)
visits.small$HasDXCODE = 1
visits.folded = spread(visits.small, DXCODE, HasDXCODE, fill = 0, drop = TRUE)

#method 1 - group by 
visits.groupedAll = 
  visits.folded.selected %>% 
  group_by(VisitID, Hospital, Age, Race, Gender, ArriveDate, DischargeDate, LOS, ArriveDateDOW, DischargeDateDOW) %>% 
  summarize_each(funs(sum))

#method 2 - join later
visits.dxcodes = data.frame(visits.folded[,seq(-16,-2)]) #wanna keep visitID.
visits.folded.selected = data.frame(visits.folded[,c(-3,-4,-8,-12,-13,-14)]) #limit to models columns

#summarize_each allows me to aggregate everything all columns without naming them
visits.grouped = visits.dxcodes %>% group_by(VisitID) %>% summarize_each(funs(sum))

#join
#tODO - do merge expression. Abandonded because I would have do group step anyway.

#Verify grouping
#id which codes
tail(sort(table(visits.small$DXCODE)))

#250.00 is popular
head(visits.groupedAll %>% filter(X250.00==TRUE) %>% select(VisitID, X250.00, X003.9))
#TODO - figure out grouping. and column name issue. Look at group by.
#++++++++++++++++++++End folding+++++++++++++++++++++++++++++++#

#++++++++++++++++++++Data Clustering+++++++++++++++++++++++++++++++#

visits.cluster= data.frame(visits)

#remove stuff I don't want to cluster

visits.cluster$VisitID = NULL






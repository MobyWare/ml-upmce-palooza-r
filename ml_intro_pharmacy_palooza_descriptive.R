#Install libararies
library("ggplot2")
library(caTools)

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

visits.cluster= data.frame(visits)

#remove stuff I don't want to cluster

visits.cluster$VisitID = NULL
visits.cluster$VisitID = NULL
visits.cluster$VisitID = NULL
visits.cluster$VisitID = NULL
visits.cluster$VisitID = NULL
visits.cluster$VisitID = NULL
visits.cluster$VisitID = NULL


#Remove outliers
visits.regular = data.frame(visits[which(visits$LOS <= 14),])
visits.outlier = data.frame(visits[which(visits$LOS > 14),])
qplot(y = LOS, x = Hospital, data=visits_regular, geom = "boxplot")


#++++++++++++++++++++Data Visiualization+++++++++++++++++++++++++++++++#
#install.packages("ggplot2")
library("ggplot2")


#simple hist on LOS and pie on hospital
#Try see drugs with highest mean LOS
tail(sort(tapply(train$LOS, train$DXCODE, mean))) 


## More visualizations
#Plot with trend line.
plot = ggplot(data=visits, aes(x=visits$Age,y=visits$LOS)) + geom_point() + geom_smooth(method = "lm", se=FALSE, color="red")
plot = plot + geom_point(aes(colour=visits$Hospital, size=1))
plot + guides(size=FALSE) # remove 

plot = ggplot(data=visits.regular, aes(x=visits.regular$Age,y=visits.regular$LOS)) + geom_point() + geom_smooth(method = "lm", se=FALSE, color="red")
plot = plot + geom_point(aes(colour=visits.regular$Hospital, size=1))
plot + guides(size=FALSE) # remove 


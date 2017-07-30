setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 11.2 -  Decison Trees")


#LOading packages for decission trees
library(dplyr)
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)

ds <- read.csv("BH.csv")
str(ds)
head(ds$cus_employername)
unique(ds$cus_employername)
head(ds$GOOD_BAD)


ds%>%mutate(Target=ifelse(GOOD_BAD=='GOOD',1,0))->ds
head(ds$Target)
head(ds$TARGET)
str(ds$Target)
levels(as.factor(ds$Target))
#ds$Target<-as.factor(ds$Target)
levels(as.factor(ds$TARGET))
#ifelse(levels(as.factor(ds$Target))==levels(as.factor(ds$TARGET)),1,0)
ds%>%select(-GOOD_BAD,-TARGET)->ds

summary(ds$Target)
summary(ds$cus_employername)

mod<-rpart(Target~cus_employername,data=ds,control=rpart.control(maxdepth=2),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)

#################################################
plot(mod,  main="Classification Tree")
text(mod, use.n=TRUE, all=TRUE, cex=.7)
mod

plotcp(mod,minline = TRUE)
#################################################

actual<-ds$Target
predicted<-predict(mod,type = "class")

head(predicted)
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

confusionMatrix(predicted,actual,positive="1")

#kappa metric
kappa2(data.frame(actual,predicted))

#ROC curve analysis
library(ROCR)
pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

#Assigning the node values to respective data
unique(mod$where)
ds$predicted_val <- mod$where
summary(ds$predicted_val)
sum(ifelse(ds$predicted_val==2,1,0))
sum(ifelse(ds$predicted_val==3,1,0))

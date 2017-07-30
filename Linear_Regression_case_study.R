setwd("C:\\Data Science with R\\Assignments\\Non Graded Assignments\\Topic 9 - Linear Regression")
housing_data<- read.csv("boston_prices.csv")
str(housing_data)
dim(housing_data)
head(housing_data)
names(housing_data)
summary(housing_data)



#missing value treatment
housing_data$MEDV[is.na(housing_data$MEDV)]<- mean(housing_data$MEDV,na.rm = TRUE)
summary(housing_data)

#boxplot for all variables
par(mfrow=c(2,7))
list<-names(housing_data)
list
list<-list[-4]
for(i in 1:length(list)){
  boxplot(housing_data[,list[i]],main=list[i])
}

dev.off()

for(i in 1:length(list)){
  x<-boxplot(housing_data[,list[i]])
  out<- x$out
  index<- which(housing_data[,list[i]] %in% x$out)
  housing_data[index,list[i]]<-mean(housing_data[-index,list[i]])
  rm(x)
  rm(out)
}

library(ggplot2)
hist(housing_data$MEDV)

#You can look at the correlation between each IDV and the DV
#An eg :
ggplot(housing_data,aes(x=MEDV,y=LSTAT)) +geom_point()
ggplot(housing_data,aes(x=MEDV,y=DIS)) +geom_point()
ggplot(housing_data,aes(x=MEDV,y=AGE)) +geom_point()

list1<- list[-13]
for (i  in 1:length(list1)){
  x<-cor(housing_data$MEDV,housing_data[list1[i]])
  print(x)
}


#Log transformations
#Create the log transformation for all variables
housing_data$log_CRIM<-log(housing_data$CRIM)
housing_data$log_ZN<-log(housing_data$ZN)
housing_data$log_NOX<-log(housing_data$nitric.oxides.concentration)
housing_data$log_RM<-log(housing_data$X.rooms.dwelling)
housing_data$log_AGE<-log(housing_data$AGE)
housing_data$log_DIS<-log(housing_data$DIS)
housing_data$log_RAD<-log(housing_data$RAD)
housing_data$log_TAX<-log(housing_data$TAX)
housing_data$log_PTRATIO<-log(housing_data$PTRATIO)
housing_data$log_B<-log(housing_data$B)
housing_data$log_LSTAT<-log(housing_data$LSTAT)
housing_data$log_MEDV<-log(housing_data$MEDV) #DV
housing_data$log_INDUS<-log(housing_data$INDUS)



#Function to get the list of correlations between : log_DV and log of IDV's
summary(housing_data)
list_log<-names(housing_data)[c(15:25,27)]
for(i in 1:length(list_log))
{
  xlog<-cor(housing_data$log_MEDV,housing_data[list_log[i]])
  print(xlog)
}

#Function to get the list of correlations between : log_DV and IDV's

list_log_DV<-names(housing_data)[1:13]
list_log_DV<-list_log_DV[-4]
for(i in 1:length(list_log_DV))
{
  xlogdv<-cor(housing_data$log_MEDV,housing_data[list_log_DV[i]])
  print(xlogdv)
}

sampling<- sort(sample(nrow(housing_data),nrow(housing_data)*0.7))
train<- housing_data[sampling,]
test<- housing_data[-sampling,]
nrow(train)
nrow(test)
str(train)

##Building SimpLe Linear Regression Model

##Building SimpLe Linear Regression Model

#Metrics :
#Rsquare
#Coefficients
#P values : Significance levels of the IDV's
#Residuals distribution

#Factor variables as IDV's
#All good modelssummm
Reg<-lm(log_MEDV~CRIM+INDUS+RAD+TAX+B+
          Charles.River.dummy.variable+
          DIS+ZN+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)

summary(Reg)

#Getting the formula
formula(Reg)

#Getting the formula
formula(Reg)

#Remove insignificant variables :

Reg1<-lm(log_MEDV~
           Charles.River.dummy.variable+
           DIS+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)
summary(Reg1)


#Reg2 : remove insignificant values

Reg2 <- lm(log_MEDV ~CRIM+INDUS+RAD+TAX+B+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+X.rooms.dwelling+nitric.oxides.concentration, data=train)
summary(Reg2)

#Reg3 _ remove insignificant values
Reg3 <- lm(log_MEDV ~CRIM+RAD+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+nitric.oxides.concentration, data=train)
summary(Reg3)

#Some other combination 
Reg4<-lm(log_MEDV~INDUS  +ZN + X.rooms.dwelling + LSTAT+CRIM + Charles.River.dummy.variable,data=train)
summary(Reg4)

#The best model happens to be : Reg3

##Getting predicted values
predicted<-predict(Reg3)
plot(predicted)
length(predicted)


##Finding Residuals
residuals<-resid(Reg3)
plot(residuals)
length(residuals)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity
##There should be no trend between predicted values and residual values
plot(predicted,residuals,abline(0,0))

#You can notice that there seems to be an inverse pattern for some points

#So this model may not be the preferred model.

#atttching predicted values to test data
predicted<-predict(Reg3,newdata=test)
length(predicted)
test$p<-predicted

#Calculating error in the test dataset - (Actual- predicted)/predicted values
test$error<-(test$log_MEDV-test$p)/test$log_MEDV
mean(test$error)*100 #you get to know the average error in the given dataset



##Plotting actual vs predicted values
plot(test$p,col="blue",type="l")
lines(test$log_MEDV,col="red",type="l")

#checking for Correlation between variables
library(car)
vif(Reg3)

#You can drop variables if they have a vif>10 ; means high correlation between variables




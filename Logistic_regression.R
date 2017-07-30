setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 10 - Logistic Regression")
brand_data<- read.csv("goodforu-class12.csv")
dim(brand_data)
str(brand_data)
summary(brand_data)
head(brand_data,2)
#brand_data$X1<- as.factor(brand_data$X1)
head(brand_data)
colnames(brand_data)

brand_a_data<- brand_data[,c("X2","X9","X16","X30","X23")]
str(brand_a_data)
summary(brand_a_data)
sort(unique(brand_a_data$X23))

#Making the dependent variable binary, Assuming bad if rating less than 6

library(dplyr)
brand_a_data<- mutate(brand_a_data,good_bad=ifelse(brand_a_data$X23<=5,0,1))
str(brand_a_data)
head(brand_a_data)

brand_a_data<-brand_a_data[,-c(5)]

#dividing data into train and test in 70-30 ratio
index<- sample(nrow(brand_a_data),0.7*nrow(brand_a_data),replace = FALSE)
test<- brand_a_data[-index,]
train<- brand_a_data[index,]
nrow(test)
nrow(train)

#Checking for correct sampling data
table(brand_a_data$good_bad)/nrow(brand_a_data)
table(test$good_bad)/nrow(test)
table(train$good_bad)/nrow(train)

mod1<- glm(formula = good_bad~.,family = "binomial",data=train)
summary(mod1)

step(mod1,direction="both")

#Final model equation
mod2<-glm(formula = good_bad ~ X2 + X9 + X16 + X30, family = "binomial",data = train)
summary(mod2)

#predicting dependent data using test data
pred<- predict(mod2,type = "response",newdata = test)
table(test$good_bad)/nrow(test)
pred<- ifelse(pred>=0.2497581,1,0)
table(pred)
library(gains)
library(caret)
library(irr)

  kappa2(data.frame(test$good_bad,pred))

confusionMatrix(pred,test$good_bad,positive = "1")

gains(test$good_bad,predict(mod2,type = "response",newdata = test),groups = 10)

#From the final model constructed, we can able to infer that brands made with farm grown ingredients like potato, corn or wheat,
#brands having zero grams trans-fat & brands made with natural oils are negetively impacting 
#brand perception, where as food processing level is improving the brand perception.


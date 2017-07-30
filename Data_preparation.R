setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 8.2 -  Data Preparation")
campaign_data<- read.table("Campaign_File.txt",sep = "\t",header = TRUE)
customer_data<- read.table("Customers_File.txt",sep = "\t",header = TRUE)
products_data<- read.table("Products_File.txt",sep = "\t",header = TRUE)
transaction_data<- read.table("Transactions_File.txt",sep = "\t",header = TRUE)
head(campaign_data)
summary(campaign_data)
summary(customer_data)
summary(products_data)
summary(transaction_data)
head(products_data)
head(products_data$Product_Code)
library(dplyr)
library(lubridate)

#Q1. Based on the transactions, 
#which product category dominates in terms of $ amount?
trans_merge<-merge(transaction_data,products_data,by = "Product_Code")
summary(trans_merge)

trans_merge%>%filter(trans_merge$Items_Number!=1)
head(trans_merge)
table(trans_merge$Product_Category)

prod_cat<-group_by(trans_merge,Product_Category)
prod_cat
item_amount_grp<-summarize(prod_cat,sum(Items_Amount))
max(item_amount_grp$`sum(Items_Amount)`)
item_amount_grp$Product_Category[item_amount_grp$`sum(Items_Amount)`==max(item_amount_grp$`sum(Items_Amount)`)]
##Entertainment product catagory dominates in terms of $ amount

##Q2. Perform a suitable age grouping and find out contribution of 
#each of the age group in terms of $ amount spent.
merge_agedata<- merge(transaction_data,customer_data,by = "Card_ID")
summary(merge_agedata)
str(merge_agedata)
head(merge_agedata$Birth_Date)

merge_agedata$Birth_Date<- as.Date(as.character(merge_agedata$Birth_Date))
merge_agedata$Timestamp<- as.Date(as.character(merge_agedata$Timestamp))
head(merge_agedata$Timestamp)
merge_agedata$age <- as.numeric(format(merge_agedata$Timestamp,format='%Y'))-as.numeric(format(merge_agedata$Birth_Date,format='%Y'))
#age calculated considering age at time of transaction made
merge_agedata$age
#merge_agedata%>%mutate(Quantile=ntile(age,3)) ->merged_data
#str(merged_data)
#sort(unique(merged_data$age[merged_data$Quantile==1]))
#sort(unique(merged_data$age[merged_data$Quantile==2]))
#sort(unique(merged_data$age[merged_data$Quantile==3]))

#merge_agedata$Quantile<-ifelse(merge_agedata$age<40,'lt40',merge_agedata$Quantile)
#merge_agedata$Quantile<-ifelse(merge_agedata$age>=40 & merge_agedata$age<60,'lt60',merge_agedata$Quantile)
#merge_agedata$Quantile<-ifelse(merge_agedata$age>=60,'gt60',merge_agedata$Quantile)
merge_agedata$age[merge_agedata$age>90]
str(merge_agedata)
min(merge_agedata$age)
max(merge_agedata$age)
merge_agedata$bins<- cut(merge_agedata$age,breaks = c(1,40,70,100),labels = FALSE)
sort(unique(merge_agedata$bins))
sort(unique(merge_agedata$age[merge_agedata$bin==1]))
sort(unique(merge_agedata$age[merge_agedata$bin==2]))
sort(unique(merge_agedata$age[merge_agedata$bin==3]))

#sort(unique(merge_agedata$age[merge_agedata$Quantile=='lt60']))
#sort(unique(merge_agedata$age[merge_agedata$Quantile=='gt60']))
#merged_data$age[merged_data$age>100]
#group_data<-group_by(merged_data,Quantile)
group_data<-group_by(merge_agedata,bins)
amount_grp_quantile<-summarize(group_data,sum(Items_Amount))
amount_grp_quantile$agegrp<- c('10-40','40-70','70-100')
amount_grp_quantile
#amount_grp_quantile will provide the required answer as below
#    bins sum(Items_Amount) agegrp
#    (int)             (int)  (chr)
#     1          48804663  10-40
#     2          36700697  40-70
#     3           1187137 70-100



#Q3-Find the response rate to the campaign. 
#Also identify the age group of customers where response rate is high. 
#Is there a consistent trend.
str(campaign_data)
str(customer_data)
head(customer_data)

Campaing_merge_data<- merge(campaign_data,customer_data,by = 'Card_ID')
str(Campaing_merge_data)
head(Campaing_merge_data)
nrow(Campaing_merge_data)
response<-nrow(Campaing_merge_data[Campaing_merge_data$Campaign_Responce=='TRUE',])/nrow(Campaing_merge_data)
response#0.05455766~5%
Campaing_merge_data$Birth_Date<- as.Date(as.character(Campaing_merge_data$Birth_Date))
Campaing_merge_data$age <- as.numeric(format(Sys.Date(),format='%Y'))-as.numeric(format(Campaing_merge_data$Birth_Date,format='%Y'))
Campaing_merge_data[Campaing_merge_data$age>=100,]

max(Campaing_merge_data$age)
min(Campaing_merge_data$age)
Campaing_merge_data$bins<- cut(Campaing_merge_data$age,breaks = c(20,40,60,80,110))
sort(unique(Campaing_merge_data$bins))
sort(unique(Campaing_merge_data$age[Campaing_merge_data$bins=='(20,40]']))
sort(unique(Campaing_merge_data$age[Campaing_merge_data$bins=='(40,60]']))
sort(unique(Campaing_merge_data$age[Campaing_merge_data$bins=='(60,80]']))
sort(unique(Campaing_merge_data$age[Campaing_merge_data$bins=='(80,110]']))

response_20_40<-nrow(Campaing_merge_data[Campaing_merge_data$Campaign_Responce=='TRUE' & Campaing_merge_data$bins=='(20,40]',])/nrow(Campaing_merge_data[Campaing_merge_data$bins=='(20,40]',])
response_20_40#0.06610169
response_40_60<-nrow(Campaing_merge_data[Campaing_merge_data$Campaign_Responce=='TRUE' & Campaing_merge_data$bins=='(40,60]',])/nrow(Campaing_merge_data[Campaing_merge_data$bins=='(40,60]',])
response_40_60#0.05178523
response_60_80<-nrow(Campaing_merge_data[Campaing_merge_data$Campaign_Responce=='TRUE' & Campaing_merge_data$bins=='(60,80]',])/nrow(Campaing_merge_data[Campaing_merge_data$bins=='(60,80]',])
response_60_80#0.05890052
response_80_110<-nrow(Campaing_merge_data[Campaing_merge_data$Campaign_Responce=='TRUE' & Campaing_merge_data$bins=='(80,110]',])/nrow(Campaing_merge_data[Campaing_merge_data$bins=='(80,110]',])
response_80_110#0.03529412
#resposce rate decreses as age increses



#Q4. Repeat the analysis above with "Tenure" of customer. 
#(Tenure will be defined as the time period between the Date of Registration and 31/12/2002)
str(campaign_data)
str(customer_data)
campaign_cust_merge<- merge(campaign_data,customer_data,by = 'Card_ID')
str(campaign_cust_merge)
campaign_cust_merge$Registration_Date<- as.Date(as.character(campaign_cust_merge$Registration_Date))
campaign_cust_merge$Tenure<- 2002-as.numeric(format(campaign_cust_merge$Registration_Date,format='%Y'))
unique(campaign_cust_merge$Tenure)
response_tenure1<-nrow(campaign_cust_merge[campaign_cust_merge$Campaign_Responce=='TRUE' & campaign_cust_merge$Tenure==1,])/nrow(campaign_cust_merge[campaign_cust_merge$Tenure==1,])
response_tenure1#0.04416961
response_tenure2<-nrow(campaign_cust_merge[campaign_cust_merge$Campaign_Responce=='TRUE' & campaign_cust_merge$Tenure==2,])/nrow(campaign_cust_merge[campaign_cust_merge$Tenure==2,])
response_tenure2#0.05389908
response_tenure3<-nrow(campaign_cust_merge[campaign_cust_merge$Campaign_Responce=='TRUE' & campaign_cust_merge$Tenure==3,])/nrow(campaign_cust_merge[campaign_cust_merge$Tenure==3,])
response_tenure3#0.05509642
response_tenure4<-nrow(campaign_cust_merge[campaign_cust_merge$Campaign_Responce=='TRUE' & campaign_cust_merge$Tenure==4,])/nrow(campaign_cust_merge[campaign_cust_merge$Tenure==4,])
response_tenure4#0.05786026
#Response increses as tenure increses

#Q5. Create a cross tab of response rate between Age and Tenure of customers. 
#Do you observe anything?
campaign_cust_merge$Birth_Date<- as.Date(as.character(campaign_cust_merge$Birth_Date))
campaign_cust_merge$age <- as.numeric(format(Sys.Date(),format='%Y'))-as.numeric(format(campaign_cust_merge$Birth_Date,format='%Y'))
table(sort(campaign_cust_merge$Tenure),sort(campaign_cust_merge$age))
#We can observe tenure of people increses as age increses


#Q6. Which mode of payment is most popular? 
#Is mode of payment affected by the time of transaction?
str(transaction_data)
unique(transaction_data$Payment_Method)
class(transaction_data$Timestamp)
transaction_data$Timestamp<- as.POSIXct(transaction_data$Timestamp)
str(transaction_data)
transaction_data$transtime<- as.POSIXlt(transaction_data$Timestamp)
head(transaction_data$transtime)
unique(transaction_data$transtime$hour)
table(transaction_data$transtime$hour)
table(transaction_data$Payment_Method)
table(transaction_data$Payment_Method,transaction_data$transtime$hour)
#For the above result it seems there is no impact of time of transaction to mode of payment
#trans_hrs   10    11    12    13    14    15    16    17    18
#Cash       11796 11803 11941 11665 11869 11804 11731 11821   198
#CreditCard 19891 19912 19843 19708 19802 19940 19800 19939   379
#DebitCard  11940 11927 11872 12075 11922 12168 12055 12219   221

#Q7. Do you think, based on the data, 
#that age and gender has any impact on $ amount spent?

transaction_data<- read.table("Transactions_File.txt",sep = "\t",header = TRUE)
str(transaction_data)
str(customer_data)
merged_customer_data<- merge(transaction_data,customer_data,by = 'Card_ID')
str(merged_customer_data)
merged_customer_data$Birth_Date<- as.Date(as.character(merged_customer_data$Birth_Date))
merged_customer_data$Timestamp<- as.Date(as.character(merged_customer_data$Timestamp))
merged_customer_data$age <- as.numeric(format(merged_customer_data$Timestamp,format='%Y'))-as.numeric(format(merged_customer_data$Birth_Date,format='%Y'))
#age calculated considering age at time of transaction made
str(merged_customer_data)
max(merged_customer_data$age)
merged_customer_data$bins<- cut(merged_customer_data$age,breaks = c(9,40,70,100))
xtabs(merged_customer_data$Items_Amount~merged_customer_data$Gender+merged_customer_data$bins)
#sum(merged_customer_data$Items_Amount)
#                            merged_customer_data$bins
#merged_customer_data$Gender   (9,40]  (40,70] (70,100]
#                           F 24378837 18411747   512085
#                           M 24425826 18288950   675052
#From the above output it seems amount spend decrceses as age increses.

#Q8. Produce a histogram for "tenure of a customer" separately for male and female customers.
merged_customer_data$Registration_Date<- as.Date(as.character(merged_customer_data$Registration_Date))
merged_customer_data$Tenure<- 2002-as.numeric(format(merged_customer_data$Registration_Date,format='%Y'))
unique(campaign_cust_merge$Tenure)
library(ggplot2)
vis<- ggplot(merged_customer_data[merged_customer_data$Gender=='M',],aes(x=Tenure))
vis+geom_histogram(aes(fill=Gender))
vis<- ggplot(merged_customer_data[merged_customer_data$Gender=='F',],aes(x=Tenure))
vis+geom_histogram(aes(fill=Gender))

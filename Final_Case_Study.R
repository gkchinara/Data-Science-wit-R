setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 1v -  Final Case Study Course Wrap up")
ds<-read.csv("telecomfinal.csv")

str(ds)
unique(ds$churn)
dim(ds)
names(ds)

#Code for data quality report Preparation

str(ds$mou_Mean)
summary(ds$mou_Mean)
str(unique(ds$mou_Mean))
summary(is.na(ds$mou_Mean))
length(ds$mou_Mean[ is.na(ds$mou_Mean)])/length(ds$mou_Mean)*100
length(ds$mou_Mean[ !is.na(ds$mou_Mean)])/length(ds$mou_Mean)*100
quantile(ds$mou_Mean,na.rm = TRUE,c(0.05,0.10,0.25,0.5,0.75,0.9,0.95))

names(ds)[2]
summary(ds[,2])
str(unique(ds[,2]))
summary(is.na(ds[,2]))
length(ds[,2][ is.na(ds[,2])])/length(ds[,2])*100
length(ds[,2][ !is.na(ds[,2])])/length(ds[,2])*100
quantile(ds[,2],na.rm = TRUE,c(0.05,0.10,0.25,0.5,0.75,0.9,0.95))
str(ds[,2])

#Generic
v<-81
names(ds)[v]
summary(ds[,v])
str(unique(ds[,v]))
summary(is.na(ds[,v]))
length(ds[,v][ is.na(ds[,v])])/length(ds[,v])*100#NA values
length(ds[,v][ !is.na(ds[,v])])/length(ds[,v])*100#Data Values
quantile(ds[,v],na.rm = TRUE,c(0.05,0.10,0.25,0.5,0.75,0.9,0.95))
str(ds[,v])


##########################################################################

dim(ds)
#summary(ds[,12])
#indx<-which(is.na(ds$mou_Mean))
#ds<-ds[-indx,]
library(dplyr)

#Removing the  columns having more than 15% data as missing values
ds<-ds[,-c(12,46,47,48,49,52,53,55,61,62,63,64,66,67,72,79)]

#Analyzing and imputing Other missing value data
colSums(is.na(ds))

dim(ds)
ds<- ds[-which(is.na(ds$models)),]

#summary(ds$eqpdays)
#ds<- ds[-which(is.na(ds$eqpdays)),]
#dim(ds)
#ds<- ds[-which(is.na(ds$area)),]
#dim(ds)
#ds<- ds[-which(is.na(ds$marital)),]
#dim(ds)
#colSums(is.na(ds))
#summary(ds$change_mou)
#quantile(ds$change_mou,p=c(1:8)/8,na.rm = TRUE)
#library(sqldf)
#summary(ds$marital)


############################################################################################################################################
#Making decile binning for continuous variables for further investigation

#Mean number of monthly minutes of use
ds%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_mou_Mean
dat_mou_Mean$N<-unclass(ds%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_mou_Mean$churn_perc<-dat_mou_Mean$n/dat_mou_Mean$N
dat_mou_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat_mou_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat_mou_Mean$varname<-rep("mou_Mean",nrow(dat_mou_Mean))
dat_mou_Mean
write.csv(x = dat_mou_Mean,file = "data_dec_mou_mean.csv")

#Mean total monthly recurring charge
ds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_totmrc_Mean
dat_totmrc_Mean$N<-unclass(ds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_totmrc_Mean$churn_perc<-dat_totmrc_Mean$n/dat_totmrc_Mean$N
dat_totmrc_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat_totmrc_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat_totmrc_Mean$varname<-rep("totmrc_Mean",nrow(dat_totmrc_Mean))
dat_totmrc_Mean
write.csv(x = dat_totmrc_Mean,file = "dat_totmrc_Mean.csv")

#Range of revenue (charge amount)
ds%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_rev_Range
dat_rev_Range$N<-unclass(ds%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_rev_Range$churn_perc<-dat_rev_Range$n/dat_rev_Range$N
dat_rev_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat_rev_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat_rev_Range$varname<-rep("rev_Range",nrow(dat_rev_Range))
dat_rev_Range
write.csv(x = dat_rev_Range,file = "dat_rev_Range.csv")

#Range of number of minutes of use
ds%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_mou_Range
dat_mou_Range$N<-unclass(ds%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_mou_Range$churn_perc<-dat_mou_Range$n/dat_mou_Range$N
dat_mou_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat_mou_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat_mou_Range$varname<-rep("mou_Range",nrow(dat_mou_Range))
dat_mou_Range
write.csv(x = dat_mou_Range,file = "dat_mou_Range.csv")

#Percentage change in monthly minutes of use vs previous three month average
ds%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_change_mou
dat_change_mou$N<-unclass(ds%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_change_mou$churn_perc<-dat_change_mou$n/dat_change_mou$N
dat_change_mou$GreaterThan<-unclass(ds%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat_change_mou$LessThan<-unclass(ds%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat_change_mou$varname<-rep("change_mou",nrow(dat_change_mou))
dat_change_mou
write.csv(x = dat_change_mou,file = "dat_change_mou.csv")

#Mean number of dropped or blocked calls
ds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_drop_blk_Mean
dat_drop_blk_Mean$N<-unclass(ds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_drop_blk_Mean$churn_perc<-dat_drop_blk_Mean$n/dat_drop_blk_Mean$N
dat_drop_blk_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat_drop_blk_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat_drop_blk_Mean$varname<-rep("drop_blk_Mean",nrow(dat_drop_blk_Mean))
dat_drop_blk_Mean
write.csv(x = dat_drop_blk_Mean,file = "dat_drop_blk_Mean.csv")

#Range of number of dropped (failed) voice calls
ds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_drop_vce_Range
dat_drop_vce_Range$N<-unclass(ds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_drop_vce_Range$churn_perc<-dat_drop_vce_Range$n/dat_drop_vce_Range$N
dat_drop_vce_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat_drop_vce_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat_drop_vce_Range$varname<-rep("drop_vce_Range",nrow(dat_drop_vce_Range))
dat_drop_vce_Range
write.csv(x = dat_drop_vce_Range,file = "dat_drop_vce_Range.csv")

#Range of number of outbound wireless to wireless voice calls
ds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_owylis_vce_Range
dat_owylis_vce_Range$N<-unclass(ds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_owylis_vce_Range$churn_perc<-dat_owylis_vce_Range$n/dat_owylis_vce_Range$N
dat_owylis_vce_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat_owylis_vce_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat_owylis_vce_Range$varname<-rep("owylis_vce_Range",nrow(dat_owylis_vce_Range))
dat_owylis_vce_Range
write.csv(x = dat_owylis_vce_Range,file = "dat_owylis_vce_Range.csv")

#Range of unrounded minutes of use of off-peak voice calls
ds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_mou_opkv_Range
dat_mou_opkv_Range$N<-unclass(ds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_mou_opkv_Range$churn_perc<-dat_mou_opkv_Range$n/dat_mou_opkv_Range$N
dat_mou_opkv_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat_mou_opkv_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat_mou_opkv_Range$varname<-rep("mou_opkv_Range",nrow(dat_mou_opkv_Range))
dat_mou_opkv_Range
write.csv(x = dat_mou_opkv_Range,file = "dat_mou_opkv_Range.csv")

#Total number of months in service
ds%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_months
dat_months$N<-unclass(ds%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat_months$churn_perc<-dat_months$n/dat_months$N
dat_months$GreaterThan<-unclass(ds%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat_months$LessThan<-unclass(ds%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat_months$varname<-rep("months",nrow(dat_months))
dat_months
write.csv(x = dat_months,file = "dat_months.csv")

#Total number of calls over the life of the customer
ds%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_totcalls
dat_totcalls$N<-unclass(ds%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat_totcalls$churn_perc<-dat_totcalls$n/dat_totcalls$N
dat_totcalls$GreaterThan<-unclass(ds%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat_totcalls$LessThan<-unclass(ds%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat_totcalls$varname<-rep("totcalls",nrow(dat_totcalls))
dat_totcalls
write.csv(x = dat_totcalls,file = "dat_totcalls.csv")

#Number of days (age) of current equipment
ds%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_eqpdays
dat_eqpdays$N<-unclass(ds%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat_eqpdays$churn_perc<-dat_eqpdays$n/dat_eqpdays$N
dat_eqpdays$GreaterThan<-unclass(ds%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat_eqpdays$LessThan<-unclass(ds%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat_eqpdays$varname<-rep("eqpdays",nrow(dat_eqpdays))
dat_eqpdays
write.csv(x = dat_eqpdays,file = "dat_eqpdays.csv")

#Mean number of customer care calls
ds%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_custcare_Mean
dat_custcare_Mean$N<-unclass(ds%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_custcare_Mean$churn_perc<-dat_custcare_Mean$n/dat_custcare_Mean$N
dat_custcare_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
dat_custcare_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
dat_custcare_Mean$varname<-rep("custcare_Mean",nrow(dat_custcare_Mean))
dat_custcare_Mean
write.csv(x = dat_custcare_Mean,file = "dat_custcare_Mean.csv")

#Mean number of call waiting calls
ds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat_callwait_Mean
dat_callwait_Mean$N<-unclass(ds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat_callwait_Mean$churn_perc<-dat_callwait_Mean$n/dat_callwait_Mean$N
dat_callwait_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat_callwait_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat_callwait_Mean$varname<-rep("callwait_Mean",nrow(dat_callwait_Mean))
dat_callwait_Mean
write.csv(x = dat_callwait_Mean,file = "dat_callwait_Mean.csv")

#Mean number of inbound wireless to wireless voice calls
ds%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_iwylis_vce_Mean
dat_iwylis_vce_Mean$N<-unclass(ds%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_iwylis_vce_Mean$churn_perc<-dat_iwylis_vce_Mean$n/dat_iwylis_vce_Mean$N
dat_iwylis_vce_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat_iwylis_vce_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat_iwylis_vce_Mean$varname<-rep("iwylis_vce_Mean",nrow(dat_iwylis_vce_Mean))
dat_iwylis_vce_Mean
write.csv(x = dat_iwylis_vce_Mean,file = "dat_iwylis_vce_Mean.csv")

#Range of number of call waiting calls
ds%>%mutate(dec=ntile(callwait_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_callwait_Range
dat_callwait_Range$N<-unclass(ds%>%mutate(dec=ntile(callwait_Range,n=2))%>%count(dec)%>%unname())[[2]]
dat_callwait_Range$churn_perc<-dat_callwait_Range$n/dat_callwait_Range$N
dat_callwait_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(callwait_Range,n=2))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
dat_callwait_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(callwait_Range,n=2))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
dat_callwait_Range$varname<-rep("callwait_Range",nrow(dat_callwait_Range))
dat_callwait_Range
write.csv(x = dat_callwait_Range,file = "dat_callwait_Range.csv")

#Range of rounded minutes of use of customer care calls
ds%>%mutate(dec=ntile(ccrndmou_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_ccrndmou_Range
dat_ccrndmou_Range$N<-unclass(ds%>%mutate(dec=ntile(ccrndmou_Range,n=2))%>%count(dec)%>%unname())[[2]]
dat_ccrndmou_Range$churn_perc<-dat_ccrndmou_Range$n/dat_ccrndmou_Range$N
dat_ccrndmou_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(ccrndmou_Range,n=2))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
dat_ccrndmou_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(ccrndmou_Range,n=2))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
dat_ccrndmou_Range$varname<-rep("ccrndmou_Range",nrow(dat_ccrndmou_Range))
dat_ccrndmou_Range
write.csv(x = dat_ccrndmou_Range,file = "dat_ccrndmou_Range.csv")

#Billing adjusted total number of calls over the life of the customer
ds%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_adjqty
dat_adjqty$N<-unclass(ds%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat_adjqty$churn_perc<-dat_adjqty$n/dat_adjqty$N
dat_adjqty$GreaterThan<-unclass(ds%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat_adjqty$LessThan<-unclass(ds%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat_adjqty$varname<-rep("adjqty",nrow(dat_adjqty))
dat_adjqty
write.csv(x = dat_adjqty,file = "dat_adjqty.csv")

#Mean overage revenue
ds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat_ovrrev_Mean
dat_ovrrev_Mean$N<-unclass(ds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat_ovrrev_Mean$churn_perc<-dat_ovrrev_Mean$n/dat_ovrrev_Mean$N
dat_ovrrev_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat_ovrrev_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat_ovrrev_Mean$varname<-rep("ovrrev_Mean",nrow(dat_ovrrev_Mean))
dat_ovrrev_Mean
write.csv(x = dat_ovrrev_Mean,file = "dat_ovrrev_Mean.csv")

#Mean monthly revenue (charge amount)
ds%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_rev_Mean
dat_rev_Mean$N<-unclass(ds%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_rev_Mean$churn_perc<-dat_rev_Mean$n/dat_rev_Mean$N
dat_rev_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat_rev_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat_rev_Mean$varname<-rep("rev_Mean",nrow(dat_rev_Mean))
dat_rev_Mean
write.csv(x = dat_rev_Mean,file = "dat_rev_Mean.csv")


#Mean overage minutes of use
ds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat_ovrmou_Mean
dat_ovrmou_Mean$N<-unclass(ds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat_ovrmou_Mean$churn_perc<-dat_ovrmou_Mean$n/dat_ovrmou_Mean$N
dat_ovrmou_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat_ovrmou_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat_ovrmou_Mean$varname<-rep("ovrmou_Mean",nrow(dat_ovrmou_Mean))
dat_ovrmou_Mean
write.csv(x = dat_ovrmou_Mean,file = "dat_ovrmou_Mean.csv")

#Mean number of completed voice calls
ds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_comp_vce_Mean
dat_comp_vce_Mean$N<-unclass(ds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_comp_vce_Mean$churn_perc<-dat_comp_vce_Mean$n/dat_comp_vce_Mean$N
dat_comp_vce_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat_comp_vce_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat_comp_vce_Mean$varname<-rep("comp_vce_Mean",nrow(dat_comp_vce_Mean))
dat_comp_vce_Mean
write.csv(x = dat_comp_vce_Mean,file = "dat_comp_vce_Mean.csv")

#Mean number of attempted voice calls placed
ds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_plcd_vce_Mean
dat_plcd_vce_Mean$N<-unclass(ds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_plcd_vce_Mean$churn_perc<-dat_plcd_vce_Mean$n/dat_plcd_vce_Mean$N
dat_plcd_vce_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat_plcd_vce_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat_plcd_vce_Mean$varname<-rep("plcd_vce_Mean",nrow(dat_plcd_vce_Mean))
dat_plcd_vce_Mean
write.csv(x = dat_plcd_vce_Mean,file = "dat_plcd_vce_Mean.csv")

#Average monthly minutes of use over the previous three months
ds%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avg3mou
dat_avg3mou$N<-unclass(ds%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg3mou$churn_perc<-dat_avg3mou$n/dat_avg3mou$N
dat_avg3mou$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat_avg3mou$LessThan<-unclass(ds%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat_avg3mou$varname<-rep("avg3mou",nrow(dat_avg3mou))
dat_avg3mou
write.csv(x = dat_avg3mou,file = "dat_avg3mou.csv")

#Average monthly minutes of use over the life of the customer
ds%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avgmou
dat_avgmou$N<-unclass(ds%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat_avgmou$churn_perc<-dat_avgmou$n/dat_avgmou$N
dat_avgmou$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat_avgmou$LessThan<-unclass(ds%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat_avgmou$varname<-rep("avgmou",nrow(dat_avgmou))
dat_avgmou
write.csv(x = dat_avgmou,file = "dat_avgmou.csv")

#Average monthly number of calls over the previous three months
ds%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avg3qty
dat_avg3qty$N<-unclass(ds%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg3qty$churn_perc<-dat_avg3qty$n/dat_avg3qty$N
dat_avg3qty$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat_avg3qty$LessThan<-unclass(ds%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat_avg3qty$varname<-rep("avg3qty",nrow(dat_avg3qty))
dat_avg3qty
write.csv(x = dat_avg3qty,file = "dat_avg3qty.csv")

#Average monthly number of calls over the life of the customer
ds%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avgqty
dat_avgqty$N<-unclass(ds%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat_avgqty$churn_perc<-dat_avgqty$n/dat_avgqty$N
dat_avgqty$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat_avgqty$LessThan<-unclass(ds%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat_avgqty$varname<-rep("avgqty",nrow(dat_avgqty))
dat_avgqty
write.csv(x = dat_avgqty,file = "dat_avgqty.csv")

#Average monthly minutes of use over the previous six months
ds%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avg6mou
dat_avg6mou$N<-unclass(ds%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg6mou$churn_perc<-dat_avg6mou$n/dat_avg6mou$N
dat_avg6mou$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat_avg6mou$LessThan<-unclass(ds%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat_avg6mou$varname<-rep("avg6mou",nrow(dat_avg6mou))
dat_avg6mou
write.csv(x = dat_avg6mou,file = "dat_avg6mou.csv")


#Average monthly number of calls over the previous six months
ds%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avg6qty
dat_avg6qty$N<-unclass(ds%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat_avg6qty$churn_perc<-dat_avg6qty$n/dat_avg6qty$N
dat_avg6qty$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat_avg6qty$LessThan<-unclass(ds%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat_avg6qty$varname<-rep("avg6qty",nrow(dat_avg6qty))
dat_avg6qty
write.csv(x = dat_avg6qty,file = "dat_avg6qty.csv")


#Age of first household member
ds%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat_age1
dat_age1$N<-unclass(ds%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
dat_age1$churn_perc<-dat_age1$n/dat_age1$N
dat_age1$GreaterThan<-unclass(ds%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat_age1$LessThan<-unclass(ds%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat_age1$varname<-rep("age1",nrow(dat_age1))
dat_age1
write.csv(x = dat_age1,file = "dat_age1.csv")

#Age of second household member
ds%>%mutate(dec=ntile(age2,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_age2
dat_age2$N<-unclass(ds%>%mutate(dec=ntile(age2,n=2))%>%count(dec)%>%unname())[[2]]
dat_age2$churn_perc<-dat_age2$n/dat_age2$N
dat_age2$GreaterThan<-unclass(ds%>%mutate(dec=ntile(age2,n=2))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
dat_age2$LessThan<-unclass(ds%>%mutate(dec=ntile(age2,n=2))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
dat_age2$varname<-rep("age2",nrow(dat_age2))
dat_age2
write.csv(x = dat_age2,file = "dat_age2.csv")


#Number of models issued
ds%>%mutate(dec=ntile(models,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_models
dat_models$N<-unclass(ds%>%mutate(dec=ntile(models,n=2))%>%count(dec)%>%unname())[[2]]
dat_models$churn_perc<-dat_models$n/dat_models$N
dat_models$GreaterThan<-unclass(ds%>%mutate(dec=ntile(models,n=2))%>%group_by(dec)%>%summarise(min(models)))[[2]]
dat_models$LessThan<-unclass(ds%>%mutate(dec=ntile(models,n=2))%>%group_by(dec)%>%summarise(max(models)))[[2]]
dat_models$varname<-rep("models",nrow(dat_models))
dat_models
write.csv(x = dat_models,file = "dat_models.csv")

#Current handset price
ds%>%mutate(dec=ntile(hnd_price,n=6))%>%count(churn,dec)%>%filter(churn==1)->dat_hnd_price
dat_hnd_price$N<-unclass(ds%>%mutate(dec=ntile(hnd_price,n=6))%>%count(dec)%>%unname())[[2]]
dat_hnd_price$churn_perc<-dat_hnd_price$n/dat_hnd_price$N
dat_hnd_price$GreaterThan<-unclass(ds%>%mutate(dec=ntile(hnd_price,n=6))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat_hnd_price$LessThan<-unclass(ds%>%mutate(dec=ntile(hnd_price,n=6))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat_hnd_price$varname<-rep("hnd_price",nrow(dat_hnd_price))
dat_hnd_price
write.csv(x = dat_hnd_price,file = "dat_hnd_price.csv")


#Number of active subscribers in household
ds%>%mutate(dec=ntile(actvsubs,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_actvsubs
dat_actvsubs$N<-unclass(ds%>%mutate(dec=ntile(actvsubs,n=2))%>%count(dec)%>%unname())[[2]]
dat_actvsubs$churn_perc<-dat_actvsubs$n/dat_actvsubs$N
dat_actvsubs$GreaterThan<-unclass(ds%>%mutate(dec=ntile(actvsubs,n=2))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]]
dat_actvsubs$LessThan<-unclass(ds%>%mutate(dec=ntile(actvsubs,n=2))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]]
dat_actvsubs$varname<-rep("actvsubs",nrow(dat_actvsubs))
dat_actvsubs
write.csv(x = dat_actvsubs,file = "dat_actvsubs.csv")

#Number of unique subscribers in the household
ds%>%mutate(dec=ntile(uniqsubs,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_uniqsubs
dat_uniqsubs$N<-unclass(ds%>%mutate(dec=ntile(uniqsubs,n=2))%>%count(dec)%>%unname())[[2]]
dat_uniqsubs$churn_perc<-dat_uniqsubs$n/dat_uniqsubs$N
dat_uniqsubs$GreaterThan<-unclass(ds%>%mutate(dec=ntile(uniqsubs,n=2))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
dat_uniqsubs$LessThan<-unclass(ds%>%mutate(dec=ntile(uniqsubs,n=2))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
dat_uniqsubs$varname<-rep("uniqsubs",nrow(dat_uniqsubs))
dat_uniqsubs
write.csv(x = dat_uniqsubs,file = "dat_uniqsubs.csv")

#Mean number of off-peak data calls
ds%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_opk_dat_Mean
dat_opk_dat_Mean$N<-unclass(ds%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_opk_dat_Mean$churn_perc<-dat_opk_dat_Mean$n/dat_opk_dat_Mean$N
dat_opk_dat_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
dat_opk_dat_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(opk_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
dat_opk_dat_Mean$varname<-rep("opk_dat_Mean",nrow(dat_opk_dat_Mean))
dat_opk_dat_Mean
write.csv(x = dat_opk_dat_Mean,file = "dat_opk_dat_Mean.csv")


#Mean number of roaming calls
ds%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_roam_Mean
dat_roam_Mean$N<-unclass(ds%>%mutate(dec=ntile(roam_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_roam_Mean$churn_perc<-dat_roam_Mean$n/dat_roam_Mean$N
dat_roam_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
dat_roam_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(roam_Mean,n=2))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
dat_roam_Mean$varname<-rep("roam_Mean",nrow(dat_roam_Mean))
dat_roam_Mean
write.csv(x = dat_roam_Mean,file = "dat_roam_Mean.csv")

#Mean number of received SMS calls
ds%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_recv_sms_Mean
dat_recv_sms_Mean$N<-unclass(ds%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_recv_sms_Mean$churn_perc<-dat_recv_sms_Mean$n/dat_recv_sms_Mean$N
dat_recv_sms_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
dat_recv_sms_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(recv_sms_Mean,n=2))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
dat_recv_sms_Mean$varname<-rep("recv_sms_Mean",nrow(dat_recv_sms_Mean))
dat_recv_sms_Mean
write.csv(x = dat_recv_sms_Mean,file = "dat_recv_sms_Mean.csv")


#Mean number of blocked (failed) data calls
ds%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_blck_dat_Mean
dat_blck_dat_Mean$N<-unclass(ds%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_blck_dat_Mean$churn_perc<-dat_blck_dat_Mean$n/dat_blck_dat_Mean$N
dat_blck_dat_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat_blck_dat_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(blck_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat_blck_dat_Mean$varname<-rep("blck_dat_Mean",nrow(dat_blck_dat_Mean))
dat_blck_dat_Mean
write.csv(x = dat_blck_dat_Mean,file = "dat_blck_dat_Mean.csv")

#Mean unrounded minutes of use of peak data calls
ds%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_mou_pead_Mean
dat_mou_pead_Mean$N<-unclass(ds%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_mou_pead_Mean$churn_perc<-dat_mou_pead_Mean$n/dat_mou_pead_Mean$N
dat_mou_pead_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
dat_mou_pead_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(mou_pead_Mean,n=2))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
dat_mou_pead_Mean$varname<-rep("mou_pead_Mean",nrow(dat_mou_pead_Mean))
dat_mou_pead_Mean
write.csv(x = dat_mou_pead_Mean,file = "dat_mou_pead_Mean.csv")

#Mean number of directory assisted calls
ds%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat_da_Mean
dat_da_Mean$N<-unclass(ds%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
dat_da_Mean$churn_perc<-dat_da_Mean$n/dat_da_Mean$N
dat_da_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat_da_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat_da_Mean$varname<-rep("da_Mean",nrow(dat_da_Mean))
dat_da_Mean
write.csv(x = dat_da_Mean,file = "dat_da_Mean.csv")

#Range of number of directory assisted calls
ds%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)->dat_da_Range
dat_da_Range$N<-unclass(ds%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
dat_da_Range$churn_perc<-dat_da_Range$n/dat_da_Range$N
dat_da_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat_da_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat_da_Range$varname<-rep("da_Range",nrow(dat_da_Range))
dat_da_Range
write.csv(x = dat_da_Range,file = "dat_da_Range.csv")

#Mean revenue of data overage
ds%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_datovr_Mean
dat_datovr_Mean$N<-unclass(ds%>%mutate(dec=ntile(datovr_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_datovr_Mean$churn_perc<-dat_datovr_Mean$n/dat_datovr_Mean$N
dat_datovr_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat_datovr_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(datovr_Mean,n=2))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
dat_datovr_Mean$varname<-rep("datovr_Mean",nrow(dat_datovr_Mean))
dat_datovr_Mean
write.csv(x = dat_datovr_Mean,file = "dat_datovr_Mean.csv")


#Range of revenue of data overage
ds%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_datovr_Range
dat_datovr_Range$N<-unclass(ds%>%mutate(dec=ntile(datovr_Range,n=2))%>%count(dec)%>%unname())[[2]]
dat_datovr_Range$churn_perc<-dat_datovr_Range$n/dat_datovr_Range$N
dat_datovr_Range$GreaterThan<-unclass(ds%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat_datovr_Range$LessThan<-unclass(ds%>%mutate(dec=ntile(datovr_Range,n=2))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
dat_datovr_Range$varname<-rep("datovr_Range",nrow(dat_datovr_Range))
dat_datovr_Range
write.csv(x = dat_datovr_Range,file = "dat_datovr_Range.csv")

#Mean number of dropped (failed) data calls
ds%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_drop_dat_Mean
dat_drop_dat_Mean$N<-unclass(ds%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_drop_dat_Mean$churn_perc<-dat_drop_dat_Mean$n/dat_drop_dat_Mean$N
dat_drop_dat_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
dat_drop_dat_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(drop_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
dat_drop_dat_Mean$varname<-rep("drop_dat_Mean",nrow(dat_drop_dat_Mean))
dat_drop_dat_Mean
write.csv(x = dat_drop_dat_Mean,file = "dat_drop_dat_Mean.csv")

#Mean number of dropped (failed) voice calls
ds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_drop_vce_Mean
dat_drop_vce_Mean$N<-unclass(ds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_drop_vce_Mean$churn_perc<-dat_drop_vce_Mean$n/dat_drop_vce_Mean$N
dat_drop_vce_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat_drop_vce_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat_drop_vce_Mean$varname<-rep("drop_vce_Mean",nrow(dat_drop_vce_Mean))
dat_drop_vce_Mean
write.csv(x = dat_drop_vce_Mean,file = "dat_drop_vce_Mean.csv")

#Billing adjusted total minutes of use over the life of the customer
ds%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_adjmou
dat_adjmou$N<-unclass(ds%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat_adjmou$churn_perc<-dat_adjmou$n/dat_adjmou$N
dat_adjmou$GreaterThan<-unclass(ds%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat_adjmou$LessThan<-unclass(ds%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat_adjmou$varname<-rep("adjmou",nrow(dat_adjmou))
dat_adjmou
write.csv(x = dat_adjmou,file = "dat_adjmou.csv")


#Total revenue
ds%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_totrev
dat_totrev$N<-unclass(ds%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_totrev$churn_perc<-dat_totrev$n/dat_totrev$N
dat_totrev$GreaterThan<-unclass(ds%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat_totrev$LessThan<-unclass(ds%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat_totrev$varname<-rep("totrev",nrow(dat_totrev))
dat_totrev
write.csv(x = dat_totrev,file = "dat_totrev.csv")


#Billing adjusted total revenue over the life of the customer
ds%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_adjrev
dat_adjrev$N<-unclass(ds%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_adjrev$churn_perc<-dat_adjrev$n/dat_adjrev$N
dat_adjrev$GreaterThan<-unclass(ds%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat_adjrev$LessThan<-unclass(ds%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat_adjrev$varname<-rep("adjrev",nrow(dat_adjrev))
dat_adjrev
write.csv(x = dat_adjrev,file = "dat_adjrev.csv")


#Average monthly revenue over the life of the customer
ds%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_avgrev
dat_avgrev$N<-unclass(ds%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_avgrev$churn_perc<-dat_avgrev$n/dat_avgrev$N
dat_avgrev$GreaterThan<-unclass(ds%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat_avgrev$LessThan<-unclass(ds%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat_avgrev$varname<-rep("avgrev",nrow(dat_avgrev))
dat_avgrev
write.csv(x = dat_avgrev,file = "dat_avgrev.csv")



#Mean number of completed data calls
ds%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_comp_dat_Mean
dat_comp_dat_Mean$N<-unclass(ds%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_comp_dat_Mean$churn_perc<-dat_comp_dat_Mean$n/dat_comp_dat_Mean$N
dat_comp_dat_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]]
dat_comp_dat_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(comp_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]]
dat_comp_dat_Mean$varname<-rep("comp_dat_Mean",nrow(dat_comp_dat_Mean))
dat_comp_dat_Mean
write.csv(x = dat_comp_dat_Mean,file = "dat_comp_dat_Mean.csv")

#Mean number of attempted data calls placed
ds%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%count(churn,dec)%>%filter(churn==1)->dat_plcd_dat_Mean
dat_plcd_dat_Mean$N<-unclass(ds%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%count(dec)%>%unname())[[2]]
dat_plcd_dat_Mean$churn_perc<-dat_plcd_dat_Mean$n/dat_plcd_dat_Mean$N
dat_plcd_dat_Mean$GreaterThan<-unclass(ds%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean)))[[2]]
dat_plcd_dat_Mean$LessThan<-unclass(ds%>%mutate(dec=ntile(plcd_dat_Mean,n=2))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean)))[[2]]
dat_plcd_dat_Mean$varname<-rep("plcd_dat_Mean",nrow(dat_plcd_dat_Mean))
dat_plcd_dat_Mean
write.csv(x = dat_plcd_dat_Mean,file = "dat_plcd_dat_Mean.csv")



#################################################################################################################################################################
#
#Catagorical Variable Profiling
####################################################
#Credit class code
ds%>%count(churn,levels=crclscod)%>%filter(churn==1)->dat_crclscod
dat_crclscod$N<-unclass(ds%>%filter(crclscod%in%dat_crclscod$levels)%>%count(crclscod))[[2]]
dat_crclscod$ChurnPerc<-dat_crclscod$n/dat_crclscod$N
dat_crclscod$Var.Name<-rep("crclscod",nrow(dat_crclscod))
dat_crclscod
write.csv(x = dat_crclscod,file = "dat_crclscod.csv")

#Account spending limit
ds%>%count(churn,levels=asl_flag)%>%filter(churn==1)->dat_asl_flag
dat_asl_flag$N<-unclass(ds%>%filter(asl_flag%in%dat_asl_flag$levels)%>%count(asl_flag))[[2]]
dat_asl_flag$ChurnPerc<-dat_asl_flag$n/dat_asl_flag$N
dat_asl_flag$Var.Name<-rep("asl_flag",nrow(dat_asl_flag))
dat_asl_flag
write.csv(x = dat_asl_flag,file = "dat_asl_flag.csv")

#Social group letter only
ds%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->dat_prizm_social_one
dat_prizm_social_one$N<-unclass(ds%>%filter(prizm_social_one%in%dat_prizm_social_one$levels)%>%count(prizm_social_one))[[2]]
dat_prizm_social_one$ChurnPerc<-dat_prizm_social_one$n/dat_prizm_social_one$N
dat_prizm_social_one$Var.Name<-rep("prizm_social_one",nrow(dat_prizm_social_one))
dat_prizm_social_one
write.csv(x = dat_prizm_social_one,file = "dat_prizm_social_one.csv")


#Geographic area
ds%>%count(churn,levels=area)%>%filter(churn==1)->dat_area
dat_area$N<-unclass(ds%>%filter(area%in%dat_area$levels)%>%count(area))[[2]]
dat_area$ChurnPerc<-dat_area$n/dat_area$N
dat_area$Var.Name<-rep("area",nrow(dat_area))
dat_area
write.csv(x = dat_area,file = "dat_area.csv")

#Handset: refurbished or new
ds%>%count(churn,levels=refurb_new)%>%filter(churn==1)->dat_refurb_new
dat_refurb_new$N<-unclass(ds%>%filter(refurb_new%in%dat_refurb_new$levels)%>%count(refurb_new))[[2]]
dat_refurb_new$ChurnPerc<-dat_refurb_new$n/dat_refurb_new$N
dat_refurb_new$Var.Name<-rep("refurb_new",nrow(dat_refurb_new))
dat_refurb_new
write.csv(x = dat_refurb_new,file = "dat_refurb_new.csv")

#Handset web capability
ds%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->dat_hnd_webcap
dat_hnd_webcap$N<-unclass(ds%>%filter(hnd_webcap%in%dat_hnd_webcap$levels)%>%count(hnd_webcap))[[2]]
dat_hnd_webcap$ChurnPerc<-dat_hnd_webcap$n/dat_hnd_webcap$N
dat_hnd_webcap$Var.Name<-rep("hnd_webcap",nrow(dat_hnd_webcap))
dat_hnd_webcap
write.csv(x = dat_hnd_webcap,file = "dat_hnd_webcap.csv")

#Marital status
ds%>%count(churn,levels=marital)%>%filter(churn==1)->dat_marital
dat_marital$N<-unclass(ds%>%filter(marital%in%dat_marital$levels)%>%count(marital))[[2]]
dat_marital$ChurnPerc<-dat_marital$n/dat_marital$N
dat_marital$Var.Name<-rep("marital",nrow(dat_marital))
dat_marital
write.csv(x = dat_marital,file = "dat_marital.csv")

#Ethnicity roll-up code
ds%>%count(churn,levels=ethnic)%>%filter(churn==1)->dat_ethnic
dat_ethnic$N<-unclass(ds%>%filter(ethnic%in%dat_ethnic$levels)%>%count(ethnic))[[2]]
dat_ethnic$ChurnPerc<-dat_ethnic$n/dat_ethnic$N
dat_ethnic$Var.Name<-rep("ethnic",nrow(dat_ethnic))
dat_ethnic
write.csv(x = dat_ethnic,file = "dat_ethnic.csv")

#New or used car buyer
ds%>%count(churn,levels=car_buy)%>%filter(churn==1)->dat_car_buy
dat_car_buy$N<-unclass(ds%>%filter(car_buy%in%dat_car_buy$levels)%>%count(car_buy))[[2]]
dat_car_buy$ChurnPerc<-dat_car_buy$n/dat_car_buy$N
dat_car_buy$Var.Name<-rep("car_buy",nrow(dat_car_buy))
dat_car_buy
write.csv(x = dat_car_buy,file = "dat_car_buy.csv")

#Communications local service area
ds%>%count(churn,levels=csa)%>%filter(churn==1)->dat_csa
dat_csa$N<-unclass(ds%>%filter(csa%in%dat_csa$levels)%>%count(csa))[[2]]
dat_csa$ChurnPerc<-dat_csa$n/dat_csa$N
dat_csa$Var.Name<-rep("csa",nrow(dat_csa))
dat_csa
write.csv(x = dat_csa,file = "dat_csa.csv")

ds$forgntvl<-as.factor(ds$forgntvl)
ds$mtrcycle<-as.factor(ds$mtrcycle)
ds$truck<-as.factor(ds$truck)

#Foreign travel dummy variable
ds%>%count(churn,levels=forgntvl)%>%filter(churn==1)->dat_forgntvl
dat_forgntvl$N<-unclass(ds%>%filter(forgntvl%in%dat_forgntvl$levels)%>%count(forgntvl))[[2]]
dat_forgntvl$ChurnPerc<-dat_forgntvl$n/dat_forgntvl$N
dat_forgntvl$Var.Name<-rep("forgntvl",nrow(dat_forgntvl))
dat_forgntvl
write.csv(x = dat_forgntvl,file = "dat_forgntvl.csv")


#Motorcycle indicator
ds%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->dat_mtrcycle
dat_mtrcycle$N<-unclass(ds%>%filter(mtrcycle%in%dat_mtrcycle$levels)%>%count(mtrcycle))[[2]]
dat_mtrcycle$ChurnPerc<-dat_mtrcycle$n/dat_mtrcycle$N
dat_mtrcycle$Var.Name<-rep("mtrcycle",nrow(dat_mtrcycle))
dat_mtrcycle
write.csv(x = dat_mtrcycle,file = "dat_mtrcycle.csv")

#Truck indicator
ds%>%count(churn,levels=truck)%>%filter(churn==1)->dat_truck
dat_truck$N<-unclass(ds%>%filter(truck%in%dat_truck$levels)%>%count(truck))[[2]]
dat_truck$ChurnPerc<-dat_truck$n/dat_truck$N
dat_truck$Var.Name<-rep("truck",nrow(dat_truck))
dat_truck
write.csv(x = dat_truck,file = "dat_truck.csv")

#Missing value imputations
indx<-which(is.na(ds$mou_Mean))
ds<-ds[-indx,]
colSums(is.na(ds))
mean(ds$change_mou,na.rm = TRUE)

indx<-which(is.na(ds$change_mou))
ds[indx,]$change_mou<- mean(ds$change_mou,na.rm = TRUE)

indx<-which(is.na(ds$avg6mou))
ds[indx,]$avg6mou<-mean(ds$avg6mou,na.rm = TRUE)

indx<-which(is.na(ds$avg6qty))
ds[indx,]$avg6qty<-mean(ds$avg6qty,na.rm = TRUE)


str(ds)
indx<-which(is.na(ds$area))
ds$area<-as.character(ds$area)
ds[indx,]$area<-"Unknown"
ds$area<- as.factor(ds$area)


indx<-which(is.na(ds$hnd_webcap))
ds$hnd_webcap<-as.character(ds$hnd_webcap)
ds[indx,]$hnd_webcap<-"WNC"
indx<-which(ds$hnd_webcap=="WCMB")
ds[indx,]$hnd_webcap<-"WC"
ds$hnd_webcap<- as.factor(ds$hnd_webcap)

colSums(is.na(ds))

indx<-which(ds$prizm_social_one=="S")
ds$prizm_social_one<-as.character(ds$prizm_social_one)
ds[indx,]$prizm_social_one<-"R"
indx<-which(ds$prizm_social_one=="C")
ds[indx,]$prizm_social_one<-"U"
indx<-which(ds$prizm_social_one=="T")
ds[indx,]$prizm_social_one<-"U"
indx<-which(is.na(ds$prizm_social_one))
ds[indx,]$prizm_social_one<-"Unknown"
ds$prizm_social_one<- as.factor(ds$prizm_social_one)

indx<-which(ds$marital=="B")
ds$marital<-as.character(ds$marital)
ds[indx,]$marital<-"S"
indx<-which(ds$marital=="A")
ds[indx,]$marital<-"M"
indx<-which(is.na(ds$marital))
ds[indx,]$marital<-"U"
ds$marital<- as.factor(ds$marital)

colSums(is.na(ds))
str(ds$hnd_price)
indx<-which(is.na(ds$hnd_price))
ds[indx,]$hnd_price <-64.98999

ds$forgntvl<-as.character(ds$forgntvl)
indx<-which(is.na(ds$forgntvl))
ds[indx,]$forgntvl<-2
ds$forgntvl<-as.factor(ds$forgntvl)

ds$mtrcycle<-as.character(ds$mtrcycle)
indx<-which(is.na(ds$mtrcycle))
ds[indx,]$mtrcycle<-2
ds$mtrcycle<-as.factor(ds$mtrcycle)

ds$truck<-as.character(ds$truck)
indx<-which(is.na(ds$truck))
ds[indx,]$truck<-2
ds$truck<-as.factor(ds$truck)

ds$car_buy<-as.character(ds$car_buy)
indx<-which(is.na(ds$car_buy))
ds[indx,]$car_buy<-"UNKNOWN"
ds$car_buy<-as.factor(ds$car_buy)

ds$ethnic<-as.character(ds$ethnic)
indx<-which(is.na(ds$ethnic))
ds[indx,]$ethnic<-"A"
ds$ethnic<-as.factor(ds$ethnic)


indx<-which(is.na(ds$age1))
ds[indx,]$age1<-31


indx<-which(is.na(ds$age2))
ds[indx,]$age2<-0

#Data Preparation

ds$callwait_Mean_dummy<-0
indx<- which(ds$callwait_Mean<=0.33)
ds$callwait_Mean_dummy[indx]<-1
indx<- which(ds$callwait_Mean<=0.33)
ds$callwait_Mean_dummy[indx]<-2
indx<- which(ds$callwait_Mean>0.33)
ds$callwait_Mean_dummy[indx]<-3
ds$callwait_Mean<-NULL

#Creating dummies for model building
ds$roam_Mean<-ifelse(ds$roam_Mean>0,1,0)
ds$truck<- ifelse(ds$truck==1,1,0)
ds$refurb_new<-ifelse(ds$refurb_new=='R',1,0)
ds$ethnicB<-ifelse(ds$ethnic=='B',1,0)
ds$ethnicC<-ifelse(ds$ethnic=='C',1,0)
ds$ethnicD<-ifelse(ds$ethnic=='D',1,0)
ds$ethnicO<-ifelse(ds$ethnic=='O',1,0)
ds$ethnicZ<-ifelse(ds$ethnic=='Z',1,0)
ds$ethnicR<-ifelse(ds$ethnic=='R',1,0)
ds$ethnic<-NULL


ds$area_SOUTH_FLORIDA_AREA<-ifelse(ds$area=='SOUTH FLORIDA AREA',1,0)
ds$area_NORTHWEST_ROCKY_MOUNTAIN_AREA<-ifelse(ds$area=='NORTHWEST/ROCKY MOUNTAIN AREA',1,0)
ds$area_MIDWEST_AREA<-ifelse(ds$area=='MIDWEST AREA',1,0)
ds$area_CENTRAL_SOUTH_TEXAS_AREA<-ifelse(ds$area=='CENTRAL/SOUTH TEXAS AREA',1,0)
ds$area<-NULL

#dividing data into train and test in 70-30 ratio
set.seed(200)
index<- sample(nrow(ds),0.7*nrow(ds),replace = FALSE)
test<- ds[-index,]
train<- ds[index,]
nrow(test)
nrow(train)

#Checking for correct sampling data
table(ds$churn)/nrow(ds)
table(test$churn)/nrow(test)
table(train$churn)/nrow(train)


mod<- glm(churn~.,data = train,family = binomial )

summary(mod)

#step(mod,direction="backward")

mod<- glm(churn~totmrc_Mean+mou_Range+mou_Range+drop_blk_Mean+
            owylis_vce_Range+months+eqpdays+iwylis_vce_Mean+
            ovrrev_Mean+rev_Mean+comp_vce_Mean+avg3mou+avgmou+
            avg6mou+avg6qty+asl_flag+prizm_social_one+area+
            refurb_new+ethnic+
            age1+models+hnd_price+actvsubs+
            uniqsubs+truck+mou_pead_Mean+drop_vce_Mean+adjmou
            ,data = train,family = binomial )

mod<- glm(churn~totmrc_Mean+mou_Range+mou_Range+
            owylis_vce_Range+months+eqpdays+iwylis_vce_Mean+
            rev_Mean+comp_vce_Mean+avg3mou+avgmou+
            avg6qty+asl_flag+area+
            refurb_new+ethnic+age1+models+hnd_price+actvsubs+
            uniqsubs+truck+drop_vce_Mean
          ,data = train,family = binomial )



summary(mod)

mod<- glm(churn~totmrc_Mean+mou_Range+mou_Range+
            owylis_vce_Range+months+eqpdays+iwylis_vce_Mean+
            rev_Mean+comp_vce_Mean+avg3mou+avgmou+
            avg6qty+asl_flag+area_SOUTH_FLORIDA_AREA+area_NORTHWEST_ROCKY_MOUNTAIN_AREA+
            area_MIDWEST_AREA+area_CENTRAL_SOUTH_TEXAS_AREA+
            refurb_new+ethnicB+ethnicC+ethnicD+ethnicO+ethnicZ+ethnicR+
            age1+models+hnd_price+actvsubs+
            uniqsubs+truck+drop_vce_Mean
          ,data = train,family = binomial )

summary(mod)
#Final Model
mod<- glm(churn~totmrc_Mean+mou_Range+mou_Range+
            owylis_vce_Range+months+eqpdays+iwylis_vce_Mean+
            rev_Mean+comp_vce_Mean+avg3mou+avgmou+#avg6qty+
            asl_flag+area_SOUTH_FLORIDA_AREA+area_NORTHWEST_ROCKY_MOUNTAIN_AREA+
            refurb_new+ethnicB+ethnicC+ethnicO+ethnicZ+
            age1+models+hnd_price+actvsubs+
            uniqsubs+drop_vce_Mean
          ,data = train,family = binomial )

summary(mod)
#confint(mod)
head(mod$fitted.values)
pred<- predict(mod,type = "response",newdata = test)
table(test$churn)/nrow(test)

#summary(pred)
cust_levels<-ifelse(pred>=0.5,2,ifelse(pred>=0.2388263,1,0))
pred<-ifelse(pred>=0.2388263,1,0)


#table(pred)

library(gains)
library(caret)
library(irr)
library(ROCR)

confusionMatrix(pred,test$churn,positive = "1")
kappa2(data.frame(test$churn,pred))
gains(test$churn,predict(mod,type = "response",newdata = test),groups = 10)

#Finding AUC of the model
predf<-prediction(mod$fitted.values,train$churn)
perf<-performance(predf,'tpr','fpr')
plot(perf)
abline(0,1,lty=8,col="red")

auc<-performance(predf,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

#alias(glm(churn~.,data = ds,family = binomial ))
#head(ds[ds$truck==2,])

#Creating variables for customer targeting


cust_type<-ifelse(test$rev_Mean>79.23,"High",ifelse(test$rev_Mean>48.61 & test$rev_Mean<79.23,"Medium","Low"))

target_cust<-ifelse(((cust_levels==2|cust_levels==1) & (cust_type=="High"|cust_type=="Medium")),1,0)
test$target_cust<-target_cust
head(target_cust)
head(cust_levels)
head(cust_type)

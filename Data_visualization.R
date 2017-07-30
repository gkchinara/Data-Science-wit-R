setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 6.2 -  Data Visualization in R")
library(XML)
library(RCurl)
htmlContent<- getURL("file:///C:/Data%20Science%20with%20R/Assignments/Graded%20Assignments/Topic%206.2%20-%20%20Data%20Visualization%20in%20R/The%20World's%20Most%20Valuable%20Brands%20List%20-%20Forbes.html")


table<- readHTMLTable("file:///C:/Data%20Science%20with%20R/Assignments/Graded%20Assignments/Topic%206.2%20-%20%20Data%20Visualization%20in%20R/The%20World's%20Most%20Valuable%20Brands%20List%20-%20Forbes.html")
table
str(table)
table<-table$the_list
head(table)
table$Brand
table$`Brand Value`
table$`Brand Value`<- gsub("B","",table$`Brand Value`)
table$`Brand Value`<- gsub("\\$","",table$`Brand Value`)
table$Rank
table$Rank<- gsub("#","",table$Rank)
table$`Brand Revenue`
table$`Brand Revenue`<- gsub("B","",table$`Brand Revenue`)
table$`Brand Revenue`<- gsub("\\$","",table$`Brand Revenue`)
table$`Brand Revenue`<- as.numeric(table$`Brand Revenue`)
table$`Brand Revenue`

table$`Company Advertising`
table$`Company Advertising`<- gsub("\\$","",table$`Company Advertising`)
million_val<- grepl("M",table$`Company Advertising`)
million_val
table$`Company Advertising`<- gsub("M","",table$`Company Advertising`)
table$`Company Advertising`<- gsub("B","",table$`Company Advertising`)

table$`Company Advertising`<- as.numeric(table$`Company Advertising`)
table$`Company Advertising`[million_val]<-table$`Company Advertising`[million_val]*0.001
table$`Company Advertising`

table$`Brand Value`<- as.numeric(table$`Brand Value`)

library(dplyr)
library(ggplot2)

table$Industry
table<- table[,-c(1)]
names(table)

table%>%filter(Industry=="Technology")->Tech
table%>%filter(Industry=="Luxury")->Luxury
table%>%filter(Industry=="Financial Services")->Finance
table%>%filter(Industry=="Automotive")->Automotive


head(Tech)
Tech$`Company Advertising`
vis_tech<-ggplot(Tech,aes(x = Tech$`Company Advertising`,
                          y = Tech$`Brand Revenue`,color=Tech$`Brand Value`,
                          label= Tech$Brand
                          ))+geom_point(aes(cex=as.numeric(Tech$`Brand Value`,cex.lab=0.3,cex.main=3)))
                          
vis_tech+scale_color_continuous(guide=FALSE)+geom_text(check_overlap = FALSE,vjust=0,nudge_y = -10)+labs(
  title="Technology",
  x="Company Advertising in Billion of $",
  y="Brand Advertising in Billions of $"
  ,cex="Brand Value $(Billions)")+theme_bw()+theme(panel.background= element_rect(fill = "white"),
        panel.grid.major= element_line(colour = "grey40"),
        panel.grid.minor= element_line(colour = "grey"),
        axis.text.y= element_text())

#Represenation for Industry=="Luxury"
head(Luxury)
str(Luxury)
vis_lux<-ggplot(Luxury,aes(x = Luxury$`Company Advertising`,
                          y = Luxury$`Brand Revenue`,color=Luxury$`Brand Value`,
                          label= Luxury$Brand))+geom_point(aes(cex=as.numeric(Luxury$`Brand Value`)))
vis_lux+scale_color_continuous(guide=FALSE)+ geom_text(check_overlap = FALSE,vjust=-2,nudge_y = -1)+labs(
        title="Luxury",
  x="Company Advertising in Billion of $",
  y="Brand Advertising in Billions of $"
  ,cex="Brand Value $(Billions)")+theme_bw()+theme(panel.background= element_rect(fill = "white"),
                                                   panel.grid.major= element_line(colour = "grey40"),
                                                   panel.grid.minor= element_line(colour = "grey"),
                                                   axis.text.y= element_text())

head(Finance)
str(Finance)
vis_lux<-ggplot(Finance,aes(x = Finance$`Company Advertising`,
                           y = Finance$`Brand Revenue`,color=Finance$`Brand Value`,
                           label= Finance$Brand))+geom_point(aes(cex=as.numeric(Finance$`Brand Value`)))
vis_lux+scale_color_continuous(guide=FALSE)+ geom_text(check_overlap = FALSE,vjust=1,nudge_y = -1)+labs(
  title="Finance",
  x="Company Advertising in Billion of $",
  y="Brand Advertising in Billions of $"
  ,cex="Brand Value $(Billions)")+theme_bw()+theme(panel.background= element_rect(fill = "white"),
                                                   panel.grid.major= element_line(colour = "grey40"),
                                                   panel.grid.minor= element_line(colour = "grey"),
                                                   axis.text.y= element_text())


#Automotive industry visualization
head(Automotive)
str(Automotive)
vis_lux<-ggplot(Automotive,aes(x = Automotive$`Company Advertising`,
                            y = Automotive$`Brand Revenue`,color=Automotive$`Brand Value`,
                            label= Automotive$Brand))+geom_point(aes(cex=as.numeric(Automotive$`Brand Value`)))
vis_lux+scale_color_continuous(guide=FALSE)+ geom_text(check_overlap = FALSE,vjust=1,nudge_y = -1)+labs(
  title="Automotive",
  x="Company Advertising in Billion of $",
  y="Brand Advertising in Billions of $"
  ,cex="Brand Value $(Billions)")+theme_bw()+theme(panel.background= element_rect(fill = "white"),
                                                   panel.grid.major= element_line(colour = "grey40"),
                                                   panel.grid.minor= element_line(colour = "grey"),
                                                   axis.text.y= element_text())
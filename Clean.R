library(dplyr)
library(shiny)

setwd("C:/Users/193344/Desktop/TRANS SHINY")
TRAN <- read.delim("//knx3fs01/ed_ba_group/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/TRAN.txt",)
Today <- read.csv("//knx3fs01/ED_BA_GROUP/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/Today.csv", header=FALSE)
HEADER <- read.csv("//knx3fs01/ED_BA_GROUP/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/HEADER.csv", header=FALSE,stringsAsFactors=FALSE)
colnames(Today) <- HEADER[1,]

TRAN$PostTms <- as.Date(TRAN$PostTms,format="%Y-%m-%d")
TRAN$EffDt <- as.Date(TRAN$EffDt,format="%Y-%m-%d")

Today$PostTms <- as.Date(Today$PostTms,format="%m/%d/%Y")
Today$EffDt <- as.Date(Today$EffDt,format="%m/%d/%Y")

Today <- Today %>%
  mutate(CommAmt = ifelse(is.na(CommAmt),0,CommAmt))

TRAN <- TRAN[,names(TRAN)!="Month"]

TRAN <- rbind(TRAN,Today)

TRAN$SpecialCommAmt <- as.numeric(as.character(TRAN$SpecialCommAmt))

TRAN$Month <- as.factor(format(TRAN$PostTms,"%B %Y"))


order<-c("January 2015",
         "February 2015",
         "March 2015",
         "April 2015",
         "May 2015",
         "June 2015",
         "July 2015",
         "August 2015",
         "September 2015",
         "October 2015",
         "November 2015",  
         "December 2015",
         "January 2016",
         "February 2016",
         "March 2016",
         "April 2016",
         "May 2016",
         "June 2016")

TRAN$Month<-factor(TRAN$Month,levels=order)


library(plyr)
TRAN$PostedClientCode <- revalue(TRAN$PostedClientCode,c("7222"="Knoxville","8035"="Knoxville-Old",
                                                         "C7222"="Columbus","C8035"="Columbus-Old","A7222"="Kennesaw",
                                                         "A8035"="Kennesaw-Old","B7222"="Columbus 2","D7222"="Defiance",
                                                         "T8035"="Tucson-Old","W7222"="Westlake"))
detach("package:plyr", unload=TRUE)

Direct <- subset(TRAN,FundsType%in%c("RGVO","RVVO","SB"))
AWG <- subset(TRAN,FundsType%in%c("RGWG","RVWG"))
RHB <- subset(TRAN,FundsType%in%c("RGRH","RVRH","WORH"))

MTD <- TRAN %>%
  group_by(FundsType,Month) %>%
  summarise(Transaction_Amt = sum(TransAmt),
            Commission_Amt = sum(CommAmt))


Summary1 <- Direct %>%
  group_by(PostedClientCode,Month,add=T) %>%
  summarise(DC = round(sum(CommAmt),2))

Summary2 <- AWG %>%
  group_by(PostedClientCode,Month,add=T) %>%
  summarise(AWG = round(sum(CommAmt),2))

Summary3 <- RHB %>%
  group_by(PostedClientCode,Month,add=T) %>%
  summarise(RHB = round(sum(CommAmt),2))

detach("package:dplyr", unload=TRUE)
library(plyr)

Summarya <- join(Summary1,Summary2,by=c("PostedClientCode","Month"))
Summary <- join(Summarya,Summary3,by=c("PostedClientCode","Month"))
row.names(Summary) <- NULL

detach("package:plyr", unload=TRUE)
library(dplyr)

Summary[is.na(Summary)]<-0

Daily_Summary1 <- Direct %>%
  group_by(PostedClientCode,Month,PostTms,add=T) %>%
  summarise(DC = round(sum(TransAmt)*.152,2))

Daily_Summary2 <- AWG %>%
  group_by(PostedClientCode,Month,PostTms,add=T) %>%
  summarise(AWG = round(sum(TransAmt)*.152,2))

Daily_Summary3 <- RHB %>%
  group_by(PostedClientCode,Month,PostTms,add=T) %>%
  summarise(RHB = round(sum(CommAmt),2))

detach("package:dplyr", unload=TRUE)
library(plyr)

Daily_Summarya <- join(Daily_Summary1,Daily_Summary2,by=c("PostedClientCode","Month","PostTms"))
Daily_Summary <- join(Daily_Summarya,Daily_Summary3,by=c("PostedClientCode","Month","PostTms"))
row.names(Summary) <- NULL

detach("package:plyr", unload=TRUE)
library(dplyr)

Daily_Summary[is.na(Daily_Summary)]<-0


write.table(TRAN,"//knx3fs01/ED_BA_GROUP/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/TRACKERFILE.txt",sep="\t")

#write.table(TRAN,"//knx3fs01/ED_BA_GROUP/Lowhorn/Monthly Transaction Detail Report/Transaction Detail Dashboard/TRAN.txt",sep="\t")

write.table(TRAN,"//knx3it/AWG Management/Lowhorn/MTD Database/TRACKERFILE.txt",sep="\t")

runApp(host="0.0.0.0",port=5050)

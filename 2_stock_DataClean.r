########################################################
# Data Cleaning for Netflix Stock price and NASDAQ100 index
###################################################
## NASDAQ 100 DATASET FROM 2012-12-31 to 2021-09-20
##
## read csv NASDAQ 100 stocks
nda100<-read.csv("NDX.csv")
head(nda100)

keep<-c("Date","Open","Close")
nda100<-nda100[keep]

nda100$week<-seq.int(nrow(nda100))

## compare the price changes from week open to week close
nda100["changesOtoC"]<-as.numeric(sub("%","",round((nda100$Close-nda100$Open)/nda100$Open,3)))

## compare the price changes from previous week close to week close 
percentChangeCtoC10013 <- (nda100$Close - lag(nda100$Close))/nda100$Close
percentChangeCtoC10013[is.na(percentChangeCtoC10013)]<-0
nda100["changesCtoC"]<-as.numeric(sub("%","",round(percentChangeCtoC10013,3)))

head(nda100)
## summary of the table
summary(nda100)

## change the last two columns into percentage 
nda100["changesOtoC"]<-paste(as.numeric(sub("%","",100*round((nda100$Close-nda100$Open)/nda100$Open,4))),"%",sep="")
nda100["changesCtoC"]<-paste(as.numeric(sub("%","",100*round(percentChangeCtoC10013,4))),"%",sep="")

head(nda100,10)
tail(nda100,10)

nda100<-nda100[c("Date","week","changesCtoC")]
head(nda100)

###################################################
## NETFLIX STOCK PRICE FROM 2012-12-31 TO 2021-09-20
## read csv NFLX stock
NFLX2013<-read.csv("NFLX.csv")

head(NFLX2013)

keep<-c("Date","Open","Close")
NFLX2013<-NFLX2013[keep]

NFLX2013$week<-seq.int(nrow(NFLX2013))

## compare the price changes from week open to week close
NFLX2013["changesOtoC"]<-as.numeric(sub("%","",round((NFLX2013$Close-NFLX2013$Open)/NFLX2013$Open,3)))

## compare the price changes from previous week close to week close 
percentChangeCtoCN13 <- (NFLX2013$Close - lag(NFLX2013$Close))/NFLX2013$Close
percentChangeCtoCN13[is.na(percentChangeCtoCN13)]<-0
NFLX2013["changesCtoC"]<-as.numeric(sub("%","",round(percentChangeCtoCN13,3)))

head(NFLX2013)
## summary of the table
summary(NFLX2013)

## change the last two columns into percentage 
NFLX2013["changesOtoC"]<-paste(as.numeric(sub("%","",100*round((NFLX2013$Close-NFLX2013$Open)/NFLX2013$Open,4))),"%",sep="")
NFLX2013["changesCtoC"]<-paste(as.numeric(sub("%","",100*round(percentChangeCtoCN13,4))),"%",sep="")

head(NFLX2013,10)
tail(NFLX2013,10)

NFLX2013<-NFLX2013[c("Date","week","changesCtoC")]
head(NFLX2013)




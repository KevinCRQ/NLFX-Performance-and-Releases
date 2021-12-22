##1. Data Cleaning
library(readr)
series=read.csv('tv_processed.csv')
films=read.csv('NetflixOriginals.csv')



films$Date=as.Date(films$Premiere,format="%b %d, %Y")
series$Date=as.Date(series$Premiere)

seriesdata=data.frame(series$Date,series$IMDb)
colnames(seriesdata)<-c('Date','IMDB')

filmsdata=data.frame(films$Date,films$IMDB.Score)
colnames(filmsdata)<-c('Date','IMDB')
data=rbind(seriesdata,filmsdata)
data$IMDB<-as.numeric(data$IMDB)
data=na.omit(data)

data$Week <- floor(difftime(data$Date, "2012/12/31")/7)
data$Week=as.numeric(data$Week)

##2. Clustering the IMDBscore by K-means
##Environment setting
source("DataAnalyticsFunctions.R")
install.packages("stats")
library(stats)
install.packages("class")
library(class)
install.packages("lattice")
library(lattice)
IMDBscore=data.frame(data$IMDB,1)
##cluster data using K-means, try 4 clusters firstly
data_4_kmeans<-kmeans(IMDBscore,4,nstart=10)
##Have a look at how 4 clusters perform, R^2 is 0.884,not bad
1 - sum(data_4_kmeans$tot.withinss)/data_4_kmeans$totss
##Visualize the 4 clusters, with their centres
colorcluster <- 1+data_4_kmeans$cluster
plot(IMDBscore, col=colorcluster,type = 'o', pch = 'o', ylab = '',yaxt='n')
points(data_4_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
##To pick a better k (possibly), we used regularization via information Criteria
kfit <- lapply(1:30, function(k) kmeans(IMDBscore,k,nstart=10))
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic)), # get them on same page
     type="l", lwd=2)
abline(v=which.min(kaic))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
##10 clusters are not appropriate as the IMDB_score scale is only from 0 to 10,
##7 clusters could be a more appropriate number, but it is still to explain
## By having a look at the graph, the IC for 4 clusters is lightly bigger than
## the minimum point.The good thing is that 4 clusters could be explained easily
## in the business world. They Netfilx original productions can then be categorized
## into 4 clusters:Disappointed(2) ,Normal(3),Good(4),Excellent(1)

##Assign the cluster number to the original data
data_clustered<-cbind(data,data_4_kmeans$cluster)

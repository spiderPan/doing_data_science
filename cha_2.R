library(readr)
nyt1 <- read_csv("dds_datasets/dds_datasets/dds_ch2_nyt/nyt1.csv")
head(nyt1,10)
nyt1$agecat <- cut(nyt1$Age,c(
  -Inf,0,18,24,34,44,54,64,Inf))
summary(nyt1)

library('doBy')
mode(nyt1)
siterange <- function(x){
  c(length(x),min(x),mean(x),max(x))
}

dataFrame <- as.data.frame(nyt1)
summaryBy(Age ~ agecat, data=dataFrame,FUN = siterange)
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat,data=dataFrame)

library(ggplot2)
ggplot(nyt1,aes(x=Impressions,fill=agecat))+geom_histogram(binwidth = 1)
ggplot(nyt1,aes(x=agecat,y=Impressions,fill=agecat))+geom_boxplot()

dataFrame$hasimps <- cut(dataFrame$Impressions,c(-Inf,0,Inf))
dataFrame$Gender <- factor(dataFrame$Gender)
dataFrame$Signed_In <- factor(dataFrame$Signed_In)
summaryBy(Clicks ~hasimps,data=dataFrame,Fun=siterange)

ggplot(subset(dataFrame,Impressions>0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(dataFrame,Clicks>0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(dataFrame,Clicks>0),aes(x=agecat,y=Clicks,fill=agecat))+geom_boxplot()
ggplot(subset(dataFrame,Clicks>0),aes(x=Clicks,colour=agecat))+geom_density()

dataFrame$scode[dataFrame$Impressions==0] <- "NoImps"
dataFrame$scode[dataFrame$Impressions>0]<- "Imps"
dataFrame$scode[dataFrame$Clicks>0]<-"Clicks"
dataFrame$scode<-factor(dataFrame$scode)
head(dataFrame)

clen<-function(x){c(length(x))}
etable<-summaryBy(Impressions~scode+Gender+agecat,data=dataFrame,FUN=clen)
testSet <- subset(dataFrame, Age<=18 & Clicks>0)
head(testSet)

ggplot(subset(dataFrame, Age<=18 & Clicks>0),aes(x=Clicks/Impressions,colour=Gender))+geom_density()
ggplot(subset(dataFrame, Clicks>0),aes(x=Clicks/Impressions,colour=Signed_In))+geom_density()

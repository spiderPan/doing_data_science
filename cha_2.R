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


library(foreign)
library(plyr)
library(stringr)

rm(list=ls())

#setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 2/Data")
polarization<- read.csv("CMP/CMP party data.csv", sep=";", dec = ",")

s.polar<-polarization[, c("countryname","edate", "date" ,"partyname", "pervote", "rile"  )]


s.polar <- s.polar[order(s.polar$countryname, s.polar$date),]

s.polar$uid<-paste0(s.polar$countryname, s.polar$edate)
s.polar$elect.d<-as.numeric(factor(s.polar$uid))


#converting from [-100,-100] to [0,10] range
OldRange = (100 - -100)  
NewRange = (10 - 0)  
for(i in 1:nrow(s.polar)){
  s.polar$left_right[i]<- (((s.polar$rile[i] - -100) * NewRange) / OldRange) + 0
}

### Polarization measure
w.polar<- function(votes, location){
  p.votes<-votes/100
  location<-location
  p.votes<-p.votes
  mean.position=sum(p.votes*location)
  cmp.wpsd<-sqrt(sum(p.votes*(location-mean.position)^2))
  return(cmp.wpsd)
} 

uw.polar<- function(votes, location){
  p.votes<-votes/100
  location<-location
  p.votes<-p.votes
  mean.position=sum(p.votes*location)
  cmp.uwpsd<-sqrt(sum((location-mean.position)^2)/length(location))
  return(cmp.uwpsd)
} 

ss.polar<-s.polar[complete.cases(s.polar$left_right), ]
ss.polar<-ss.polar[complete.cases(ss.polar$pervote), ]



polar.s<- ddply(ss.polar, c("elect.d","countryname", "edate", "date"), summarise, 
                cmp.wpsd.s = w.polar(pervote, left_right),
                cmp.uwpsd.s = uw.polar(pervote, left_right)
)

# Preparation for merging
polar.s$country<-as.character(polar.s$countryname)
polar.s$country[polar.s$country=="Great Britain"]<-"United Kingdom"
polar.s$countryname<-NULL

polar.s$year<-str_sub(polar.s$date,0, 4)

#unique ID
polar.s$uid<-paste0(polar.s$country, polar.s$year)
polar.s$elect2<-duplicated(polar.s$uid)
polar.s$uid[polar.s$elect2==T]<-paste0(polar.s$uid[polar.s$elect2==T], ".", 5)

polar.s$elect2<-NULL
polar.s$elect.d<-NULL
polar.s$country<-NULL

write.csv(file="CMP/polarization.csv", x=polar.s)
rm(list=ls())


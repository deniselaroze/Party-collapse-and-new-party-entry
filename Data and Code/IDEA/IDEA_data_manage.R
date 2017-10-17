### Data managemant for IDEA int turnout data downloaded 20-10-2015 
# http://www.idea.int/vt/viewdata.cfm#




rm(list=ls())

#setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 2/Data")


library(plyr)
library(stringr)


turnout <- read.csv("IDEA/VTData-20151020.csv", as.is=TRUE)
turnout$X<-NULL
turnout<-rename(turnout, c("Voter.Turnout...."="turnout", "VAP.Turnout...." ="VAP.turnout"))
turnout <- transform(turnout, elect.id=ave(rep(0,length(Country)), Country,
                                           FUN=seq_along))  

turnout$turnout.change<-NA
turnout$vap.t.change<-NA
for (i in 1:nrow(turnout)){
  
  if (turnout$elect.id[i]!=1) turnout$turnout.change[i]<-turnout$turnout[i]-turnout$turnout[i-1]
  else turnout$turnout.change[i]<-NA
  
  if (turnout$elect.id[i]!=1) turnout$vap.t.change[i]<-turnout$VAP.turnout[i]-turnout$VAP.turnout[i-1]
  else turnout$vap.t.change[i]<-NA
  
}

#turnout$uid<-paste0(turnout$Country, turnout$Year)
#turnout$elect2<-duplicated(turnout$uid)
#turnout$uid[turnout$elect2==T]<-paste0(turnout$uid[turnout$elect2==T], ".", 5)

turnout<-rename(turnout, c("Country"="country", "Year"="year"))
turnout$elect.id<-NULL



write.csv(file="IDEA/turnout.csv", x=turnout)
  
rm(turnout)  
  
  
  
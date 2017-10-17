
#setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 2/Data")

library(foreign)
library(plyr)


###############   Tavits 2006 Data 
##################################

tavits.2006 <- read.dta("Tavits/Tavits_PP_2006.dta")
var.labels <- attr(tavits.2006,"var.labels")
data.key.t6 <- data.frame(var.name=names(tavits.2006),var.labels)

# Adjusting variables for merging
tavits.2006<-rename(tavits.2006, c("country1"="country", "year1"="year", "newpart"="np5")) # this has to be adjusted for the real new party criteria
tavits.2006$country[tavits.2006$country=="UK"]<-"United Kingdom"                
tavits.2006$country[tavits.2006$country=="USA"]<-"United States"  
tavits.2006$country[tavits.2006$country=="NZ"]<-"New Zealand"  

unique(tavits.2006$country)
#check<-mydf.j[, c("country", "year", "name" , "iny", "outy")]
#View(subset(check, country=="unitedkingdom" ))

###############   Tavits 2008 Data 
##################################
tavits.2008<-read.csv("Tavits/New_parties_updated_Tavits.csv", header=T,sep=";", dec = ",")
NP.df<-rbind.fill(tavits.2006, tavits.2008)
rm(tavits.2006, tavits.2008)

NP.df$uid<-paste0(NP.df$country, NP.df$year)
NP.df$elect2<-duplicated(NP.df$uid)
NP.df$uid[NP.df$elect2==T]<-paste0(NP.df$uid[NP.df$elect2==T], ".", 5)
NP.df<-NP.df[, c("country", "year", "np5", "reg_cost" , "eth_frag", 
                 "vote_new", "party_fin", "uid", "vote_new")]
write.csv(file="Tavits/NP.df.csv", x=NP.df)


#count(mydf.t$elect2[mydf.t$elect2==T])






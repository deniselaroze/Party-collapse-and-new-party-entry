
### Data managemant for DPI world bank data downloaded 20-10-2015 
# http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/0,,contentMDK:20649465~pagePK:64214825~piPK:64214943~theSitePK:469382,00.html




rm(list=ls())

#setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 2/Data")



library(foreign)
library(plyr)

DPI<-read.dta("Worldbank/DPI2012.dta")
DPI.s<-DPI[, c("countryname", "year" , "yrsoffc", "prtyin", "legelec", "mdmh" , "tensys" ,  "liec" , "eiec" , "thresh" ) ]
DPI.s<- DPI.s[!DPI.s$legelec==0, ]

DPI.s<-rename(DPI.s, c("countryname"="country"))

DPI.s$country[DPI.s$country=="UK"]<-"United Kingdom"
DPI.s$country[DPI.s$country=="Czech Rep."]<-"Czech Republic"

DPI.s[DPI.s==-999]<-NA
DPI.s[DPI.s==888]<-NA




write.csv(file="Worldbank/dpi.csv", x=DPI.s)
rm(list=ls())


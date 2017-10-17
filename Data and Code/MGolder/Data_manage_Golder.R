
### script to select and clean Nils-Christian Bormann and Matt Golder's 1946-2011 electoral system data


library(foreign)
library(plyr)

Golder <- read.csv("MGolder/es_data-v2_0_1.csv")
g.short<-Golder[, c("country", "year", "ccode", "ccode2" ,"legislative_type", "elecrule", "tier1_avemag", "enep1", "enpp1" )]
g.short$country[g.short$country=="West Germany"]<-"Germany"


#unique election ID
g.short$uid<-paste0(g.short$country, g.short$year)
g.short$elect2<-duplicated(g.short$uid)
g.short$uid[g.short$elect2==T]<-paste0(g.short$uid[g.short$elect2==T], ".", 5)
g.short$elect2<-NULL


write.csv(file="MGolder/g.short.csv", x=g.short)

rm(Golder)
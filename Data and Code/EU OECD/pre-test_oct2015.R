setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 2/EU OECD")


library(plyr)
library(stringr)


### Data from PARLGOV  www.parlgov.rg
#####################################

# view_cabinet.csv
#  * ParlGov table of government formation that includes all parties
#    in parliament each time a new government forms
#
# external_party_castles_mair.csv
#  * Castles/Mair 1983 party expert survey data
#  * 'left_right' includes left/right position made used of here
#  * 'id' includes Castles/Mair party ids
#
# view_party.csv
#  * ParlGov parties table that documents all parties in ParlGov and
#    links ids from external data sets with party positions
#  * 'castles_mair' includes Castles/Mair party ids

# reading ParlGov data tables described above mac/linux may require:
# <- read.csv(file(".csv", encoding = "latin1"), as.is=TRUE)
parties <- read.csv("view_party.csv", as.is=TRUE)
#cabinets <- read.csv("view_cabinet.csv", as.is=TRUE)
castles <- read.csv("external_party_castles_mair.csv", as.is=TRUE)
elections<-read.csv("view_election.csv", as.is=TRUE)



# Trim and rename variables in datasets for merging
castles<-rename(castles, c("left_right"="left_right_CM", "range_left"="range_left_cm","range_right"="range_right_cm" ))

# merge Castles/Mair positions to cabinet data
castles <- merge(castles, parties[,c('castles_mair', 'party_id', 'family_name_short', 'family_name', 'family_id')],
                 by.x='id', by.y='castles_mair', all.x=TRUE)
mydf <- merge(elections, castles[,c('party_id', 'left_right_CM',  'range_left_cm', 'range_right_cm')],
                  by='party_id', all.x=TRUE)

mydf <- mydf[order(mydf$country_name, mydf$election_date),]

mydf<-subset(mydf, election_type="parliament")
mydf$election_type<-NULL

### Remove unnecessary data frames
rm(castles, elections, parties )


# e.g. limit observations to period 1975--1985 and parties with
#cabinets <- cabinets[cabinets$start_date >= '1975-01-01', ]

##### creating variables of interest in long data
# create observations for seats share and distance from center
mydf$seats_share <- (mydf$seats / mydf$seats_total) * 100
mydf$left_right_extreme <- abs(mydf$left_right - 5)
mydf$e_year<-str_sub(mydf$election_date,0, -7)

df$left_right_extreme<-as.numeric(df$left_right_extreme)

df<-mydf
df<- df[!df$party_name=="no party affiliation", ]

df$party_name<-NULL
df$previous_cabinet_id<-NULL

#order for easness if checking results
df <- df[order(df$country_name, df$e_year, df$election_id),]


df$seat.change<-NA
df$s.seat.change<-NA
df$s.vote.change<-NA
for(i in 1:nrow(df)){ 
  met1<-df$election_id == df$previous_parliament_election_id[i] & df$party_name_short==df$party_name_short[i]  
  df$seat.change[i]<- if(T %in% met1) df$seats[i]- df$seats[met1] else NA
  df$s.seat.change[i]<- if(T %in% met1) df$seats[i]/df$seats[met1] else NA
  df$s.vote.change[i]<- if(T %in% met1) df$vote_share[i]/df$vote_share[met1] else NA
  }

df$collapse.seat<-0
df$collapse.seat[df$s.seat.change<.5]<-1

df$collapse.vote<-0
df$collapse.vote[df$s.vote.change<.5]<-1

#descriptives
count(df$collapse.seat)
count(df$collapse.vote)

#location of collapsed party dummy for being +-1.5 from the centre
df$cs.location<-0
df$cs.location[df$left_right_extreme>1.5 & df$collapse.seat==1]<-1

df$cv.location<-0
df$cv.location[df$left_right_extreme>1.5 & df$collapse.vote==1]<-1





View(subset(df, collapse.seat==1))

#df$share.change[!is.finite(df$share.change)]<-  NA 
 
#### Collapsing df

library(doBy)
collapse<-summaryBy(collapse ~ e_year + country_name, data=df)
                            
agg.dat<-aggregate(df, by=list(df$e_year, df$country_name ), mean, na.rm=T )
agg.dat<-rename(agg.dat, c("Group.1"="year", "Group.2"="country" ))

#dummies in aggregated data
agg.dat$collapse.seat[agg.dat$collapse.seat>0]<-1
agg.dat$collapse.vote[agg.dat$collapse.vote>0]<-1

agg.dat$cs.location[agg.dat$cs.location>0]<-1
agg.dat$cv.location[agg.dat$cv.location>0]<-1
#location of collapsed party dummy for being +-1.5 from the centre


collapse.df<-agg.dat[, c("year", "country", "collapse.seat", "collapse.vote", "cs.location", "cv.location") ]

# Turning collapse into a dummy

### Descriptive Statatistics
#############################

library(MASS)
library(plm)
library(DataCombine)

## lagged variables
merged <- merged[order(merged$country, merged$year),]

merged <- slide(merged, Var = "collapse.seat" ,
                  GroupVar = "country",
                  TimeVar= "year",
                   slideBy = -1)
merged <- slide(merged, Var = "collapse.vote" ,
                GroupVar = "country",
                NewVar = "cv.lag",
                TimeVar= "year",
                slideBy = -1)
merged <- slide(merged, Var = "collapse.seat" ,
                GroupVar = "country",
                TimeVar= "year",
                NewVar = "cs.lag",
                slideBy = -1)
merged <- slide(merged, Var = "cs.location" ,
                GroupVar = "country",
                NewVar = "csl.lag",
                TimeVar= "year",
                slideBy = -1)

merged <- slide(merged, Var = "cv.location" ,
                GroupVar = "country",
                NewVar = "cvl.lag",
                TimeVar= "year",
                slideBy = -1)


poisson1 <- glm.nb(np5~cv.lag*cv.location + gdp_grow +  ln_pop, data= merged)
summary(poisson1)

lm1<-lm(np5~ cv.lag + gdp_grow + diff_unemp , data=merged)
summary(lm1)

fe1 <- plm(np5~ cv.lag + gdp_grow  , data=merged , index=c("country", "year"), model="pooling")
summary(fe1)

unique(merged$country)

merged$country<-as.factor(merged$country)
merged$year<-as.numeric(merged$year)

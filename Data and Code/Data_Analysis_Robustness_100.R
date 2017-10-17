### New Party Entry
### Author: Denise Laroze
#############################

library(MASS)
library(plm)
library(DataCombine)
#library(effects)
library(AER)
#library(dplyr)
library(pglm)
library(rms)
library(plyr)
library(boot)
#library(Hmisc)
library(texreg)
library(arm)
library(stargazer)

rm(list=ls())

setwd("C:/Users/André Laroze/Dropbox/Essex PhD/Paper 2/Data")

#bd<-"C:/Users/André Laroze/Dropbox/Essex PhD/Paper 2/Draft 1/Robustness"
#bd<-"C:/Users/André Laroze/Dropbox/Essex PhD/Paper 2/Presentations/"




###################################
############### Merging Data frames
###################################
##### Sourcing updated data management files 
# source("Tavits/Data_manage_Tavits.R")
source("EU OECD/Party_Data_Management_Robustness_1.R")
# source("WorldBank/Worldbank.R")
# source("WorldBank/DPI_manage.R")
# source("MGolder/Data_manage_Golder.R")
# source("IDEA/IDEA_data_manage.R")
# source("CMP/CMP_manage.R")

############################
##### Merging Data
############################
rm(list=ls())


#"reading" the data
NP.df<-read.csv(file="Tavits/NP.df.csv")
NP.df$X<-NULL
wdi<-read.csv(file="WorldBank/wdi_data.csv")
wdi$X<-NULL
g.short<-read.csv(file="MGolder/g.short.csv")
g.short$X<-NULL
party.data<-read.csv(file="EU OECD/party_data_robustness_1.csv")
party.data$X<-NULL
party.data$country.1<-NULL

turnout<-read.csv(file="IDEA/turnout.csv")
turnout$X<-NULL

dpi<-read.csv(file="WorldBank/dpi.csv")
dpi$X<-NULL

polar<-read.csv(file="CMP/polarization.csv")
polar$X<-NULL

mydf<-merge(x = party.data, y = NP.df, by = c("uid", "year"), all.x = T ) # Party data with NP data with
mydf<-merge(x = mydf, y = g.short, by = c("uid", "year")) #Merged data with Electoral Data
mydf<-merge(x = mydf, y = wdi, by = c("country", "year"),all.x = T ) #Merged data with Economic data
mydf<-merge(x = mydf, y = turnout, by = c("country", "year"),all.x = T ) # turnout data IDEA
mydf<-merge(x = mydf, y = dpi, by = c("country", "year"),all.x = T ) # dpi data world bank
mydf<-merge(x = mydf, y = polar, by = c("uid", "year"),all.x = T ) # CMP polarization data
mydf$country.x<-NULL
mydf$country.y<-NULL
mydf$date<-NULL
mydf$edate<-NULL

# Cleaning df
mydf <- mydf[order(mydf$country, mydf$year),]
rm(NP.df, g.short, wdi, party.data, dpi, turnout, polar)

write.csv(file="NPE_robustness_1.csv", x=mydf)


#######################
##### Data Management
######################################
bd<-"C:/Users/André Laroze/Dropbox/Essex PhD/Paper 2/Draft 1/Robustness/"


#rm(list=ls())
mydf<-read.csv(file="NPE_robustness_1.csv")

#cleaning irrelevant or constant variables
mydf$X<-NULL
mydf$Election.type<-NULL
mydf$legelec<-NULL

# Dummy for FPTP electoral system 
mydf$fptp<-0
mydf$fptp[mydf$legislative_type==1]<-1


#Election Id
mydf <- mydf[order(mydf$country, mydf$year),]
mydf <- transform(mydf, elect.id=ave(rep(0,length(country)), country,
                               FUN=seq_along))
mydf$country_id<-as.numeric(mydf$country)


#check<-mydf[, c("uid", "country", "year", "leader" ,"enddate", "exit", "died", "ivdead")]
#View(check)

## Time and years of observation for each country

# year.table<-ddply(mydf, "country", summarize,
#       start.year = min(year, na.rm=T),
#       end.year = max(year, na.rm=T),
#       N.elections= max(elect.id, na.rm=T))
# year.table
#tex.year.table<- xtable(year.table)
#print(tex.year.table, type="latex", file="country.table.tex", floating=FALSE)


### lagged versions of the variables for estimations that cannot export panel models 

lagpad <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(x)) 
    stop('x must be numeric')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  c(rep(NA, k), x)[1 : length(x)] 
}

mydf$share.v.crash.tp1.k2<-lagpad(mydf$share.v.crash.tp1, 2)
mydf$share.v.crash.tp1.k2[mydf$elect.id==1 | mydf$elect.id==2 ]<-NA


mydf$c.location.mean.tp1.k2<-lagpad(mydf$c.location.mean.tp1, 2)
mydf$c.location.mean.tp1.k2[mydf$elect.id==1 | mydf$elect.id==2 ]<-NA


mydf$cmp.wpsd.s.k3<-lagpad(mydf$cmp.wpsd.s, 3)
mydf$cmp.wpsd.s.k3[mydf$elect.id==1 | mydf$elect.id==2 | mydf$elect.id==3 ]<-NA

mydf$n.parties.k1<-lagpad(mydf$n.parties, 1)
mydf$n.parties.k1[mydf$elect.id==1]<-NA

mydf$S.NP.d.k1<-lagpad(mydf$S.NP.d, 1)
mydf$S.NP.d.k1[mydf$elect.id==1]<-NA

mydf$np.vote.k1<-lagpad(mydf$np.vote, 1)
mydf$np.vote.k1[mydf$elect.id==1]<-NA

mydf$log.share<-log(mydf$share.v.crash.tp1.k2+1) # recoding for running lm models and plotting interaction terms

# Dummy variable for East Europe (former soviet) countries
mydf<-ddply(mydf, "country", transform,
                  e.eu = ifelse(min(year, na.rm=T)>1989,1, 0))
                  

############################
#### Descriptive Statistics
############################
##### Summary table 

sum.df<-mydf[, c("np.vote", "S.NP.d" ,"S.NP.n", "share.v.crash.tp1", "c.location.mean.tp1", "cmp.wpsd.s", "population.wdi",
                 "gdp.pct.chg", "cpi.wdi", "n.parties", "elect.id",  "tier1_avemag", "turnout", "Compulsory.voting", "e.eu", "thresh"
                 ,"enep1")]
#sum.df$share.v.crash.tp1.D0<-sum.df$share.v.crash.tp1
#sum.df$share.v.crash.tp1.D0[sum.df$share.v.crash.tp1==0]<-NA
#sum.df$c.location.mean.tp1.D0<-sum.df$c.location.mean.tp1
#sum.df$c.location.mean.tp1.D0[sum.df$c.location.mean.tp1==0]<-NA  

sum.df$population.wdi<-log(sum.df$population.wdi)
sum.df$cpi.wdi<-log(sum.df$cpi.wdi+2)

sum.df$share.v.crash.tp1<-log(sum.df$share.v.crash.tp1 + 1)

sum.df$Compulsory.voting<-as.numeric(sum.df$Compulsory.voting)
sum.df$Compulsory.voting<-sum.df$Compulsory.voting-1
#summary(sum.df$share.v.crash.tp1)
#summary(sum.df$share.v.crash.tp1.D0)

stargazer(sum.df, covariate.labels=c("Share Votes NP", "Successful NP (dummy)", "N.Successful NP"  ,"Log % votes collapsed party (T-2)", 
                                     "Location collapsed party (T-2)", "Weighted Party System Dispersion",
                                     "Log of population", "% change in GDP", "Log of Inflation (+2)", "Number of parties",
                                     "N. Elections", "Mean Dist. Mag.", "Turnout", "Compulsory voting (dummy)", "East Europe (dummy)", "Threshold", "ENEP"),
                                      label="tab:summary",  out=paste0(bd, "summary_robustness_1.tex", sep=""))


# Description of distribution of np results  
describe(mydf$np.vote)


#### Correlation between share of votes controlled by the collapsed party and its location
# rcorr(mydf$share.v.crash.tp1, mydf$c.location.mean.tp1) # value of variable
# 
# rcorr(mydf$share.v.crash.tp1[mydf$p.crash.tp1>0], mydf$c.location.mean.tp1[mydf$p.crash.tp1>0]) #relevant values above zero
# 
# 
# png(filename= paste0(bd, "crash_location_all.png", sep=""))
# scatterplot(mydf$share.v.crash.tp1, mydf$c.location.mean.tp1,
#             ylab="Location: Abs. distance from the centre", xlab="% votes collapsed party")
# #, col=c("black", "gray65", "black", "black"))
# dev.off()
# 
# png(filename= paste0(bd, "crash_location_a0.png", sep=""))
# scatterplot(mydf$share.v.crash.tp1[mydf$p.crash.tp1>0], mydf$c.location.mean.tp1[mydf$p.crash.tp1>0],
#             ylab="Location: Abs. distance from the centre", xlab="% votes collapsed party", col=c("black", "gray65", "black", "black"))
# dev.off()

# ### Apendix figure of locaion of collapsed parties and vote share. 
# pdf<-mydf[mydf$p.crash.tp1>0, ]
# describe(pdf$c.location.mean.tp1, na.rm=T)
# require(ggplot2)
# p <- ggplot(pdf, aes(share.v.crash.tp1, c.location.mean.tp1)) + scale_fill_gradient(low="grey90", high="black")
# p <- p + stat_bin2d(bins = 30) + xlab("Share votes") + xlim(c(0,50)) + ylab("Absolute distance from the centre") + theme_bw() #+ ylim(c(0,1)) +  #ylab("") #+ xlim(c(0,15)) + ylim(c(0,15)) + theme_bw()
# #p <-p + geom_text(aes(4, 0.2, label="NE"), colour="blue")
# p
# ggsave(filename=paste0(bd, "crash_location.png", sep=""),  width=6, height=6)

summary(mydf$np.vote, na.rm=T)



### Figure XXX density plots of the dependent variable
png(filename= paste0(bd, "vote_share_np.png", sep=""))
par(mfrow = c(1, 2))
plot(density(mydf$np.vote, na.rm=T), main="Vote share NP", xlab="", cex.main=0.9)
plot(density(log(mydf$np.vote), na.rm=T), main="Log vote share NP", ylab="", xlab="", cex.main=0.9)
dev.off()

### Figure XXX histrograms of number of new parties (appendix)
png(filename= paste0(bd, "descrete_np.png", sep=""))
par(mfrow = c(1, 2))
discrete.histogram(mydf$S.NP.n, main="", xlab="Number of NP", bar.width=0.7, prob.col="black")
discrete.histogram(mydf$S.NP.d, main="", xlab="NP entry", bar.width=0.9, prob.col="black")
dev.off()


###########################
### Empirical Analyses
###########################

##########################
### Success of new parties 
############################
p1.n <- plm(np.vote~ lag(share.v.crash.tp1, 2)+lag(c.location.mean.tp1,  2)
           + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
           + lag(n.parties, 1)  #+ pub_fin_new
           + log(as.numeric(elect.id)+1) 
           + tier1_avemag #+ factor(fptp) 
           + turnout
           + Compulsory.voting
           #+ thresh
           #+ lag(enep1,1) 
           # + lag(np.vote, 1)
           ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
summary(p1.n)

p1.n.pse<-coeftest(p1.n, vcov=vcovHC(p1.n, method="arellano"))
p1.n.pse



p1 <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)
          + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
          + lag(n.parties, 1)  #+ pub_fin_new
          + log(as.numeric(elect.id)+1) 
          + tier1_avemag #+ factor(fptp) 
          + turnout
          + Compulsory.voting
          #+ thresh
          #+ lag(enep1,1)  
          #+ lag(np.vote, 1)
          ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
summary(p1)

p1.pse<-coeftest(p1, vcov=vcovHC(p1, method="arellano"))
p1.pse
#plot(density(resid(p1.l))) #A density plot
#qqnorm(resid(p1.l)) # A quantile normal plot - good for checking normality

p2 <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.uwpsd.s, 3)
           + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
           + lag(n.parties, 1)  #+ pub_fin_new
           + log(as.numeric(elect.id)+1) 
           + tier1_avemag #+ factor(fptp) 
           + turnout
           + Compulsory.voting
           #+ thresh
           #+ lag(enep1,1)  
           + log(lag(np.vote, 1)+1)
           ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
summary(p2)

p2.pse<-coeftest(p2, vcov=vcovHC(p2, method="arellano"))
p2.pse


re <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
           + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
           + lag(n.parties, 1)  #+ pub_fin_new
           + log(as.numeric(elect.id)+1) 
           + tier1_avemag #+ factor(fptp) 
           + turnout
           + Compulsory.voting
           #+ thresh
           #+ lag(enep1,1)  
           #+ log(lag(np.vote, 1)+1)
           ,data=mydf , index=c("country", "elect.id"), model="random", effect ="individual")
summary(re)

re.pse<-coeftest(re, vcov=vcovHC(re, method="arellano"))
re.pse




fe1<- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
           + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
           + lag(n.parties, 1)  #+ pub_fin_new
           + log(as.numeric(elect.id)+1) 
           + tier1_avemag #+ factor(fptp) 
           + turnout
           + Compulsory.voting
           #+ thresh
           #+ lag(enep1,1)  
           + log(lag(np.vote, 1)+1)
           ,data=mydf , index=c("country", "elect.id"), model="within", effect ="individual")
summary(fe1)

fe1.pse<-coeftest(fe1, vcov=vcovHC(fe1, method="arellano"))
fe1.pse


fe2 <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
           + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
           + lag(n.parties, 1)  #+ pub_fin_new
           #+ log(as.numeric(elect.id)+1) 
           + tier1_avemag #+ factor(fptp) 
           + turnout
           + Compulsory.voting
           #+ thresh
           #+ lag(enep1,1)  
           + log(lag(np.vote, 1)+1)
           ,data=mydf , index=c("country", "elect.id"), model="within", effect ="twoway")
summary(fe2)
fe2.pse<-coeftest(fe2, vcov=vcovHC(fe2, method="white2"))
fe2.pse


vxf <- plm(log(np.vote+1)~ factor(fptp)*log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
           + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
           + lag(n.parties, 1)  #+ pub_fin_new
           #+ log(as.numeric(elect.id)+1) 
           #+ tier1_avemag #+ factor(fptp) 
           + turnout
           + Compulsory.voting
           #+ thresh
           #+ lag(enep1,1)  
           + log(lag(np.vote, 1)+1)
           ,data=mydf , index=c("country", "elect.id"), model="within", effect ="twoway")
summary(vxf)

vxf.pse<-coeftest(vxf, vcov=vcovHC(vxf, method="white2"))
vxf.pse

#### Test for footnote XXXXXXXXXX
vxf2 <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+factor(fptp)*lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
            + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
            + lag(n.parties, 1)  #+ pub_fin_new
            #+ log(as.numeric(elect.id)+1) 
            #+ tier1_avemag #+ factor(fptp) 
            + turnout
            + Compulsory.voting
            #+ thresh
            #+ lag(enep1,1)  
            + log(lag(np.vote, 1)+1)
            ,data=mydf , index=c("country", "elect.id"), model="within", effect ="twoway")
summary(vxf2)

vxf2.pse<-coeftest(vxf2, vcov=vcovHC(vxf2, method="white2"))
vxf2.pse


screenreg(list(p2.pse,fe2.pse, vxf.pse) , stars = c(0.001, 0.01, 0.05, 0.1),  reorder.coef=c( 2, 3, 13, 14, 4,5, 6, 7, 8, 9, 10, 11, 12, 1))


### Printing all models in table 1

texreg(file= paste0(bd, "main_models_robustness_1.tex", sep=""), 
       list(p2.pse,fe2.pse, vxf.pse) , 
       custom.model.names = c("M1 Pooled", "M2 FE", "M3 FE Interaction " ), 
       custom.coef.names = c("Intercept", "Log % votes collapsed party (t-2)", "Location collapsed party (t-2)", #"Weighted Party System Dispersion",
                             "Log of population", "% change in GDP", "Log inflation (+2) ", "Number of parties (t-1)",
                             "Log time (+ 1)", "Mean Dist. Mag.", "Turnout", "Compulsory voting (Yes)",   
                             "Log vote share NP (t-1)" , "FPTP (dummy - Yes)", "FPTP * log % collapsed party (t-2)"),
       reorder.coef=c( 2, 3, 13, 14, 4,5, 6, 7, 8, 9, 10, 11, 12, 1),
       caption = "Empirical models on the association between size of party crash, its location and interaction of size 
       with electoral system. The dependent  variable is the logged vote share for new parties",
       label="table:main_models", stars = c(0.001, 0.01, 0.05, 0.1), #ci.force = T, ci.force.level = 0.95,
       booktabs = F, dcolumn = F,  sideways = T)


######################
#### Robustness tests
######################

#########################################################################
### Success of new parties other control variables, Appendix Table NNNNNN
#########################################################################
p1.n.mc <- plm(np.vote~ lag(share.v.crash.tp1, 2)+lag(c.location.mean.tp1,  2)
               + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
               #+ lag(n.parties, 1)  #+ pub_fin_new
               + log(as.numeric(elect.id)+1) 
               #+ tier1_avemag #
               + factor(fptp) 
               + turnout
               + Compulsory.voting
               + thresh
               + lag(enep1,1) 
               # + lag(np.vote, 1)
               ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")

p1.n.mc.pse<-coeftest(p1.n.mc, vcov=vcovHC(p1.n.mc, method="arellano"))

p1.mc <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
             #+ lag(n.parties, 1)  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             #+ tier1_avemag #
             + factor(fptp) 
             + turnout
             + Compulsory.voting
             + thresh
             + lag(enep1,1) 
             # + lag(np.vote, 1)
             ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
p1.mc.pse<-coeftest(p1.mc, vcov=vcovHC(p1.mc, method="arellano"))

p2.mc <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.uwpsd.s, 3)
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
             #+ lag(n.parties, 1)  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             #+ tier1_avemag #
             + factor(fptp) 
             + turnout
             + Compulsory.voting
             + thresh
             + lag(enep1,1) 
             + log(lag(np.vote, 1)+1)
             ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
p2.mc.pse<-coeftest(p2.mc, vcov=vcovHC(p2.mc, method="arellano"))

re.mc <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
             #+ lag(n.parties, 1)  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             #+ tier1_avemag #
             + factor(fptp) 
             + turnout
             + Compulsory.voting
             + thresh
             + lag(enep1,1) 
             # + lag(np.vote, 1)
             ,data=mydf , index=c("country", "elect.id"), model="random", effect ="individual")
re.mc.pse<-coeftest(re.mc, vcov=vcovHC(re.mc, method="arellano"))

fe1.mc<- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
             #+ lag(n.parties, 1)  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             #+ tier1_avemag #
             + factor(fptp) 
             + turnout
             + Compulsory.voting
             + thresh
             + lag(enep1,1) 
             + log(lag(np.vote, 1)+1)
             ,data=mydf , index=c("country", "elect.id"), model="within", effect ="individual")
fe1.mc.pse<-coeftest(fe1.mc, vcov=vcovHC(fe1.mc, method="arellano"))

fe2.mc <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
              + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
              #+ lag(n.parties, 1)  #+ pub_fin_new
              #+ log(as.numeric(elect.id)+1) 
              #+ tier1_avemag 
              + factor(fptp) 
              + turnout
              + Compulsory.voting
              + thresh
              + lag(enep1,1)  
              + log(lag(np.vote, 1)+1)
              ,data=mydf , index=c("country", "elect.id"), model="within", effect ="twoway")
fe2.mc.pse<-coeftest(fe2.mc, vcov=vcovHC(fe2.mc, method="white2"))


vxf.mc <- plm(log(np.vote+1)~ factor(fptp)*log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
              + log(population.wdi) + gdp.pct.chg+     +  log(cpi.wdi+2)  #+ unempl.wdi
              #+ lag(n.parties, 1)  #+ pub_fin_new
              #+ log(as.numeric(elect.id)+1) 
              #+ tier1_avemag #
              #+ factor(fptp) 
              + turnout
              + Compulsory.voting
              + thresh
              + lag(enep1,1)  
              + log(lag(np.vote, 1)+1)
              ,data=mydf , index=c("country", "elect.id"), model="within", effect ="twoway")
vxf.mc.pse<-coeftest(vxf.mc, vcov=vcovHC(vxf.mc, method="white2"))

screenreg(list(p2.mc.pse, fe2.mc.pse , vxf.mc.pse), 
          reorder.coef=c( 2,  3, 8, 14, 4,5, 6, 7, 9, 10, 11, 12, 13, 1), stars = c(0.001, 0.01, 0.05, 0.1))


### Printing all models for appendix table NNNNNNNNNNNNNNNNNNN
texreg(file= paste0(bd, "control_models_robustness_1.tex", sep=""), 
       list(p2.mc.pse, fe2.mc.pse , vxf.mc.pse ), 
       custom.model.names = c("A.M8 Pooled", "A.M9 FE ", "A.M10 FE Interaction" ), 
       custom.coef.names = c("Intercept", "Log % votes collapsed party (t-2)", "Location collapsed party (t-2)", 
                             "Log of population", "% change in GDP", "Log inflation (+2) ", 
                             "Log time (+ 1)", "FPTP (dummy - Yes)", "Turnout", "Compulsory voting (Yes)", "Threshold", "ENEP",   
                             "Log vote share NP (t-1)" , "FPTP * log % collapsed party (t-2)"),
       reorder.coef=c( 2,  3, 8, 14, 4,5, 6, 7, 9, 10, 11, 12, 13, 1),
       caption = "Alternative control variables for models M1-3 in the main text",
       label="table:control_models", stars = c(0.001, 0.01, 0.05, 0.1), #ci.force = T, ci.force.level = 0.95,
       booktabs = F, dcolumn = F,  sideways = F)


########################
### East Europe controls
############################
p1.n.e <- plm(np.vote~ lag(share.v.crash.tp1, 2)+lag(c.location.mean.tp1,  2)
            + e.eu
            + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
            + lag(n.parties, 1)  #+ pub_fin_new
            + log(as.numeric(elect.id)+1) 
            + tier1_avemag #+ factor(fptp) 
            + turnout
            + Compulsory.voting
            #+ thresh
            #+ lag(enep1,1) 
            + log(lag(np.vote, 1)+1)
            ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
p1.n.e.pse<-coeftest(p1.n.e, vcov=vcovHC(p1.n.e, method="arellano"))

p1.e <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)
          + e.eu
          + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
          + lag(n.parties, 1)  #+ pub_fin_new
          + log(as.numeric(elect.id)+1) 
          + tier1_avemag #+ factor(fptp) 
          + turnout
          + Compulsory.voting
          #+ thresh
          #+ lag(enep1,1)  
          + log(lag(np.vote, 1)+1)
          ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
p1.e.pse<-coeftest(p1.e, vcov=vcovHC(p1.e, method="arellano"))

#plot(density(resid(p1.l))) #A density plot
#qqnorm(resid(p1.l)) # A quantile normal plot - good for checking normality


re.e <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
          + e.eu
          + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
          + lag(n.parties, 1)  #+ pub_fin_new
          + log(as.numeric(elect.id)+1) 
          + tier1_avemag #+ factor(fptp) 
          + turnout
          + Compulsory.voting
          #+ thresh
          #+ lag(enep1,1)  
          #+ log(lag(np.vote, 1)+1)
          ,data=mydf , index=c("country", "elect.id"), model="random", effect ="individual")
re.e.pse<-coeftest(re.e, vcov=vcovHC(re.e, method="arellano"))




lm1xe <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)*e.eu+lag(c.location.mean.tp1,  2)#*lag(cmp.uwpsd.s, 3)
            + e.eu
            + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
            + lag(n.parties, 1)  #+ pub_fin_new
            + log(as.numeric(elect.id)+1) 
            + tier1_avemag #+ factor(fptp) 
            + turnout
            + Compulsory.voting
            #+ thresh
            #+ lag(enep1,1)  
            + log(lag(np.vote, 1)+1)
            ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
lm1xe.pse<-coeftest(lm1xe, vcov=vcovHC(lm1xe, method="arellano"))
linearHypothesis(lm1xe, "log(lag(share.v.crash.tp1, 2) + 1) + log(lag(share.v.crash.tp1, 2) + 1):e.eu")

#lm1xe.pse

lm2xe <- plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)*e.eu#*lag(cmp.uwpsd.s, 3)
             + e.eu
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
             + lag(n.parties, 1)  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             + tier1_avemag #+ factor(fptp) 
             + turnout
             + Compulsory.voting
             #+ thresh
             #+ lag(enep1,1)  
             + log(lag(np.vote, 1)+1)
             ,data=mydf , index=c("country", "elect.id"), model="pooling", effect ="individual")
lm2xe.pse<-coeftest(lm2xe, vcov=vcovHC(lm2xe, method="arellano"))
#lm2xe.pse
linearHypothesis(lm2xe, "lag(c.location.mean.tp1, 2) + lag(c.location.mean.tp1, 2):e.eu")



screenreg(list(p1.e.pse, lm1xe.pse, lm2xe.pse), stars = c(0.001, 0.01, 0.05, 0.1))

texreg(file= paste0(bd, "eeu_models_robustness_1.tex", sep=""), 
  list(p1.e.pse, lm1xe.pse, lm2xe.pse) , 
  custom.model.names = c("A.M11 Pooled", "A.M12 Int. Size", "A.M13 Int. Location"), 
  custom.coef.names = c("Intercept", "Log % votes collapsed party (t-2)", "Location collapsed party (t-2)", "East Europe (dummy - Yes)",
                        "Log of population", "% change in GDP", "Log inflation (+2) ", "Number of parties (t-1)",
                        "Log time (+ 1)", "Mean Dist. Mag.", "Turnout", "Compulsory voting (Yes)",   
                         "Log vote share NP (t-1)" , "Log % votes * East Europe", "Location * East Europe"),
  reorder.coef=c( 2, 3, 14, 15, 4,5, 6, 7, 8, 9, 10, 11,12 ,  13, 1),
  caption = "Pooled linear models controling for the impact of elections in post-soviet (East European) countries, their interaction
  with the size of the collapsed party and location. The dependent variable is the log share of votes for new parties (+1).",
  label="table:eeu_models", stars = c(0.001, 0.01, 0.05, 0.1), #ci.force = T, ci.force.level = 0.95,
  booktabs = F, dcolumn = F,  sideways = F)

texreg(#file= paste0(bd, "eeu_models.tex", sep=""), 
  list(p1.e, lm1xe, lm2xe) , 
  custom.model.names = c("A.M1 Pooled", "A.M2 Int. Size", "A.M3 Int. Location"), 
   booktabs = F, dcolumn = F,  sideways = F)


############################################################
#### Count definitions of the dependent variable table NNNNN
#################################################


################# Logit on Successful new parties >= 1 seat

logit <- glm(S.NP.d~ log.share+c.location.mean.tp1.k2#*cmp.wpsd.s.k3
                #+ e.eu
                + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
                + n.parties.k1  #+ pub_fin_new
                + log(as.numeric(elect.id)+1) 
                #+ tier1_avemag 
                + turnout
                + Compulsory.voting
                #+ thresh
                #+ lag(enep1,1)  
                + S.NP.d.k1
                + factor(fptp) 
                ,data=mydf, family=binomial(link = "logit"))
summary(logit)

logit2 <- glm(S.NP.d~ factor(fptp)*log.share+c.location.mean.tp1.k2#*cmp.wpsd.s.k3
             #+ e.eu
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
             + n.parties.k1  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             #+ tier1_avemag #+ factor(fptp) 
             + turnout
             + Compulsory.voting
             #+ thresh
             #+ lag(enep1,1)  
             + S.NP.d.k1
             ,data=mydf, family=binomial(link = "logit"))
summary(logit2)


linearHypothesis(logit2, "log.share + factor(fptp)1:log.share")

#la <- pglm(pat ~ lag(logr, 0:5) + scisect + logk + factor(year), PatsRD,
#           family = negbin, model = "within", print.level=3, method="nr",
#           index=c('cusip', 'year'))


negbin <- glm.nb(S.NP.n ~ log.share+c.location.mean.tp1.k2#*cmp.wpsd.s.k3
                 #+ e.eu
                 + log(population.wdi) + gdp.pct.chg     + log(cpi.wdi+2) # + unempl.wdi
                 + n.parties.k1  #+ pub_fin_new
                 + log(as.numeric(elect.id)+1) 
                 #+ tier1_avemag #+ factor(fptp) 
                 + turnout
                 + Compulsory.voting
                 #+ thresh
                 #+ lag(enep1,1)  
                 + S.NP.d.k1
                 + factor(fptp) 
                 ,data=mydf)

summary(negbin)

negbin2 <- glm.nb(S.NP.n ~ factor(fptp)*log.share+c.location.mean.tp1.k2#*cmp.wpsd.s.k3
                 #+ e.eu
                 + log(population.wdi) + gdp.pct.chg     + log(cpi.wdi+2) # + unempl.wdi
                 + n.parties.k1  #+ pub_fin_new
                 + log(as.numeric(elect.id)+1) 
                 #+ tier1_avemag #+ factor(fptp) 
                 + turnout
                 + Compulsory.voting
                 #+ thresh
                 #+ lag(enep1,1)  
                 + S.NP.d.k1
                 ,data=mydf)

summary(negbin2)

linearHypothesis(negbin2,c("log.share + factor(fptp)1:log.share"))


texreg(file= paste0(bd, "count_models_robustness_1.tex", sep=""), 
       list(logit, logit2, negbin, negbin2), 
       custom.model.names = c("A.M14 Logit", "A.M15 Logit int " ,"A.M16 Neg. Bino.", "A.M17 Neg. Bin. int"), 
       custom.coef.names = c("Intercept", "Log % votes collapsed party (t-2)", "Location collapsed party (t-2)", #"East Europe (dummy - Yes)",
                             "Log of population", "% change in GDP", "Log inflation (+2)", "Number of parties (t-1)",
                             "Log time (+ 1)", "Turnout", "Compulsory voting (Yes)",   
                              "Existence NP (t-1)", "FPTP (dummy - Yes)","FPTP * log % collapsed party (t-2)"),
       reorder.coef=c(2, 3, 12, 13, 4, 5, 6, 7, 8, 9, 10, 11 ,1 ),
       caption = "Count models on the existence of successful new parties and the number of successful new parties 
                  that enter. A successful new party is defined as one that obtains enough votes to gain a seat in the legislature.",
       label="table:count_models", stars = c(0.001, 0.01, 0.05, 0.1),
       booktabs = F, dcolumn = F,  sideways = F)



###################################################
### Success of new parties white2 se,  footnote NNNNN
###################################################
p1.n.w.pse<-coeftest(p1.n, vcov=vcovHC(p1.n, method="white2"))

p1.w.pse<-coeftest(p1, vcov=vcovHC(p1, method="white2"))

p2.w.pse<-coeftest(p2, vcov=vcovHC(p2, method="white2"))

re.w.pse<-coeftest(re, vcov=vcovHC(re, method="white2"))

fe1.w.pse<-coeftest(fe1, vcov=vcovHC(fe1, method="white2"))


screenreg(list(p1.n.w.pse,p1.w.pse,p2.w.pse, vxf.pse  ,re.w.pse, fe1.w.pse, fe2.pse),stars = c(0.001, 0.01, 0.05, 0.1))


#############################################
### Bootstaped coefficients, Figures NNNNNNN
############################################

# # Bootstrap 95% CI for regression coefficients 
# 
# # function to obtain regression weights 
# # get 95% confidence interval 
# 
# set.seed(45264)
# 
# bs <- function(data,indices) {
#   d <- data[unique(indices),]
#   fit<-plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
#            + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
#            + lag(n.parties, 1)  #+ pub_fin_new
#            + log(as.numeric(elect.id)+1) 
#            + tier1_avemag #+ factor(fptp) 
#            + turnout
#            + Compulsory.voting
#            #+ thresh
#            #+ lag(enep1,1)  
#            + log(lag(np.vote, 1)+1)
#            ,data=d , index=c("country", "elect.id"), model="pooling", effect ="individual")
#    return(coef(fit))
# }
# 
# 
# # bootstrapping with 10000 replications 
# results <- boot(data=mydf, statistic=bs, 
#                 R=10000)
# 
# # view results
# results
# plot(results, index=2) # log(lag(share.v.crash.tp1, 2)+1)
# plot(results, index=3) # lag(c.location.mean.tp1,  2)  
# 
# png(filename= paste0(bd, "Boot_share_votes_robustness_1.png", sep=""))
# plot(results, index=2)
# dev.off()
# 
# png(filename= paste0(bd, "Boot_location_robustness_1.png", sep=""))
# plot(results, index=3)
# dev.off()
# 
# #png(filename= paste0(bd, "Boot_interaction.png", sep=""))
# #plot(results, index=14)
# #dev.off()
# 
# # get 95% confidence intervals 
# boot.ci(results, type="bca", index=2) # log(lag(share.v.crash.tp1, 2)+1)   
# boot.ci(results, type="bca", index=3, conf = 0.90) # lag(c.location.mean.tp1,  2)
# #boot.ci(results, type="bca", index=14) # llag(c.location.mean.tp1,  2)*lag(cmp.wpsd.s, 3)
# 
# 
# ###### Bootstrap countries
# #################################
# 
# set.seed(48956)
# c.data<-mydf[, c( "country", "year", "elect.id", "np.vote", "share.v.crash.tp1", "c.location.mean.tp1", "population.wdi", "gdp.pct.chg"
#                   , "cpi.wdi", "n.parties", "elect.id", "tier1_avemag", "fptp", "turnout", "cmp.wpsd.s", "Compulsory.voting")]
# 
# c.data$c.id<-as.numeric(c.data$country)
# 
# countries = unique(c.data$c.id)
# n = 33
# iterations = 10000
# mybootresults=list()
# 
# for(j in 1:iterations){
#   
#   v = sample(length(countries),n,replace=TRUE)
#   newdata = NULL
#   
#   for(i in 1:n){
#     newdata = rbind(newdata,subset(c.data, c.id == v[i]))
#   }
#   
#   reg1 <-plm(log(np.vote+1)~ log(lag(share.v.crash.tp1, 2)+1)+lag(c.location.mean.tp1,  2)#*lag(cmp.wpsd.s, 3)
#              + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
#              + lag(n.parties, 1)  #+ pub_fin_new
#              + log(as.numeric(elect.id)+1) 
#              + tier1_avemag #+ factor(fptp) 
#              + turnout
#              #+ Compulsory.voting
#              #+ thresh
#              #+ lag(enep1,1)  
#              + log(lag(np.vote, 1)+1)
#              ,data=newdata , index=c("country", "elect.id"), model="pooling", effect ="individual")
#   mybootresults[[j]] = coefficients(reg1)
#   
# }
# 
# mybootresults = as.data.frame(t(matrix(unlist(mybootresults),ncol=iterations)))
# names(mybootresults) = names(reg1$coefficients)
# View(mybootresults)
# 
# describe(mybootresults[, 2])
# describe(mybootresults[, 3])
# #describe(mybootresults[, 13])
# 
# png(filename=paste0(bd, "boot_c_vote_robustness_1.png", sep=""))
# mean<-mean(mybootresults[, 2])
# sd<-sd(mybootresults[, 2])
# ll<-mean-1.96*sd
# ul<-mean+1.96*sd
# hist(mybootresults[, 2], freq=F, xlab="Bootstrapped coef. Log % vote before collapse +1", main="")
# abline(v=ll, col = "red", lwd = 1)
# abline(v=ul, col = "red", lwd = 1)
# dev.off() 
# 
# 
# png(filename=paste0(bd, "boot_c_location_robustness_1.png", sep=""))
# mean<-mean(mybootresults[, 3])
# sd<-sd(mybootresults[, 3])
# ll<-mean-1.96*sd
# ul<-mean+1.96*sd
# hist(mybootresults[, 3], freq=F, xlab="Bootstrapped coef. Location collapsed party (t-2)", main="")
# abline(v=ll, col = "red", lwd = 1)
# abline(v=ul, col = "red", lwd = 1)
# dev.off() 

# png(filename=paste0(bd, "boot_c_interaction.png", sep=""))
# mean<-mean(mybootresults[, 13])
# sd<-sd(mybootresults[, 13])
# ll<-mean-1.96*sd
# ul<-mean+1.96*sd
# hist(mybootresults[, 13], freq=F, xlab="Bootstrapped coef. location*WPSD", main="")
# abline(v=ll, col = "red", lwd = 1)
# abline(v=ul, col = "red", lwd = 1)
# dev.off() 


## Other definitions of the dependent variable

#######################################
#### Interaction Plots, Figures NNNNNNN
########################################


###### Interaction of Vote share and fptp
#########################################

#### two-way model
iip3 <- lm(log(np.vote+1)~ log.share*fptp+c.location.mean.tp1.k2#*lag(cmp.wpsd.s, 3)
            +log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2) # + unempl.wdi
            + n.parties.k1  #+ pub_fin_new
            #+ log(as.numeric(elect.id)+1) 
            + tier1_avemag #+ factor(fptp) 
            + turnout
            + Compulsory.voting
            #+ thresh
            #+ lag(enep1,1)  
            + log(np.vote.k1+1)
           + factor(country_id)
           +factor(elect.id)
          ,data=mydf)
summary(iip3)


###### Predicted values
idata <- data.frame(predict(iip3), iip3$model$log.share, iip3$model$fptp)

ipe<-ggplot(idata, aes(x = iip3.model.log.share, y = predict.iip3., group = factor(iip3.model.fptp), color = factor(iip3.model.fptp))) 
ipe <- ipe + stat_smooth(method="lm") + geom_point()
ipe <- ipe + scale_colour_grey(name  ="Electoral System", breaks=c(0, 1), labels=c("Other", "FPTP")) 
ipe <- ipe + xlab("Log % votes collapsed party (t-2) +1")+ylab("Predicted log sum NP vote share")+ theme_bw()
ipe
ggsave(filename= paste0(bd, "fptpXsize_robustness_1.png", sep=""), height=6, width=6)






##################################################################
##### Interaction of size and Location of collapse and East Europe
##################################################################
### Lm model interaction location
ilm2xe <- lm(log(np.vote+1)~ log(share.v.crash.tp1.k2+1)+c.location.mean.tp1.k2*e.eu#*lag(cmp.uwpsd.s, 3)
             + e.eu
             + log(population.wdi) + gdp.pct.chg     +  log(cpi.wdi+2)  #+ unempl.wdi
             + n.parties.k1  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             + tier1_avemag #+ factor(fptp) 
             + turnout
             + Compulsory.voting
             #+ thresh
             #+ lag(enep1,1)  
             + log(np.vote.k1+1)
             ,data=mydf)
summary(ilm2xe)

####### Ploting on the predicted values
i2data <- data.frame(predict(ilm2xe), ilm2xe$model$c.location.mean.tp1.k2, ilm2xe$model$e.eu)

ipeu<-ggplot(i2data, aes(x = ilm2xe.model.c.location.mean.tp1.k2, y = predict.ilm2xe., 
                         group = factor(ilm2xe.model.e.eu), color = factor(ilm2xe.model.e.eu))) 
ipeu<- ipeu + stat_smooth(method="lm") + geom_point()
ipeu <- ipeu + scale_colour_grey(name  ="Region", breaks=c(0, 1), labels=c("Other", "East Europe")) 
ipeu <- ipeu + xlab("Mean Location collapsed party (t-2)(ies)")+ylab("Predicted log sum NP vote share")+ theme_bw()
ipeu
ggsave(filename= paste0(bd, "eeuXlocation_robustness_1.png", sep=""), height=6, width=6)


ilm2xe2 <- lm(log(np.vote+1)~ log.share*e.eu+c.location.mean.tp1.k2#*lag(cmp.uwpsd.s, 3)
             + e.eu
             + log(population.wdi) + gdp.pct.chg  +  log(cpi.wdi+2)  #+ unempl.wdi
             + n.parties.k1  #+ pub_fin_new
             + log(as.numeric(elect.id)+1) 
             + tier1_avemag #+ factor(fptp) 
             + turnout
             + Compulsory.voting
             #+ thresh
             #+ lag(enep1,1)  
             + log(np.vote.k1+1)
             ,data=mydf)
summary(ilm2xe2)

#### Interaction terms Plots

####### Ploting on the predicted values
i2data2 <- data.frame(predict(ilm2xe2), ilm2xe2$model$log.share, ilm2xe2$model$e.eu)

ipeu2<-ggplot(i2data2, aes(x = ilm2xe2.model.log.share, y = predict.ilm2xe2., 
                         group = factor(ilm2xe2.model.e.eu), color = factor(ilm2xe2.model.e.eu))) 
ipeu2<- ipeu2 + stat_smooth(method="lm") + geom_point()
ipeu2 <- ipeu2 + scale_colour_grey(name  ="Region", breaks=c(0, 1), labels=c("Other", "East Europe")) 
ipeu2 <- ipeu2 + xlab("Log % votes collapsed party (t-2) +1")+ylab("Predicted log sum NP vote share")+ theme_bw()
ipeu2
ggsave(filename= paste0(bd, "eeuXsize_robustness_1.png", sep=""), height=6, width=6)




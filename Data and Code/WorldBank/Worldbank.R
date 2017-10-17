
library(WDI)
library(zoo)
wdi <-  WDI(country = c("AU", "AT", "BE", "BG", "CA", "CZ", "DK", "EE", "FI", "FR", "DE", 
                        "GR", "HU", "IS", "IE", "IT", "LV", "LT", "NL",  "NZ", "NO", "PL", 
                        "PT", "RO", "SK", "SI", "ES",  "SE", "CH", "GB", "JP", "MT", "LU" ), 
            indicator = c("NY.GDP.PCAP.CD", "FP.CPI.TOTL.ZG","SL.UEM.TOTL.ZS","SP.POP.TOTL", "NY.GDP.PCAP.PP.CD" )
            ,start = 1960, end = 2012, extra = F)

wdi$country[wdi$country=="Slovak Republic"]<-"Slovakia"
wdi<-rename(wdi, c("NY.GDP.PCAP.CD"="gdp.pc.wdi",
                   "FP.CPI.TOTL.ZG" = "cpi.wdi",
                   "SL.UEM.TOTL.ZS" = "unempl.wdi", 
                   "SP.POP.TOTL"  = "population.wdi"     ,
                   "NY.GDP.PCAP.PP.CD" = "gdp.pc.ppp.wdi"))

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

wdi$gdp.pct.chg<- 100 * (( wdi$gdp.pc.wdi- lagpad( wdi$gdp.pc.wdi, 1))/lagpad( wdi$gdp.pc.wdi, 1))

wdi$gdp.pct.chg2<- 100 * (( wdi$gdp.pc.wdi- lagpad( wdi$gdp.pc.wdi, 2))/lagpad( wdi$gdp.pc.wdi, 2))


wdi$country_id<-as.numeric(wdi$country)
wdi <- transform(wdi, country_id=ave(rep(0,length(country)), country,
                                                 FUN=seq_along))


# eliminate the first observation for each country
wdi$gdp.pct.chg[wdi$country_id==1]<-NA
wdi$gdp.pct.chg2[wdi$country_id==1 | wdi$country_id==2 ]<-NA
wdi$country_id<-NULL

write.csv(file="WorldBank/wdi_data.csv", x=wdi)

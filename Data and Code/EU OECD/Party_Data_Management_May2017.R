rm(list=ls())

#setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 2/Data")
#bd<-"C:/Users/André Laroze/Dropbox/Essex PhD/Paper 2/Draft 1/"

library(plyr)
library(stringr)
library(xtable)
#library(data.table)


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
parties <- read.csv("EU OECD/view_party.csv", as.is=TRUE)
#cabinets <- read.csv("view_cabinet.csv", as.is=TRUE)
castles <- read.csv("EU OECD/external_party_castles_mair.csv", as.is=TRUE)
elections<-read.csv("EU OECD/view_election.csv", as.is=TRUE)



# Trim and rename variables in datasets for merging
castles<-rename(castles, c("left_right"="left_right_CM", "range_left"="range_left_cm","range_right"="range_right_cm" ))

# merge Castles/Mair positions to cabinet data
castles <- merge(castles, parties[,c('castles_mair', 'party_id', 'family_name_short', 'family_name', 'family_id')],
                 by.x='id', by.y='castles_mair', all.x=TRUE)
o.party.df <- merge(elections, castles[,c('party_id', 'left_right_CM',  'range_left_cm', 'range_right_cm')],
                  by='party_id', all.x=TRUE)

o.party.df <- o.party.df[order(o.party.df$country_name, o.party.df$election_date),]
o.party.df<-subset(o.party.df, election_type=="parliament")
o.party.df$election_type<-NULL

### Remove unnecessary data frames
rm(castles, elections, parties )


# e.g. limit observations to period 1975--1985 and parties with
#cabinets <- cabinets[cabinets$start_date >= '1975-01-01', ]


# create observations for seats share and distance from center
o.party.df$seat_share <- (o.party.df$seats / o.party.df$seats_total) * 100
o.party.df$left_right_extreme <- abs(o.party.df$left_right - 5)
o.party.df$year<-str_sub(o.party.df$election_date,0, -7)


df<-o.party.df

#renaming parties for practical use
df<-rename(df, c("party_name"="party_name_original", "party_name_english"="party_name" ))

#Eliminate votes for groups that do not have a party affiliation
df<- df[!df$party_name=="no party affiliation", ]
df<- df[!df$party_name=="no seats", ]
df<- df[!df$party_name=="others", ]
df<- df[!df$party_name_short=="none", ]
df<- df[!c(df$country_name=="Germany" & df$party_name=="Free Voters"), ] # a group of independentes, not a party

df$previous_cabinet_id<-NULL

#order for easness if checking results
df <- df[order(df$country_name, df$election_date, df$election_id),]

df$uid<-paste0(df$country_name, df$election_date)
df$elect.uid<-as.numeric(factor(df$uid))

df$s.seat.change<-NA
df$s.vote.change.tm1<-NA
df$vote.share.t1<-NA
df$seat.share.t1<-NA
df$s.vote.change.tp1<-NA
df$NP<-NA
for(i in 1:nrow(df)){ 
  metp1<-df$elect.uid == df$elect.uid[i]+1 & df$party_name==df$party_name[i]
  metm1<-df$election_id == df$previous_parliament_election_id[i] & df$party_name==df$party_name[i]
  metb<-df$country_name==df$country_name[i] & df$party_name==df$party_name[i] & df$elect.uid<df$elect.uid[i]
  df$s.seat.change[i]<- if(T %in% metm1) df$seats[i]/df$seats[metm1] else NA
  df$s.vote.change.tm1[i]<- if(T %in% metm1) df$vote_share[i]/df$vote_share[metm1] else NA
  df$seat.share.tm1[i]<- if(T %in% metm1) df$seat_share[metm1] else NA
  df$vote.share.tm1[i]<- if(T %in% metm1) df$vote_share[metm1] else NA
  df$s.vote.change.tp1[i]<- if(T %in% metp1) df$vote_share[metp1]/df$vote_share[i] else 0
  df$NP[i]<- if(T %in% metb) 0 else 1
  }


#check<-df[, c("country_name", "year" ,"party_name", "vote_share",  "s.vote.change.tm1", "s.vote.change.tp1", "NP")]
#View(subset(check, country_name=="United Kingdom")) #To check if the code worked
#####################
#### Cleaning DF
#####################

#####################
#### Cleaning NP
#####################



#check<-check[check$year>1945, ]

#check<-df[, c("country_name", "election_date", "year", "party_name_short" , "party_name", "NP", "vote_share", "left_right" )]

df$NP[df$year==1949 & df$country_name=="Austria" & df$party_name=="Communists and Left Socialists"]<-0 #The communist party already existed, this is a merger (party data observation)
df$NP[df$year==1986 & df$country_name=="Austria" & df$party_name=="The Greens -- The Green Alternative"]<-0 # Merger of the Alternative list and the Greens (Wikipedia)

df$NP[df$year==1968 & df$country_name=="Belgium" & df$party_name=="Walloon Rally"]<-0 # Merger of different Wallon parties (Wikipedia)
df$NP[df$year==1991 & df$country_name=="Belgium" & df$party_name=="National Front"]<-0 # Founded in 1985 (https://en.wikipedia.org/wiki/National_Front_(Belgium)
df$NP[df$year==1995 & df$country_name=="Belgium" & df$party_name=="Liberal Reformation Party / Francophone Democratic Front"]<-0 # Alliance (party data)

df$NP[df$year==1994 & df$country_name=="Bulgaria" & df$party_name=="Bulgarian People's Union"]<-0 # Name change/alliance (https://en.wikipedia.org/wiki/Bulgarian_parliamentary_election,_1994)
df$NP[df$year==1997 & df$country_name=="Bulgaria" & df$party_name=="Democratic Left"]<-0 # Old party reestablished in 1990 (https://en.wikipedia.org/wiki/Democratic_Party_(Bulgaria))
df$NP[df$year==1997 & df$country_name=="Bulgaria" & df$party_name=="Communist Party of Bulgaria"]<-0 # Founded in 1903 (https://en.wikipedia.org/wiki/Bulgarian_Communist_Party)
df$NP[df$year==1997 & df$country_name=="Bulgaria" & df$party_name=="Union for National Salvation"]<-0 # Alliance (https://en.wikipedia.org/wiki/Bulgarian_parliamentary_election,_1997)
df$NP[df$year==2001 & df$country_name=="Bulgaria" & df$party_name=="Coalition for Bulgaria"]<-0 #coalition lead by the Socialist party (https://en.wikipedia.org/wiki/Coalition_for_Bulgaria)
df$NP[df$year==2009 & df$country_name=="Bulgaria" & df$party_name=="Blue Coalition"]<-0 #Coalition of long standing parties (https://en.wikipedia.org/wiki/Blue_Coalition)

df$NP[df$year==2004 & df$country_name=="Canada" & df$party_name=="Conservative Party of Canada"]<-0# Merger https://en.wikipedia.org/wiki/Conservative_Party_of_Canada

df$NP[df$year==1992 & df$country_name=="Czech Republic" & df$party_name=="Liberal Social Union"]<-0 #political alliance (https://en.wikipedia.org/wiki/Liberal-Social_Union)
df$NP[df$year==2002 & df$country_name=="Czech Republic" & df$party_name=="Coalition"]<-0 #political alliance (http://www.ipu.org/parline-e/reports/arc/2083_02.htm)

df$NP[df$year==1995 & df$country_name=="Estonia" & df$party_name_short=="RKI/ERSP"]<-0 # Electoral Alliance (party data observation)
df$NP[df$year==1999 & df$country_name=="Estonia" & df$party_name=="Pro Patria Union"]<-0 # merger of parties (https://en.wikipedia.org/wiki/Pro_Patria_Union)
df$NP[df$year==1999 & df$country_name=="Estonia" & df$party_name=="Estonian Coalition Party"]<-0 #founded in 1991 (https://en.wikipedia.org/wiki/Estonian_Coalition_Party)
df$NP[df$year==2003 & df$country_name=="Estonia" & df$party_name=="People's Union of Estonia"]<-0 # merger and renaming of existing parties (https://en.wikipedia.org/wiki/People%27s_Union_of_Estonia)
df$NP[df$year==2007 & df$country_name=="Estonia" & df$party_name=="Union of Pro Patria and Res Publica"]<-0 # merger of existing parties (party data observation)

df$NP[df$year==1991 & df$country_name=="Finland" & df$party_name=="Left Alliance"]<-0 # merger of former parties (https://en.wikipedia.org/wiki/Left_Alliance_(Finland))
df$NP[df$year==2003 & df$country_name=="Finland" & df$party_name_short=="SKP-Y"]<-0 # reminantes of the old communist party that had survived in local elections (https://en.wikipedia.org/wiki/Communist_Party_of_Finland_(1997))

df$NP[df$year==1946 & df$country_name=="France" & df$party_name=="Rally of Republican Lefts"]<-0 # alliance of existing parties (https://en.wikipedia.org/wiki/Rally_of_Left_Republicans)
df$NP[df$year==1967 & df$country_name=="France" & df$party_name=="Democratic Centre"]<-0  # Merger of existing parties (https://en.wikipedia.org/wiki/Democratic_Centre_(France))
df$NP[df$year==1978 & df$country_name=="France" & df$party_name=="Union for French Democracy"]<-0 # Merger (https://en.wikipedia.org/wiki/Union_for_French_Democracy)
df$NP[df$year==2002 & df$country_name=="France" & df$party_name=="Union for a Popular Movement"]<-0 # Merger of major parties (https://en.wikipedia.org/wiki/Union_for_a_Popular_Movement)
df$NP[df$year==2002 & df$country_name=="France" & df$party_name=="Movement for France"]<-0 # Founded in 1994, previously ran in alliances (https://en.wikipedia.org/wiki/Movement_for_France)
df$NP[df$year==2007 & df$country_name=="France" & df$party_name=="Democratic Movement"]<-0 # party split / renaming of existing party (https://en.wikipedia.org/wiki/Democratic_Movement_(France))
df$NP[df$year==2012 & df$country_name=="France" & df$party_name=="Radical Party"]<-0 # Party founded in 1901, just slight name change (Radical Party)

df$NP[df$year==1994 & df$country_name=="Germany" & df$party_name=="Alliance 90 / Greens"]<-0 ## name reordering (pary data)

df$NP[df$year==1990 & df$country_name=="Greece" & df$party_name=="PASOK / Coalition"]<-0 # Coalition of existing parties (party data observation)
df$NP[df$year==2004 & df$country_name=="Greece" & df$party_name=="Coalition of the Radical Left"]<-0 # Coalition of left wing parties (https://en.wikipedia.org/wiki/Syriza)
df$NP[df$year==2009 & df$country_name=="Greece" & df$party_name=="Ecologist Greens"]<-0 # Participated in 2007 elections, but was too small to show up (https://en.wikipedia.org/wiki/Ecologist_Greens)

df$NP[df$year==1994 & df$country_name=="Hungary" & df$party_name=="Hungarian Communist Workers' Party"]<-0 # Name change from socialist workers party (https://en.wikipedia.org/wiki/Hungarian_Workers%27_Party)
df$NP[df$year==2002 & df$country_name=="Hungary" & df$party_name=="Fidesz -- Hungarian Civic Party / Hungarian Democratic Forum"]<-0 # Alliance (party data)
df$NP[df$year==2006 & df$country_name=="Hungary" & df$party_name_short=="Fidesz/KDNP"]<-0 # Alliance (party data)

df$NP[df$year==1999 & df$country_name=="Iceland" & df$party_name=="Social Democratic Alliance"]<-0 # merger of existing parties (https://en.wikipedia.org/wiki/Social_Democratic_Alliance)

df$NP[df$year==1957 & df$country_name=="Ireland" & df$party_name=="Sinn Fein"]<-0 # Old party crated in 1905 (https://www.google.co.uk/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=sinn%20fein)
df$NP[df$year==1957 & df$country_name=="Ireland" & df$party_name=="Communist Party"]<-0 # Old party with representation before the 1940's (https://en.wikipedia.org/wiki/Communist_Party_of_Ireland)
df$NP[df$year==1987 & df$country_name=="Ireland" & df$party_name=="Democratic Socialist Party"]<-0 # founded in 1972 (https://en.wikipedia.org/wiki/Democratic_Socialist_Party_(Ireland))
df$NP[df$year==2011 & df$country_name=="Ireland" & df$party_name=="People Before Profit Alliance"]<-0 # founded in 2005 contested elections but was too small to show up (https://en.wikipedia.org/wiki/People_Before_Profit_Alliance)

df$NP[df$year==1948 & df$country_name=="Italy" & df$party_name=="Popular Democratic Front"]<-0 # Alliance (https://en.wikipedia.org/wiki/Popular_Democratic_Front_(Italy))
df$NP[df$year==1948 & df$country_name=="Italy" & df$party_name=="National Bloc"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/National_Bloc_(Italy))
df$NP[df$year==1953 & df$country_name=="Italy" & df$party_name=="Italian Liberal Party"]<-0 # long standing party, previously in electoral alliance "National Bloc" (https://en.wikipedia.org/wiki/Italian_Liberal_Party)
df$NP[df$year==1958 & df$country_name=="Italy" & df$party_name=="Valdotanian Union"]<-0 # Party founded in 1945, previously in alliances or too small to appear (https://en.wikipedia.org/wiki/Valdostan_Union)
df$NP[df$year==1963 & df$country_name=="Italy" & df$party_name=="Italian Democratic Party of Monarchist Unity"]<-0 # Merger (https://en.wikipedia.org/wiki/Italian_Democratic_Party_of_Monarchist_Unity)
df$NP[df$year==1968 & df$country_name=="Italy" & df$party_name=="Unified Socialist Party"]<-0 # Alliance (https://en.wikipedia.org/wiki/Unified_Socialist_Party_(Italy))
df$NP[df$year==1976 & df$country_name=="Italy" & df$party_name=="Radicals"]<-0 # existed before, probably just too small to appear en the dataset (https://en.wikipedia.org/wiki/Radical_Party_(Italy))
# Italy 1994 first election,  all alliances are kept in because they contain multiple new parties and votes are associated to the alliances
df$NP[df$year==1994 & df$country_name=="Italy" & df$party_name=="PSI/AD"]<-0 # alliance (party data)
df$NP[df$year==1996 & df$country_name=="Italy" & df$party_name=="Popular Party for Prodi"]<-0 # founded in 1994 (https://en.wikipedia.org/wiki/Italian_People%27s_Party_(1994))
df$NP[df$year==1996 & df$country_name=="Italy" & df$party_name=="The Olive Tree"]<-0 # Electoral aliance of exisiting parties (https://en.wikipedia.org/wiki/The_Olive_Tree_(Italy))
df$NP[df$year==2001 & df$country_name=="Italy" & df$party_name=="Greens and Social Democrats"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Federation_of_the_Greens)
#df$NP[df$year==2001 & df$country_name=="Italy" & df$party_name=="Democracy is Freedom -- The Daisy"]<-0 # merger that included new parties that were the main members (https://en.wikipedia.org/wiki/Democracy_is_Freedom_%E2%80%93_The_Daisy)
df$NP[df$year==2006 & df$country_name=="Italy" & df$party_name=="Union of Christian and Centre Democrats"]<-0 # merger (https://en.wikipedia.org/wiki/Union_of_the_Centre_(2002))
df$NP[df$year==2006 & df$country_name=="Italy" & df$party_name=="Rose in the Fist"]<-0 # coalition of parties (https://en.wikipedia.org/wiki/Rose_in_the_Fist)
df$NP[df$year==2006 & df$country_name=="Italy" & df$party_name=="Autonomy Liberty Democracy"]<-0 # party alliance (https://en.wikipedia.org/wiki/Autonomy_Liberty_Democracy)
df$NP[df$year==2006 & df$country_name=="Italy" & df$party_name=="The Union-Prodi"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/The_Union_(Italy))
df$NP[df$year==2006 & df$country_name=="Italy" & df$party_name=="Chrisitan Democracy / New PSI"]<-0 # electoral alliance (party data)
df$NP[df$year==2008 & df$country_name=="Italy" & df$party_name=="The People of Freedom"]<-0 # electoral alliance of existing parties (https://en.wikipedia.org/wiki/The_People_of_Freedom)
df$NP[df$year==2008 & df$country_name=="Italy" & df$party_name=="Democratic Party"]<-0 # merger (https://en.wikipedia.org/wiki/Democratic_Party_(Italy))
df$NP[df$year==2008 & df$country_name=="Italy" & df$party_name=="Union of the Centre"]<-0 # name change (https://en.wikipedia.org/wiki/Union_of_the_Centre_(2002))  
df$NP[df$year==2008 & df$country_name=="Italy" & df$party_name=="The Right Tricolor Flame"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/The_Right%E2%80%93Tricolour_Flame)
df$NP[df$year==2008 & df$country_name=="Italy" & df$party_name=="The Left The Rainbow"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/The_Left_%E2%80%93_The_Rainbow)

df$NP[df$year==1958 & df$country_name=="Japan" & df$party_name=="Liberal Democratic Party"]<-0 # merger (https://en.wikipedia.org/wiki/Japan_Democratic_Party_%281954%29)
df$NP[df$year==1996 & df$country_name=="Japan" & df$party_name=="New Frontier Party"]<-0 # merger (https://en.wikipedia.org/wiki/New_Frontier_Party_(Japan))

df$NP[df$year==1993 & df$country_name=="Latvia" & df$party_name=="Latvian Green Party"]<-0 # contested previous elections, results not in dataset (https://en.wikipedia.org/wiki/Latvian_Green_Party)
df$NP[df$year==1993 & df$country_name=="Latvia" & df$party_name=="Latvian National Independence Movement"]<-0 # Part of the independence movement created in 1988 (https://en.wikipedia.org/wiki/Latvian_National_Independence_Movement)
df$NP[df$year==1995 & df$country_name=="Latvia" & df$party_name=="Latvian National Independence Movement / Latvian Green Party"]<-0 # electoral alliance (party data)
df$NP[df$year==1995 & df$country_name=="Latvia" & df$party_name=="Christian Democratic Union / Farmers Union of Latvia"]<-0 # electoral alliance (party data )
df$NP[df$year==1995 & df$country_name=="Latvia" & df$party_name=="Latvian Unity Party"]<-0 # formed in 1992 contested eleccions before only 0.1% of votes (https://en.wikipedia.org/wiki/Latvian_Unity_Party)
df$NP[df$year==1998 & df$country_name=="Latvia" & df$party_name=="For Fatherland and Freedom / LNNK"]<-0 # electoral alliance (party data )
df$NP[df$year==1998 & df$country_name=="Latvia" & df$party_name=="Latvia's Union of Social Democrats"]<-0 # merger (https://en.wikipedia.org/wiki/Latvian_Social_Democratic_Workers%27_Party)
df$NP[df$year==2002 & df$country_name=="Latvia" & df$party_name=="Latvian Social Democratic Workers' Party"]<-0 # renaming of existing party (idem)
df$NP[df$year==2002 & df$country_name=="Latvia" & df$party_name=="Green and Farmers' Union"]<-0 # political alliance (https://en.wikipedia.org/wiki/Union_of_Greens_and_Farmers)
df$NP[df$year==2006 & df$country_name=="Latvia" & df$party_name=="Latvian First Party / Latvian Way Party"]<-0 # merger (https://en.wikipedia.org/wiki/Latvia%27s_First_Party/Latvian_Way)
df$NP[df$year==2006 & df$country_name=="Latvia" & df$party_name=="Harmony Centre"]<-0 # alliance than merger (https://en.wikipedia.org/wiki/Harmony_Centre)
df$NP[df$year==2010 & df$country_name=="Latvia" & df$party_name=="For a Good Latvia"]<-0 # aliance (https://en.wikipedia.org/wiki/For_a_Good_Latvia)

df$NP[df$year==1992 & df$country_name=="Lithuania" & df$party_name=="Lithuanian Christian Democrats / Union of Lithuanian Political Prisoners and Deportees / Lithuanian Democratic Party"]<-0 # political alliance - majority existing parties (party data)
df$NP[df$year==1992 & df$country_name=="Lithuania" & df$party_name=="Sajudis coalition"]<-0 # participated in independence movement (https://en.wikipedia.org/wiki/S%C4%85j%C5%ABdis)
df$NP[df$year==1992 & df$country_name=="Lithuania" & df$party_name=="Democratic Labour Party of Lithuania"]<-0 # founded in 1989 (https://en.wikipedia.org/wiki/Democratic_Labour_Party_of_Lithuania)
df$NP[df$year==1996 & df$country_name=="Lithuania" & df$party_name=="Liberal Union of Lithuania"]<-0 # name change, still has same "short name" (party data)
df$NP[df$year==1996 & df$country_name=="Lithuania" & df$party_name=="Union of Lithuanian Political Prisoners and Deportees"]<-0 # existed before in alliance (party data)
df$NP[df$year==1996 & df$country_name=="Lithuania" & df$party_name=="Lithuanian National Union List -- Liberal Democratic Party"]<-0 #electoral alliance (party data)
df$NP[df$year==2000 & df$country_name=="Lithuania" & df$party_name=="Young Lithuania / Union of Political Prisoners"]<-0 # alliance of exisiting parties (party data)
df$NP[df$year==2000 & df$country_name=="Lithuania" & df$party_name=="Brazauskas Social Democratic Coalition"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Lithuanian_parliamentary_election,_2000)
df$NP[df$year==2004 & df$country_name=="Lithuania" & df$party_name=="Working for Lithuania"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Lithuanian_parliamentary_election,_2004)
df$NP[df$year==2004 & df$country_name=="Lithuania" & df$party_name=="Peasant Party / New Democratic Party"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Lithuanian_parliamentary_election,_2004)
df$NP[df$year==2004 & df$country_name=="Lithuania" & df$party_name=="Christian Conservative Social Union"]<-0 # renaming (https://en.wikipedia.org/wiki/Christian_Conservative_Social_Union)
df$NP[df$year==2004 & df$country_name=="Lithuania" & df$party_name=="Coalition of Rolandas Paksas 'For Order and Justice'"]<-0 #2 elections in 1 year. small name change party data 
df$NP[df$year==2008 & df$country_name=="Lithuania" & df$party_name=="Homeland Union -- Lithuanian Christian Democrats"]<-0 # merger name change (https://en.wikipedia.org/wiki/Homeland_Union)
df$NP[df$year==2008 & df$country_name=="Lithuania" & df$party_name=="Lithuanian Peasant Popular Union"]<-0 # name change coalition (https://en.wikipedia.org/wiki/Lithuanian_Peasant_and_Greens_Union)

df$NP[df$year== 1947  & df$country_name=="Malta" & df$party_name=="Nationalist Party"]<-0 # Old party founded 1880 (https://en.wikipedia.org/wiki/Nationalist_Party_%28Malta%29)

df$NP[df$year==1963  & df$country_name=="Netherlands" & df$party_name=="Reformed Political League"]<-0 # founded in 1948 but too small of appear in df (https://en.wikipedia.org/wiki/Reformed_Political_League)
df$NP[df$year==1963  & df$country_name=="Netherlands" & df$party_name=="Farmers Party"]<-0# Founded 1958 contested general elections 1959 (https://en.wikipedia.org/wiki/Farmers%27_Party_(Netherlands))     
df$NP[df$year==1977  & df$country_name=="Netherlands" & df$party_name=="Christian Democrats"]<-0# merger (https://en.wikipedia.org/wiki/Christian_Democratic_Appeal)
df$NP[df$year==1981  & df$country_name=="Netherlands" & df$party_name=="Reformatory Political Federation"]<-0 # contested elections in 1977 (no seats) (https://en.wikipedia.org/wiki/Reformatory_Political_Federation)
df$NP[df$year==1982  & df$country_name=="Netherlands" & df$party_name=="Center Party"]<-0 # contested the 1981 elections (no seats) (https://en.wikipedia.org/wiki/Centre_Party_(Netherlands))
df$NP[df$year==1982  & df$country_name=="Netherlands" & df$party_name=="Evangelical Peoples Party"]<-0 # contested the 1981 elections (no seats) (https://en.wikipedia.org/wiki/Evangelical_People%27s_Party_%28Netherlands%29)
df$NP[df$year==1989  & df$country_name=="Netherlands" & df$party_name=="Centre Democrats"]<-0 # contested the 1986 elections (no seats) (https://en.wikipedia.org/wiki/Centre_Democrats_(Netherlands))
df$NP[df$year==1989  & df$country_name=="Netherlands" & df$party_name=="GreenLeft"]<-0 # merger (https://en.wikipedia.org/wiki/GreenLeft)
df$NP[df$year==2002  & df$country_name=="Netherlands" & df$party_name=="ChristianUnion -- Reformed Political Party"]<-0 # merger (https://en.wikipedia.org/wiki/ChristianUnion)
df$NP[df$year==2006  & df$country_name=="Netherlands" & df$party_name=="Party for the Animals"]<-0 # founded in 2002 (no seats) (https://en.wikipedia.org/wiki/Party_for_the_Animals)

df$NP[df$year==1946  & df$country_name=="New Zealand" & df$party_name=="Communist Party of New Zealand"]<-0 # founded in 1921 (https://en.wikipedia.org/wiki/Communist_Party_of_New_Zealand)
df$NP[df$year==1954  & df$country_name=="New Zealand" & df$party_name=="Social Credit / Democratic Party"]<-0 # alliance/ merger (party data)
df$NP[df$year==1993  & df$country_name=="New Zealand" & df$party_name=="Alliance"]<-0 # merger (https://en.wikipedia.org/wiki/Alliance_%28New_Zealand_political_party%29)
df$NP[df$year==1993  & df$country_name=="New Zealand" & df$party_name=="Christian Heritage Party of New Zealand"]<-0 # contested it's first election in 1990 (no seats) (https://en.wikipedia.org/wiki/Christian_Heritage_Party_of_New_Zealand)
df$NP[df$year==2002  & df$country_name=="New Zealand" & df$party_name=="United Future New Zealand"]<-0 # merger (https://en.wikipedia.org/wiki/United_Future)

df$NP[df$year==1957  & df$country_name=="Norway" & df$party_name=="Red Electoral Alliance"]<-0 # merger (https://en.wikipedia.org/wiki/Red_Electoral_Alliance)
df$NP[df$year==1993  & df$country_name=="Norway" & df$party_name=="Pensioners Party"]<-0 # contested elections since 1985 (no seats) (https://en.wikipedia.org/wiki/Pensioners%27_Party_(Norway))
df$NP[df$year==2009  & df$country_name=="Norway" & df$party_name=="Red"]<-0 #merger and change of name (https://en.wikipedia.org/wiki/Red_Party_(Norway))

df$NP[df$year==1993  & df$country_name=="Poland" & df$party_name_short=="O"]<-0 # electoral alliance (https://pl.wikipedia.org/wiki/Katolicki_Komitet_Wyborczy_%E2%80%9EOjczyzna%E2%80%9D)
df$NP[df$year==1997  & df$country_name=="Poland" & df$party_name=="Freedom Union -- Democratic Party"]<-0 #merger (https://en.wikipedia.org/wiki/Freedom_Union_(Poland))
df$NP[df$year==1997  & df$country_name=="Poland" & df$party_name=="Solidarity Electoral Action"]<-0 # coalition/merger (https://en.wikipedia.org/wiki/Solidarity_Electoral_Action)
df$NP[df$year==2001  & df$country_name=="Poland" & df$party_name=="Democratic Left Alliance / Labor Union"]<-0 # Alliance (party data)
df$NP[df$year==2007  & df$country_name=="Poland" & df$party_name=="Left and Democrats"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Left_and_Democrats)

df$NP[df$year==1979  & df$country_name=="Portugal" & df$party_name=="United People Alliance"]<-0 # coalition (https://en.wikipedia.org/wiki/United_People_Alliance)
df$NP[df$year==1979  & df$country_name=="Portugal" & df$party_name=="Popular Monarchist Party"]<-0 # founded in 1974, too small to appear before in results (https://en.wikipedia.org/wiki/People%27s_Monarchist_Party_%28Portugal%29)
df$NP[df$year==1979  & df$country_name=="Portugal" & df$party_name=="Christian Democratic Peoples Party"]<-0 # founded in 1974, too small to appear before in results (https://pt.wikipedia.org/wiki/Partido_da_Democracia_Crist%C3%A3)
df$NP[df$year==1979  & df$country_name=="Portugal" & df$party_name=="Democratic Alliance"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Democratic_Alliance_(Portugal))
df$NP[df$year==1980  & df$country_name=="Portugal" & df$party_name=="Revolutionary Socialist Party"]<-0 # ran in previous election but was too small to appear in results (https://en.wikipedia.org/wiki/Revolutionary_Socialist_Party_%28Portugal%29)
df$NP[df$year==1980  & df$country_name=="Portugal" & df$party_name=="Republican and Socialist Front"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Republican_and_Socialist_Front)
df$NP[df$year==1987  & df$country_name=="Portugal" & df$party_name=="Unified Democratic Coalition"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/Unitary_Democratic_Coalition)
df$NP[df$year==1995  & df$country_name=="Portugal" & df$party_name=="Communist Party of the Portuguese Workers / Reorganizative Movement of the Party of the Proletariat"]<-0 # founded in 1970, too small before (https://en.wikipedia.org/wiki/Portuguese_Workers%27_Communist_Party)
df$NP[df$year==1999  & df$country_name=="Portugal" & df$party_name=="Bloc of the Left"]<-0 # merger/persistent alliance (https://en.wikipedia.org/wiki/Left_Bloc)

df$NP[df$year==1992  & df$country_name=="Romania" & df$party_name=="Romanian Democratic Convention"]<-0 #political alliance (https://en.wikipedia.org/wiki/Romanian_Democratic_Convention)
df$NP[df$year==1992  & df$country_name=="Romania" & df$party_name=="Liberal Party 93 / Democratic Convention"]<-0 # political alliance (https://en.wikipedia.org/wiki/Romanian_Democratic_Convention)
df$NP[df$year==1996  & df$country_name=="Romania" & df$party_name=="National Liberal Party -- Democratic Convention"]<-0 # political alliance of existing parties (party data)
df$NP[df$year==1996  & df$country_name=="Romania" & df$party_name=="Social Democratic Party"]<-0 #merger and name change (https://en.wikipedia.org/wiki/Social_Democratic_Party_(Romania))
df$NP[df$year==2004  & df$country_name=="Romania" & df$party_name=="National Union PSD+PUR"]<-0 # electoral alliance (https://en.wikipedia.org/wiki/National_Union_PSD%2BPUR)
df$NP[df$year==2004  & df$country_name=="Romania" & df$party_name=="Justice and Truth Alliance"]<-0 # electoral/political alliance (https://en.wikipedia.org/wiki/Justice_and_Truth_Alliance)
df$NP[df$year==2008  & df$country_name=="Romania" & df$party_name=="Democratic Liberal Party"]<-0 # merger (https://en.wikipedia.org/wiki/Democratic_Liberal_Party_%28Romania%29)

df$NP[df$year==1994  & df$country_name=="Slovakia" & df$party_name=="Communist Party of Slovakia"]<-0 # merger (https://en.wikipedia.org/wiki/Communist_Party_of_Slovakia)
df$NP[df$year==1994  & df$country_name=="Slovakia" & df$party_name=="Common Choice"]<-0 # coalition (https://sk.wikipedia.org/wiki/Spolo%C4%8Dn%C3%A1_vo%C4%BEba)
df$NP[df$year==1998  & df$country_name=="Slovakia" & df$party_name=="Slovak Democratic Coalition"]<-0 # electoral alliance/coalition (https://en.wikipedia.org/wiki/Slovak_Democratic_Coalition)
df$NP[df$year==2006  & df$country_name=="Slovakia" & df$party_name=="Slovak Democratic and Christian Union -- Democratic Party"]<-0 # alliance (party data)
df$NP[df$year==2012  & df$country_name=="Slovakia" & df$party_name=="Change from the Bottom, Democratic Union of Slovakia"]<-0 #name change (https://en.wikipedia.org/wiki/List_of_political_parties_in_Slovakia)
df$NP[df$year==2012  & df$country_name=="Slovakia" & df$party_name_short=="SSS"]<-0 # name change (https://en.wikipedia.org/wiki/Idea_(political_party))

df$NP[df$year==1979  & df$country_name=="Spain" & df$party_name=="Canary Peoples Union"]<-0 # coalition (https://es.wikipedia.org/wiki/Uni%C3%B3n_del_Pueblo_Canario)
df$NP[df$year==1979  & df$country_name=="Spain" & df$party_name=="Andalusian Party"]<-0 # founded in 1976 (no previous seats) (https://en.wikipedia.org/wiki/Andalusian_Party)
df$NP[df$year==1993  & df$country_name=="Spain" & df$party_name=="Canary Coalition"]<-0 # coalition (https://en.wikipedia.org/wiki/Canarian_Coalition)
df$NP[df$year==2004  & df$country_name=="Spain" & df$party_name=="Navarre Yes"]<-0 # coalition/merger (https://en.wikipedia.org/wiki/Nafarroa_Bai)
df$NP[df$year==2011  & df$country_name=="Spain" & df$party_name=="Socialists' Party of Catalonia"]<-0 #founded 1978, renaming in coalitions in between (https://en.wikipedia.org/wiki/Socialists%27_Party_of_Catalonia)
df$NP[df$year==2011  & df$country_name=="Spain" & df$party_name=="Amaiur"]<-0 # coalition (https://en.wikipedia.org/wiki/Amaiur)
df$NP[df$year==2011  & df$country_name=="Spain" & df$party_name=="Yes to the future"]<-0 # coalition (https://en.wikipedia.org/wiki/Geroa_Bai)

df$NP[df$year==1964  & df$country_name=="Sweden" & df$party_name=="Middle Parties"]<-0 # coalition in some constituencies (https://en.wikipedia.org/wiki/Swedish_general_election,_1964) 
df$NP[df$year==1998  & df$country_name=="Sweden" & df$party_name=="Swedish Senior Citizen Interest Party"]<-0 # founded in 1987 (previously too small to appear in results) (https://en.wikipedia.org/wiki/Swedish_Senior_Citizen_Interest_Party)
df$NP[df$year==2002 & df$country_name=="Sweden" & df$party_name=="Sweden Democrats"]<-0 # founded in 1988 (previously too small) (https://en.wikipedia.org/wiki/Sweden_Democrats)

df$NP[df$year==1975  & df$country_name=="Switzerland" & df$party_name=="Progressive Organisations of Switzerland"]<-0 # First election in 1971, too small (https://en.wikipedia.org/wiki/Swiss_Progressive_Organisations)
df$NP[df$year==1983  & df$country_name=="Switzerland" & df$party_name=="Committee Herbert Maeder"]<-0 # an independent (https://de.wikipedia.org/wiki/Herbert_M%C3%A4der)
df$NP[df$year==2011  & df$country_name=="Switzerland" & df$party_name=="FDP.The Liberals"]<-0 # merger (https://en.wikipedia.org/wiki/FDP.The_Liberals)

df$NP[df$year==1950 & df$country_name=="United Kingdom" & df$party_name=="Conservatives and National Liberals"]<-0 # Alliance (party data)
df$NP[df$year==1951 & df$country_name=="United Kingdom" & df$party_name=="Irish Labout Party"]<-0    # probably a typo, the labour party in Ireland is from 1912 (https://en.wikipedia.org/wiki/Labour_Party_(Ireland))
df$NP[df$year==1955 & df$country_name=="United Kingdom" & df$party_name=="Plaid Cymru"]<-0  # Formed in the early 20th century, but didin't appear before because of the low amonf of votes (https://en.wikipedia.org/wiki/Plaid_Cymru)
df$NP[df$year==1955 & df$country_name=="United Kingdom" & df$party_name=="Sinn Fein"]<-0 # Old party created in 1905 (https://www.google.co.uk/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=sinn%20fein)
df$NP[df$year==1983 & df$country_name=="United Kingdom" & df$party_name_short=="Alliance"]<-0 #merger SDP-Liberal Alliance
df$NP[df$year==1992 & df$country_name=="United Kingdom" & df$party_name=="Liberal Democrats"]<-0 #formed before under the name SDP-Liberal Alliance (https://en.wikipedia.org/wiki/Social_Democratic_Party_(UK))
df$NP[df$year==2010 & df$country_name=="United Kingdom" & df$party_name=="Alliance Party of Northern Ireland"]<-0 # long standing (very small) party (https://en.wikipedia.org/wiki/Alliance_Party_of_Northern_Ireland)




# ### Polarization measure
# w.polar<- function(votes, location){
#   p.votes<-votes/100
#   location<-ifelse(is.na(location), 5, location)
#   p.votes<-ifelse(is.na(p.votes), 0, p.votes)
#   mean.position=sum(p.votes*location)
#   wpsd<-sqrt(sum(p.votes*(location-mean.position)^2))
#   return(wpsd)
# } 
# 
# uw.polar<- function(votes, location){
#   p.votes<-votes/100
#   location<-ifelse(is.na(location), 5, location)
#   p.votes<-ifelse(is.na(p.votes), 0, p.votes)
#   mean.position=sum(p.votes*location)
#   uwpsd<-sqrt(sum((location-mean.position)^2)/length(location))
#   return(uwpsd)
# } 
# 
# polar.m <- ddply(df, c("elect.uid","country_name", "year"), summarise, 
#             wpsd.m = w.polar(vote_share, left_right),
#             uwpsd.m = uw.polar(vote_share, left_right)
#             )
# 
# df.s<-df[complete.cases(df$left_right), ]
# df.s<-df.s[complete.cases(df.s$vote_share), ]
# 
# 
# polar.s<- ddply(df.s, c("elect.uid","country_name", "year"), summarise, 
#                 wpsd.s = w.polar(vote_share, left_right),
#                 uwpsd.s = uw.polar(vote_share, left_right)
# )




#### New party with seats in the legislature
df$S.NP<-0
df$S.NP[df$NP==1 & df$seats>=1]<-1

df$np.vote<-0
df$np.vote[df$NP==1]<-df$vote_share[df$NP==1]



##########################################
#### Creating and Cleaning Collapsed Party
##########################################
df$collapse.vote.tm1<-0
df$collapse.vote.tm1[df$s.vote.change.tm1<.5]<-1 # relative to the past, conservative measure

df$collapse.vote.tp1<-0
df$collapse.vote.tp1[df$s.vote.change.tp1<.5]<-1 # relative to the future, counts parties as collapsing when then merge or have name changes


#check<-df[, c("country_name", "election_date", "year", "party_name_short" ,"party_name", "NP" ,"vote_share", "collapse.vote.tp1", "s.vote.change.tp1", "elect.uid"  )]

df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Australia"])]<-NA

df$collapse.vote.tp1[df$year==1945 & df$country_name=="Austria" & df$party_name=="Communist Party of Austria"]<-0 #moved onto a merger/alliance
df$collapse.vote.tp1[df$year==1990 & df$country_name=="Austria" & df$party_name=="United Greens Austria"]<-0 # #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Austria"])]<-NA

df$collapse.vote.tp1[df$year==1965 & df$country_name=="Belgium" & df$party_name=="Walloon Front"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1965 & df$country_name=="Belgium" & df$party_name=="Walloon Labour Party"]<-0 # moved onto a merger
df$collapse.vote.tp1[df$year==2007 & df$country_name=="Belgium" & df$party_name=="Christian-Democrat and Flemish/New Flemish Alliance"]<-0 # alliance that ended
df$collapse.vote.tp1[df$year==2007 & df$country_name=="Belgium" & df$party_name=="Socialist Party Different / Social Liberal Party"]<-0 # alliance that ended
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Belgium"])]<-NA

df$collapse.vote.tp1[df$year==1991 & df$country_name=="Bulgaria" & df$party_name=="Agrarian People's Union"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1991 & df$country_name=="Bulgaria" & df$party_name=="United Democratic Forces"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Bulgaria" & df$party_name=="Movement for Rights and Freedoms"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Bulgaria" & df$party_name=="Bulgarian People's Union"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Bulgaria" & df$party_name=="Union of Democratic Forces-Liberal"]<-0 #moved onto a merger/name change
df$collapse.vote.tp1[df$year==1997 & df$country_name=="Bulgaria" & df$party_name=="Union for National Salvation"]<-0 #moved onto a different alliance
df$collapse.vote.tp1[df$year==1997 & df$country_name=="Bulgaria" & df$party_name=="Communist Party of Bulgaria"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1997 & df$country_name=="Bulgaria" & df$party_name==""]<-0 #moved onto a different alliance
df$collapse.vote.tp1[df$year==2005 & df$country_name=="Bulgaria" & df$party_name=="Democrats for a Strong Bulgaria"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2005 & df$country_name=="Bulgaria" & df$party_name=="United Democratic Forces"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Bulgaria"])]<-NA

df$collapse.vote.tp1[df$year==2000 & df$country_name=="Canada" & df$party_name=="Canadian Alliance"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2000 & df$country_name=="Canada" & df$party_name=="Progressive Conservative Party of Canada"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Canada"])]<-NA

df$collapse.vote.tp1[df$year==1992 & df$country_name=="Czech Republic" & df$party_name=="Liberal Social Union"]<-0 #desolution of alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Czech Republic"])]<-NA

df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Denmark"])]<-NA

df$collapse.vote.tp1[df$year==1995 & df$country_name=="Estonia" & df$party_name_short=="RKI/ERSP"]<-0 #alliance desolved
df$collapse.vote.tp1[df$year==1999 & df$country_name=="Estonia" & df$party_name=="Estonian Country People's Party"]<-0 #moved onto a renaming/merger
df$collapse.vote.tp1[df$year==2003 & df$country_name=="Estonia" & df$party_name=="Res Publica Party"]<-0 #moved onto a merger/alliance
df$collapse.vote.tp1[df$year==2003 & df$country_name=="Estonia" & df$party_name=="Pro Patria Union"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Estonia"])]<-NA

df$collapse.vote.tp1[df$year==1987 & df$country_name=="Finland" & df$party_name=="Finish People's Democratic Union"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Finland"])]<-NA

df$collapse.vote.tp1[df$year==1946 & df$country_name=="France" & df$party_name=="Rally of Republican Lefts"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==1962 & df$country_name=="France" & df$party_name=="Popular Republican Movement"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1997 & df$country_name=="France" & df$party_name=="Rally for the Republic"]<-0 #moved onto a merger/alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="France"])]<-NA

df$collapse.vote.tp1[df$year==1990 & df$country_name=="Germany" & df$party_name=="Greens / Alliance 90"]<-0 #slight name change
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Germany"])]<-NA

df$collapse.vote.tp1[df$year==1990 & df$country_name=="Greece" & df$party_name=="PASOK / Coalition"]<-0 # desolved alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Greece"])]<-NA

df$collapse.vote.tp1[df$year==1990 & df$country_name=="Hungary" & df$party_name=="Hungarian Socialist Workers' Party"]<-0 #name change
df$collapse.vote.tp1[df$year==2002 & df$country_name=="Hungary" & df$party_name=="Fidesz -- Hungarian Civic Party / Hungarian Democratic Forum"]<-0 #desolved alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Hungary"])]<-NA

df$collapse.vote.tp1[df$year==1995 & df$country_name=="Iceland"  & df$party_name=="Social Democratic Party"]<-0 #moved onto a merger/alliance
df$collapse.vote.tp1[df$year==1995 & df$country_name=="Iceland"  & df$party_name=="Womens Alliance"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1995 & df$country_name=="Iceland"  & df$party_name=="People's Alliance"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Iceland" ])]<-NA

df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Ireland"  ])]<-NA

df$collapse.vote.tp1[df$year==1946 & df$country_name=="Italy"   & df$party_name=="Front of the Ordinary Man"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1948 & df$country_name=="Italy"   & df$party_name=="National Bloc"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==1963 & df$country_name=="Italy"   & df$party_name=="Italian Democratic Socialist Party"]<-0 #moved into alliance
df$collapse.vote.tp1[df$year==1963 & df$country_name=="Italy"   & df$party_name=="Italian Socialist Party"]<-0 #moved into alliance
df$collapse.vote.tp1[df$year==1968 & df$country_name=="Italy"   & df$party_name=="Unified Socialist Party"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Italy"]<-0 #Parties were mainly realligned into different coalitiones, some of which changed again in 1996
df$collapse.vote.tp1[df$year==2001 & df$country_name=="Italy"   & df$party_name=="European Democracy"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2001 & df$country_name=="Italy"   & df$party_name=="Christian Democratic Center / United Christian Democrats"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2001 & df$country_name=="Italy"   & df$party_name=="Radicals"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2001 & df$country_name=="Italy"   & df$party_name=="Valdotanian Union"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="Go Italy"]<-0 #Name change, Berlusconi's creates new alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="Federation of the Greens"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="The Olive Tree"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="Rose in the Fist"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="The Union-Prodi"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="Communist Refoundation Party"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year== 2006& df$country_name=="Italy"   & df$party_name=="Chrisitan Democracy / New PSI"]<-0 #moved onto a new alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Italy"   & df$party_name=="Party of the Italian Communists"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Italy"  ])]<-NA

df$collapse.vote.tp1[df$year==1955 & df$country_name=="Japan"   & df$party_name=="Japan Liberal Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1955 & df$country_name=="Japan"   & df$party_name=="Japan Democratic Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1955 & df$country_name=="Japan"   & df$party_name=="Left Wing Socialist Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1955 & df$country_name=="Japan"   & df$party_name=="Right Wing Socialist Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Japan"   & df$party_name=="Japan Renewal Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Japan"   & df$party_name=="Japan New Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Japan"   & df$party_name=="Democratic Socialist Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Japan"  ])]<-NA

df$collapse.vote.tp1[df$year==1995 & df$country_name=="Latvia"   & df$party_name=="Latvian National Independence Movement / Latvian Green Party"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==1995 & df$country_name=="Latvia"   & df$party_name=="For Fatherland and Freedom"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1998 & df$country_name=="Latvia"   & df$party_name=="Latvia's Union of Social Democrats"]<-0 # renamed party
df$collapse.vote.tp1[df$year==1998 & df$country_name=="Latvia"   & df$party_name=="Farmers Union of Latvia"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2002 & df$country_name=="Latvia"   & df$party_name=="Latvia's First Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2002 & df$country_name=="Latvia"   & df$party_name=="Latvian Way"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2002 & df$country_name=="Latvia"   & df$party_name=="Social Democratic Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Latvia"   & df$party_name=="Latvian First Party / Latvian Way Party"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2006 & df$country_name=="Latvia"   & df$party_name=="People's Party"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2010 & df$country_name=="Latvia"   & df$party_name=="For a Good Latvia"]<-0 #desolved alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Latvia"  ])]<-NA

df$collapse.vote.tp1[df$year==1990 & df$country_name=="Lithuania"   & df$party_name=="Lithuanian Christian Democrats"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1990 & df$country_name=="Lithuania"   & df$party_name=="Lithuanian Democratic Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1992 & df$country_name=="Lithuania"   & df$party_name=="Lithuanian Christian Democrats / Union of Lithuanian Political Prisoners and Deportees / Lithuanian Democratic Party"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==1996 & df$country_name=="Lithuania"   & df$party_name=="Young Lithuania"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1996 & df$country_name=="Lithuania"   & df$party_name=="Democratic Labour Party of Lithuania"]<-0 #moved to an alliance
df$collapse.vote.tp1[df$year==1996 & df$country_name=="Lithuania"   & df$party_name=="Lithuanian Russian Union"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1996 & df$country_name=="Lithuania"   & df$party_name=="Lithuanian Social Democratic Party"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2000 & df$country_name=="Lithuania"   & df$party_name=="Brazauskas Social Democratic Coalition"]<-0 #desolved alliance
df$collapse.vote.tp1[df$year==2000 & df$country_name=="Lithuania"   & df$party_name=="Moderate Conservative Union"]<-0 #name change
df$collapse.vote.tp1[df$year==2004 & df$country_name=="Lithuania"   & df$party_name=="Homeland Union"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2004 & df$country_name=="Lithuania"   & df$party_name=="Lithuanian Christian Democrats"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2004 & df$country_name=="Lithuania"   & df$party_name=="Peasant Party / New Democratic Party"]<-0 #name change
df$collapse.vote.tp1[df$year==2004 & df$country_name=="Lithuania"   & df$party_name=="Working for Lithuania"]<-0 #desolved alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Lithuania"  ])]<-NA

df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Luxembourg"  ])]<-NA

df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Malta"  ])]<-NA

df$collapse.vote.tp1[df$year==1972 & df$country_name=="Netherlands"   & df$party_name=="Anti-Revolutionary Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1972 & df$country_name=="Netherlands"   & df$party_name=="Catholic Peoples Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1972 & df$country_name=="Netherlands"   & df$party_name=="Christian Historical Union"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1986 & df$country_name=="Netherlands"   & df$party_name=="Radical Political Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1986 & df$country_name=="Netherlands"   & df$party_name=="Pacifist Socialist Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1998 & df$country_name=="Netherlands"   & df$party_name=="Reformatory Political Federation"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1998 & df$country_name=="Netherlands"   & df$party_name=="Reformed Political League"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Netherlands"  ])]<-NA

df$collapse.vote.tp1[df$year==1990 & df$country_name=="New Zealand"   & df$party_name=="Social Credit / Democratic Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1990 & df$country_name=="New Zealand"   & df$party_name=="New Labour Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1990 & df$country_name=="New Zealand"   & df$party_name=="Green Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1999 & df$country_name=="New Zealand"   & df$party_name=="United New Zealand"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="New Zealand"  ])]<-NA

df$collapse.vote.tp1[df$year==2005 & df$country_name=="Norway"   & df$party_name=="Red Electoral Alliance"]<-0 #party renaming
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Norway"  ])]<-NA

df$collapse.vote.tp1[df$year==1991 & df$country_name=="Poland"    & df$party_name=="Christian National Union"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Poland"    & df$party_name=="Democratic Union"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Poland"    & df$party_name=="Liberal Democratic Congress"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Poland"    & df$party_name=="Centre Agreement"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==1993 & df$country_name=="Poland"    & df$party_name=="Solidarnosc"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2001 & df$country_name=="Poland"    & df$party_name=="Democratic Left Alliance / Labor Union"]<-0 #Desolved alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Poland"  ])]<-NA

df$collapse.vote.tp1[df$year==1980 & df$country_name=="Portugal"    & df$party_name=="Republican and Socialist Front"]<-0 #Desolved alliance
df$collapse.vote.tp1[df$year==1980 & df$country_name=="Portugal"    & df$party_name=="Democratic Alliance"]<-0 #Desolved Alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Portugal"  ])]<-NA

df$collapse.vote.tp1[df$year==1992 & df$country_name=="Romania"    & df$party_name=="Democratic National Salvation Front"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1992 & df$country_name=="Romania"    & df$party_name=="Republican Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2004 & df$country_name=="Romania"    & df$party_name=="National Union PSD+PUR"]<-0 #Desolved alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Romania"  ])]<-NA

df$collapse.vote.tp1[df$year==1992 & df$country_name=="Slovakia"    & df$party_name=="Social Democratic Party of Slovakia"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1992 & df$country_name=="Slovakia"    & df$party_name=="Party of the Democratic Left"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Slovakia"    & df$party_name=="Common Choice"]<-0 #alliance desolved
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Slovakia"    & df$party_name=="Democratic Party"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Slovakia"    & df$party_name=="Christian Democratic Movement"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==1994 & df$country_name=="Slovakia"    & df$party_name=="Democratic Union"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$year==2002 & df$country_name=="Slovakia"    & df$party_name=="Slovak Democratic and Christian Union"]<-0 #moved onto an alliance
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Slovakia"  ])]<-NA

df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Slovenia"   ])]<-NA

df$collapse.vote.tp1[df$year==2008 & df$country_name=="Spain"     & df$party_name=="Navarre Yes"]<-0 #Desolved coalition
df$collapse.vote.tp1[df$year==2008 & df$country_name=="Spain"     & df$party_name=="Basque Solidarity"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Spain"  ])]<-NA


df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Sweden"  ])]<-NA

df$collapse.vote.tp1[df$year==2007 & df$country_name=="Switzerland"     & df$party_name=="Radical Democratic Party"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$year==2007 & df$country_name=="Switzerland"     & df$party_name=="Liberal Party of Switzerland"]<-0 #moved onto a merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="Switzerland"  ])]<-NA

df$collapse.vote.tp1[df$year==1966 & df$country_name=="United Kingdom"     & df$party_name=="Conservatives and National Liberals"]<-0 #Alliance desolved
df$collapse.vote.tp1[df$year==1987 & df$country_name=="United Kingdom"     & df$party_name=="SDP-Liberal Alliance"]<-0 #name change from alliance to merger
df$collapse.vote.tp1[df$elect.uid==max(df$elect.uid[df$country_name=="United Kingdom"  ])]<-NA


#descriptives
count(df$collapse.vote.tm1)
count(df$collapse.vote.tp1)

#location of collapsed party 

df$cv.location.tm1<-ifelse(df$collapse.vote.tm1==1,df$left_right_extreme, NA) #Only values for collapsed parties are used, so calculations of the mean are not distorted. 
df$cv.location.tp1<-ifelse(df$collapse.vote.tp1==1,df$left_right_extreme, NA) #Only values for collapsed parties are used, so calculations of the mean are not distorted. 
df$np.location<-ifelse(df$NP==1,df$left_right_extreme, NA) #Only values for NP are used, so calculations of the mean are not distorted. 

### Adjustment of Seat/Voteshare so there are only values for the collapsed party in the t-1. Previously all parties had values 
df$vote.share.tm1[df$collapse.vote.tm1!=1]<-0 ## Number so the obs don't drop out, by definition no party can have a result in t-1 if they are collapsed
df$seat.share.tm1[df$collapse.seat!=1]<-0 ## Number so the obs don't drop out, by definition no party can have a result in t-1 if they are collapsed

#vote share for collapsed parties in t (useful for models using tp1)
df$vote.share.t<-ifelse(df$collapse.vote.tp1==1,df$vote_share, 0) #zero so the obs don't drop out, avoiding selection bias


# Level of party crash
df$s.crash.tm1<-ifelse(df$collapse.vote.tm1==1,df$s.vote.change.tm1, NA)
df$s.crash.tp1<-ifelse(df$collapse.vote.tp1==1,df$s.vote.change.tp1, NA)


##########################################
### Descriptive of largest collapsed party
##########################################

big.crash<-df[df$collapse.vote.tp1==1 & df$vote.share.t>20 & df$year>=1945, ]
big.crash<-big.crash[, c("country_name", "election_date", "party_name", "vote.share.t", "s.vote.change.tp1")]
big.crash<-big.crash[complete.cases(big.crash),]
big.crash<- big.crash[!big.crash$country_name=="Cyprus", ]
big.crash<- big.crash[!big.crash$country_name=="German Democratic Republic", ]
big.crash<-as.data.frame(big.crash)
big.crash <- big.crash[order(-rank(big.crash$vote.share.t), big.crash$country_name, big.crash$election_date),]
row.names(big.crash) <- NULL
#big.crash.table<- xtable(big.crash)

#print(big.crash.table, type="latex", file=paste0(bd, "big_crash_table.tex"), floating=FALSE)

############################
#### Collapsing df
################################
#library(doBy)
# Dummies and number of collapsed parties by vote and seat, as well as vote share
options(warn=-1)
party.data <- ddply(df, c("elect.uid","country_name", "year", "election_date"), summarise, 
              n.parties = length(party_name),
              p.crash.tm1=max(collapse.vote.tm1,na.rm=T),
              p.crash.tp1=max(collapse.vote.tp1,na.rm=T),
              s.crash.tm1=ifelse(is.finite(mean(s.crash.tm1,na.rm=T)),mean(s.crash.tm1,na.rm=T), 0), #zero included to avoid selection bias
              s.crash.tp1=ifelse(is.finite(mean(s.crash.tp1,na.rm=T)),mean(s.crash.tp1,na.rm=T), 0), #zero included to avoid selection bias
              c.location.mean.tm1=ifelse(is.finite(mean(cv.location.tm1,na.rm=T)),mean(cv.location.tm1,na.rm=T), 0), #zero included to avoid selection bias
              c.location.max.tm1=ifelse(is.finite(max(cv.location.tm1,na.rm=T)),max(cv.location.tm1,na.rm=T), 0), #zero included to avoid selection bias
              c.location.min.tm1=ifelse(is.finite(min(cv.location.tm1,na.rm=T)),min(cv.location.tm1,na.rm=T), 0), #zero included to avoid selection bias
              c.location.mean.tp1=ifelse(is.finite(mean(cv.location.tp1,na.rm=T)), mean(cv.location.tp1,na.rm=T), 0), #zero included to avoid selection bias
              c.location.max.tp1=ifelse(is.finite(max(cv.location.tp1,na.rm=T)), max(cv.location.tp1,na.rm=T), 0), #zero included to avoid selection bias
              c.location.min.tp1=ifelse(is.finite(min(cv.location.tp1,na.rm=T)), min(cv.location.tp1,na.rm=T), 0), #zero included to avoid selection bias
              share.v.crash.tm1=sum(vote.share.tm1, na.rm=T),
              share.v.crash.tp1=sum(vote.share.t,na.rm=T),
              np.vote=sum(np.vote,na.rm=T),
              NP.d=max(NP,na.rm=T),
              S.NP.d=max(S.NP,na.rm=T),
              NP.n=sum(NP,na.rm=T),
              S.NP.n=sum(S.NP,na.rm=T),
              np.location.min=ifelse(is.finite(min(np.location,na.rm=T)), min(np.location,na.rm=T), 0),
              np.location.max=ifelse(is.finite(max(np.location,na.rm=T)), max(np.location,na.rm=T), 0),
              np.location.mean=ifelse(is.finite(mean(np.location,na.rm=T)), mean(np.location,na.rm=T), 0)
       )




#Number of parties
# party.data<-merge(x = party.data1, y = polar.s, by = c("country_name" , "year", "elect.uid"),all.x = T)
# party.data<-merge(x = party.data, y = polar.m, by = c("country_name" , "year", "elect.uid"))
rm(o.party.df)

#adjusting NP data to omite the first year
party.data <- transform(party.data, elect.id=ave(rep(0,length(country_name)), country_name,
                                     FUN=seq_along))
                  
party.data$NP.n[party.data$elect.id==1]<-NA
party.data$np.vote[party.data$elect.id==1]<-NA
party.data$NP.d[party.data$elect.id==1]<-NA
party.data$S.NP.d[party.data$elect.id==1]<-NA
party.data$S.NP.n[party.data$elect.id==1]<-NA
#party.data$np.location.max[party.data$elect.id==1]<-NA
#party.data$np.location.mean[party.data$elect.id==1]<-NA

party.data$elect.id<-NULL # eliminate election id so it won't alter empirical analysis                        


#Adjusting duplications
party.data$uid<-paste0(party.data$country_name, party.data$year)
party.data$elect2<-duplicated(party.data$uid)
party.data$uid[party.data$elect2==T]<-paste0(party.data$uid[party.data$elect2==T], ".", 5)

party.data<-rename(party.data, c("country_name"="country"))
party.data$elect2<-NULL



write.csv(file="EU OECD/party_data.csv", x=party.data)

#check<-party.data[, c("country", "year", "vote_share" , "NP", "S.NP", "np.vote" )]
#View(check)
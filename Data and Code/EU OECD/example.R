# EXAMPLE OF PARLGOV USAGE
#
# Demonstration of linking ParlGov data tables to create a data set
# Documentation of tables provided at www.parlgov.rg
#
# A very simple study of government formation based on data from the
# ParlGov project. The goal is to study government formation from 1975
# to 1985 by making use of Castles/Mair data.  In the example, we want
# to add Castles/Mair left-right positions to the ParlGov table on
# government formation. We have to link these tables with the help of
# the 'view_party' table, which combines party ids from different data
# sets about political parties.
#
# TABLES
#
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


# SCRIPT FOR R (tested under Windows and Linux only)

# make sure current directory is working directory

# reading ParlGov data tables described above mac/linux may require:
# <- read.csv(file(".csv", encoding = "latin1"), as.is=TRUE)
parties <- read.csv("view_party.csv", as.is=TRUE)
cabinets <- read.csv("view_cabinet.csv", as.is=TRUE)
castles <- read.csv("external_party_castles_mair.csv", as.is=TRUE)

# remove ParlGov left/right values
cabinets$left_right <- NULL

# merge Castles/Mair positions to cabinet data
castles <- merge(castles, parties[,c('castles_mair', 'party_id')],
                 by.x='id', by.y='castles_mair', all.x=TRUE)
cabinets <- merge(cabinets, castles[,c('party_id', 'left_right')],
                  by='party_id', all.x=TRUE)

# limit observations to period 1975--1985 and parties with
# Castles/Mair positions only
cabinets <- cabinets[cabinets$start_date >= '1975-01-01'
                     & cabinets$start_date < '1986-01-01' , ]
cabinets <- cabinets[!is.na(cabinets$left_right) , ]

# create observations for seats share and distance from center
cabinets$seats_share <- (cabinets$seats / cabinets$election_seats_total) * 100
cabinets$left_right_extreme <- abs(cabinets$left_right - 5)

# run a toy model
glm(formula = cabinet_party ~ seats_share + left_right_extreme,
    data=cabinets, family = binomial(link = "logit"))


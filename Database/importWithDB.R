
# Workflow:
# 1. Collected data: student demographics, high school grade, AAU grades, dropout status, 
##   SSP questionnaire answers, and course performance. 
##   The latter two is excluded from this analysis due to unknown dropout effect of cohort 2017.
# 2. Merges the views in R 
# 3. Prepare a dataset for prediction analysis
# 4. Automate the prediction analysis using a list of possible predictors (LA.tbl_predictors)
# 5. Automate the process of sending the prediction results to the database.
# 6. Automate the process of retrieving the data for the prototype (Bastian)

# Here:
# filter and merge the queiries to make it fit with the import and aggregation script (hendrik started)



#########################
##### Library setup #####
#########################

# clean up these things?
library(xlsx)
library(sqldf)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(MASS)
library(manipulate)
library(lubridate)
library(base)
library(stringr)
library(corrplot)
library(Amelia)
library(gsheet)
library(splines)
library(car)
library(sandwich)
library(RcmdrMisc)

library(RJDBC) # for database connection


###############################
##### Import STADS data #####
###############################

# Example description of the database setup:
#the script assumes that you have a config.R file in your libloc folder that holds the STADSuser and STADSpass strings. 
#STADSuser<-"AAU$HK" 
#STADSpass<-"somePaSSWORD" #not shown here
#print 'libloc' in the console for the config file dir

###############################
#Config file and connection

#Bianca / Windows
source(paste(libloc,"\\config.R",sep=''))
jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="C:/Program Files/oracle/ojdbc6.jar")
jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora-stads-stdb.srv.aau.dk:1521/STAPSB", STADSuser, STADSpass)

###
#Hendrik / MAC
#source(paste(libloc,"//config.R",sep=''))
#jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="/Applications/oracle/ojdbc6.jar")
#jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ora-stads-stdb.srv.aau.dk:1521/STAPSB", STADSuser, STADSpass)

###############################
#Import xlsx table from Ole (for comparison)

#tabletest<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stamdata_hdk')

###############################
#Import views from STADS (suggested by Birthe)

######
# Enrolment data

#description of uddannelsesramme
beg_uddramme<-dbGetQuery(jdbcConnection,'select * from vueudv.beg_uddramme') # used

#relation between person and 'uddannelsesramme' (i.e. concrete study programme for this enrolment) 
rag_stud_uddramm<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_stud_uddramm') # used

#description of educations and studyboards
##aau_uddannelses_udtraek<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_uddannelses_udtraek') 

#description of study plans
##beg_studieordn <-dbGetQuery(jdbcConnection,'select * from vueudv.beg_studieordn')
#note: cant find id in this to match rag_stud_uddramm (not fra_studieramme_id)

#relation between person education elements (UDD, EKA, UVA, GPR, STU)
##rag_stud_uddelem<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_stud_uddelem')
#note: doesnt seem relevant

#####
# dropout reasons

#relation between person and dropout reason
rag_udmeld<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_udmeld') 

#####
#test

#tig_ek_tilmed<-dbGetQuery(jdbcConnection,'select * from vueudv.tig_ek_tilmed')
vyg_periode<-dbGetQuery(jdbcConnection,'select * from vueudv.vyg_periode')
vyg_admenhed<-dbGetQuery(jdbcConnection,'select * from vueudv.vyg_admenhed')


######
# Data before AAU, e.g. high school grades 

#description of zip code
aau_postnr_hdk<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_postnr_hdk') 

# adgangsgivende institution(STADS) you will have to lookup the zip code outside STADS 
#(The ’KODE’ is the national code related to the institution)
VYS_ADGANGSGIV_INST<-dbGetQuery(jdbcConnection,'select * from vueudv.VYS_ADGANGSGIV_INST') 

#fra optagelsessystemet
OPG_ADGEX<-dbGetQuery(jdbcConnection,'select * from vueudv.OPG_ADGEX') 

OPS_GYMFAG<-dbGetQuery(jdbcConnection,'select * from vueudv.OPS_GYMFAG') 

OPS_GYMFAG_NIVEAU<-dbGetQuery(jdbcConnection,'select * from vueudv.OPS_GYMFAG_NIVEAU') 

#person_id related to ’adgangseksamen’, ’resultat’ from institution/SKOLE
RAG_ADGANG<-dbGetQuery(jdbcConnection,'select * from vueudv.RAG_ADGANG') 


######
# AAU data (campus, activities, grades, merit)

#description of campus
aau_stud_campus_v<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stud_campus_v') 
#seems like we are missing campus data on dropout students

#relation imellem studieordning og aktivitet og administrativ enhed (beregnet ECTS) 
aau_stu_akt<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stu_akt')
# weird that uddelement_ID in this view is different from beg_uddelement, tested with 57144

#name of all education activities 
beg_uddelement<-dbGetQuery(jdbcConnection,'select * from vueudv.beg_uddelement') # used

#relation between person and AAU grade
reg_karakter<-dbGetQuery(jdbcConnection,'select * from vueudv.reg_karakter')

# aau grades, merit grades and STAA
aau_stud_uddelement_person_id<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stud_uddelement_person_id') 
##aarsvaerk: ECTS converted to “STÅ” (60 ECTS=1 STÅ) the students achieved in the program either by passing exams or transferring credits from other programs (from AAU or other universities)
##staa_aarsvaerk: ECTS converted to “STÅ” the students passed entirely while enrolled in the program (i.e. Medialogy)

#description of merit, do we need this????
#rag_merit<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_merit') 

#relation between person and exam activity and ECTS
#reg_resultat<-dbGetQuery(jdbcConnection,'select * from vueudv.reg_resultat') 
# note: empty due to no administration right, but not needed

#description of exam activities  
##beg_eksamensakt <-dbGetQuery(jdbcConnection,'select * from vueudv.beg_eksamensakt')
#note: doesn't seem relevant

#description of exam activities  
##beg_eksamination<-dbGetQuery(jdbcConnection,'select * from vueudv.beg_eksamination')
# doesnt seem important

#description of exam activities  
#bes_uddelement <-dbGetQuery(jdbcConnection,'select * from vueudv.bes_uddelement')
# have no access to this table but also not needed

#description of exam activities  
##beg_bedoemform <-dbGetQuery(jdbcConnection,'select * from vueudv.beg_bedoemform')
#note: irrelevant


###############################
##### Preparing the data #####
###############################

#select STUDIERETNING that is Medialogy
detach("package:RMySQL", unload=TRUE)



# STUDIERETNING name in 

# creating long tables!!

# last thing
# beg_uddramme_MED<-beg_uddramme[beg_uddramme$STUDIERETNING=='Medialogi' ,] # change this 
# #note: cannot use aau_uddannelses_udtraek, as it is missing students
# #aau_uddannelses_udtraek_MED<-aau_uddannelses_udtraek[aau_uddannelses_udtraek$STUDIERETNING=='Medialogi' ,]
# 
# #rag_stud_uddramm: enrollment data
# rag_stud_uddramm_MED <- rag_stud_uddramm[rag_stud_uddramm$UDDRAMME_ID %in% beg_uddramme_MED$UDDRAMME_ID,]
# 
# #rag_stud_uddelem: karakter and aarsvaerk
# rag_stud_uddramm_MED1 <- merge(rag_stud_uddramm_MED, rag_stud_uddelem, by="STUD_UDDRAMME_ID", all.x=T)
# count(data.frame(unique(rag_stud_uddramm_MED1$STUD_UDDRAMME_ID)))
# 
# #beg_uddelement: base activity
# rag_stud_uddramm_MED1 <- merge(rag_stud_uddramm_MED1, beg_uddelement, by="UDDELEMENT_ID", all.x=T)
# 
# rag_stud_uddramm_MED2 <- rag_stud_uddramm_MED1[c(2,5:7,9,13:16,19:21,25:28,32, 36)]




beg_uddramme_MED<-beg_uddramme[beg_uddramme$STUDIERETNING=='Medialogi' ,] # change this 
#note: cannot use aau_uddannelses_udtraek, as it is missing students
#aau_uddannelses_udtraek_MED<-aau_uddannelses_udtraek[aau_uddannelses_udtraek$STUDIERETNING=='Medialogi' ,]

#selects the bachelor 
beg_uddramme_MED<- subset(beg_uddramme_MED, grepl('Bachelor', beg_uddramme_MED$UDDANNELSES_NAVN))
#aau_uddannelses_udtraek_MED<- subset(aau_uddannelses_udtraek_MED, aau_uddannelses_udtraek_MED$UDDANNELSESTYPE == "bachelor")

#checks that UDDRAMME_ID is unique/distinct
x <-sqldf('SELECT DISTINCT UDDRAMME_ID FROM beg_uddramme')
ifelse(count(beg_uddramme) == count(x), 'UDDRAMME_ID is unique', 'UDDRAMME_ID is not unique')

#selects the medialogy BSc from the enrolment data
rag_stud_uddramm_MED <- rag_stud_uddramm[rag_stud_uddramm$UDDRAMME_ID %in% beg_uddramme_MED$UDDRAMME_ID,]
count(rag_stud_uddramm_MED) # 3650 students

#add known variables
rag_stud_uddramm_MED$ramme_retning <- 'Medialogy'
rag_stud_uddramm_MED$stype <- 'BSc'

#add enrolment year
rag_stud_uddramm_MED$fradatosn = as.Date(as.character(rag_stud_uddramm_MED$FRA_DATO), "%Y-%m-%d")
rag_stud_uddramm_MED$startaar <- as.numeric(format(rag_stud_uddramm_MED$fradatosn,'%Y'))

# check number of students over the years
table(rag_stud_uddramm_MED$startaar)

#add campus
#rag_stud_uddramm_MED1 <- merge(rag_stud_uddramm_MED, aau_stud_campus_v, id.var='PERSON_ID', all.x=T)
#count(rag_stud_uddramm_MED1) # 3651 students

# check number of students at campus location over the years
#table(rag_stud_uddramm_MED1$CAMPUS, rag_stud_uddramm_MED1$startaar)


###############################
# dropout status

#remove the double student entries for dropouts
rag_udmeld1 <- rag_udmeld[rag_udmeld$UDMELD_STATUS != 'annulleret' ,]

#add udmeldstatus
rag_stud_uddramm_MED <- merge(rag_stud_uddramm_MED, rag_udmeld1, id.var='STUD_UDDRAMME_ID', all.x=T)
count(rag_stud_uddramm_MED) # 3650 students

###############################
# AAU grades
# study plan (curriculum) - created manually
# TODO: need SPV for 2017!


testing <- sqldf('select distinct studieretn_ID, studieretning, aktgrp_id, from beg_studieordn where instr(studieretning, "Medialog") > 0 ')
count(testing)

mergetest <- merge(beg_studieordn, beg_uddramme)

# test
test <- merge(rag_stud_uddramm, aau_stud_uddelement_person_id, id.var='STUD_UDDRAMME_ID', all.x=T)


###############################
# general student data?
# high school grades?
# merit grades?
# rammeskift?
# aggregated grades

# three views:
# enrolment
# before AAU
# AAU grades, ects, dates
# AAU aggregated grades (medialogy)
# at 




# Workflow:
# 1. Collected data: student demographics, high school grade, AAU grades, dropout status, 
##   SSP questionnaire answers, and course performance. 
##   The latter two is excluded from this analysis due to unknown dropout effect of cohort 2017.
# 2. Merges the views in R 
# 3. Prepare a dataset for prediction analysis
# 4. Automate the prediction analysis using a list of possible predictors (LA.tbl_predictors)
# 5. Automate the process of sending the prediction results to the database.
# 6. Automate the process of retrieving the data for the prototype


###############################
# This script merges STADS views data to three overall tables:
# 1) enrolment and dropout
# 2) before AAU
# 3) AAU grades, ects, dates

# And one specific table for the LA database:
# 1) Aggregated grades for medialogy students


#########################
##### Library setup #####
#########################

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
library(compare)
library(RcmdrMisc)

library(RJDBC) # for database connection


###############################
##### Import STADS data #####
###############################

#Example description of the database setup:
##the script assumes that you have a config.R file in your libloc folder that holds the STADSuser and STADSpass strings. 
##STADSuser<-"AAU$HK" 
##STADSpass<-"somePaSSWORD" #not shown here
##print 'libloc' in the console for the config file dir

###############################
#Config file and connection

libloc= Sys.getenv("R_LIBS_USER")

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
### Import views from STADS ###
###############################

#Import xlsx table from Ole (for comparison)
#tabletest<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stamdata_hdk')

###############################
# Enrolment and dropout

#description of uddannelsesramme
beg_uddramme<-dbGetQuery(jdbcConnection,'select * from vueudv.beg_uddramme') # used

#relation between person and 'uddannelsesramme' (i.e. concrete study programme for this enrolment) 
rag_stud_uddramm<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_stud_uddramm') # used

# dropout reasons
rag_udmeld<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_udmeld') 

###############################
# Data before AAU, e.g. high school grades 

#description of zip code, test
# adgangsgivende institution(STADS) you will have to lookup the zip code outside STADS 
#(The ’KODE’ is the national code related to the institution)
#VYS_ADGANGSGIV_INST<-dbGetQuery(jdbcConnection,'select * from vueudv.VYS_ADGANGSGIV_INST') 

#fra optagelsessystemet
#OPG_ADGEX<-dbGetQuery(jdbcConnection,'select * from vueudv.OPG_ADGEX') 

#OPS_GYMFAG<-dbGetQuery(jdbcConnection,'select * from vueudv.OPS_GYMFAG') 

#OPS_GYMFAG_NIVEAU<-dbGetQuery(jdbcConnection,'select * from vueudv.OPS_GYMFAG_NIVEAU') 

#person_id related to ’adgangseksamen’, ’resultat’ from institution/SKOLE
#RAG_ADGANG<-dbGetQuery(jdbcConnection,'select * from vueudv.RAG_ADGANG') 

###############################
# AAU data (campus, activities, grades, merit)

#description of campus
#aau_stud_campus_v<-dbGetQuery(jdbcConnection,'select * from vueudv.aau_stud_campus_v') 
#seems like we are missing campus data on dropout students

# students and 'sted'
#rag_stu_sted <-dbGetQuery(jdbcConnection,'select * from vueudv.rag_stu_sted') 
#nrow(rag_stu_sted) #44905, seems like we still are missing campus data on students?

# campus data description
#vyg_sted <-dbGetQuery(jdbcConnection,'select * from vueudv.vyg_sted') 

#name of all education activities 
beg_uddelement<-dbGetQuery(jdbcConnection,'select * from vueudv.beg_uddelement') # used

#relation between person and AAU grade
reg_karakter<-dbGetQuery(jdbcConnection,'select * from vueudv.reg_karakter')
#We should use this view instead of the grade vectors

#relation between person and AAU grade
ret_resultat<-dbGetQuery(jdbcConnection,'select person_id, aktivitet_id, bedoemmelsesdato from vueudv.ret_resultat')
#ret_resultatCOLUMS<-dbGetQuery(jdbcConnection,'select * from vueudv.ret_resultat OFFSET 4 ROWS FETCH NEXT 4 ROWS ONLY')

# aau grades, merit grades and STAA
rag_stud_uddelem<-dbGetQuery(jdbcConnection,'select * from vueudv.rag_stud_uddelem') # used
##aarsvaerk: ECTS converted to “STÅ” (60 ECTS=1 STÅ) the students achieved in the program either by passing exams or transferring credits from other programs (from AAU or other universities)
##staa_aarsvaerk: ECTS converted to “STÅ” the students passed entirely while enrolled in the program (i.e. Medialogy)


#########################################
# Import data table from LA database 
library(RMySQL)
libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep=''))

mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver); dbSendQuery(mydb,"SET NAMES utf8")
#dbGetQuery(mydb,"show variables like 'character_set_%'")
rs<-dbSendQuery(mydb, "SELECT * FROM map_SPVCmapping")
dfECTSstruct<- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

dbDisconnect(mydb)

# solves encoding problem
Encoding(dfECTSstruct$aktivitet) <- "UTF-8"

###############################
##### Preparing the data #####
###############################

detach("package:RMySQL", unload=TRUE)

#########################################
# prep enrolment and dropout data

# not using aau_uddannelses_udtraek, as we loose some data that might be important
# enrolmentMerge <- sqldf('select rag_stud_uddramm.*, 
#                     aau_uddannelses_udtraek.STUDIERETNING, aau_uddannelses_udtraek.UDDANNELSESTYPE
#                     from rag_stud_uddramm
#                     inner join aau_uddannelses_udtraek
#                     on rag_stud_uddramm.UDDRAMME_ID = aau_uddannelses_udtraek.UDDRAMME_ID') 
# nrow(enrolmentMerge) # 162512

enrolmentMerge <- sqldf('select rag_stud_uddramm.*,
                    beg_uddramme.STUDIERETN_ID, beg_uddramme.STUDIERETNING, beg_uddramme.INDGANGSHJEMMEL, 
                    beg_uddramme.UDDANNELSES_ID, beg_uddramme.UDDANNELSES_NAVN, beg_uddramme.PRIORITET
                    from rag_stud_uddramm
                    inner join beg_uddramme
                    on rag_stud_uddramm.UDDRAMME_ID = beg_uddramme.UDDRAMME_ID')
nrow(enrolmentMerge) # 249876

#create 'stype' to label bsc, msc, or NA/unknown education types
#not sure that this is meaningful..
enrolmentMerge$stype <- ifelse( grepl('Bachelor', enrolmentMerge$UDDANNELSES_NAVN), 'bsc', 
                          ifelse(grepl('Kandidat', enrolmentMerge$UDDANNELSES_NAVN) | 
                                  grepl('Cand', enrolmentMerge$UDDANNELSES_NAVN) | 
                                  grepl('Master', enrolmentMerge$UDDANNELSES_NAVN),'msc', NA ))
count(subset(enrolmentMerge,is.na(enrolmentMerge$stype))) # 115086 / 249876

enrolmentMerge$fradatosn = as.Date(as.character(enrolmentMerge$FRA_DATO), "%Y-%m-%d")
enrolmentMerge$slutdatosn = as.Date(as.character(enrolmentMerge$TIL_DATO), "%Y-%m-%d")
enrolmentMerge$enrolID <- enrolmentMerge$STUD_UDDRAMME_ID


#########################################
# prep merging with dropout data

#check number of unique STUD_UDDRAMME_ID in two data frames
count(data.frame(unique(enrolmentMerge$STUD_UDDRAMME_ID ))) # 249912
count(data.frame(unique(rag_udmeld$STUD_UDDRAMME_ID ))) # 226989

#order data by STUD_UDDRAMME_ID
#rag_udmeld <- rag_udmeld[order(rag_udmeld$STUD_UDDRAMME_ID), ]

#add dropout date and reason
enrolmentMerge <- merge( enrolmentMerge, rag_udmeld, by="STUD_UDDRAMME_ID", all.x=T)
nrow(enrolmentMerge) # 261013, too many

# remove duplicates
enrolmentMerge<-enrolmentMerge[ !duplicated(enrolmentMerge$STUD_UDDRAMME_ID), ]
nrow(enrolmentMerge) # 249912, original number of rows
count(data.frame(unique(enrolmentMerge$STUD_UDDRAMME_ID ))) # 249912, original number of unique IDs

# prep campus...
# enrolmentMerge1 <- merge( enrolmentMerge, rag_stu_sted, by="STUD_UDDRAMME_ID", all.x=T)
# nrow(enrolmentMerge1) # 250856
# count(data.frame(unique(enrolmentMerge1$STUD_UDDRAMME_ID ))) #249912
# enrolmentMerge1<-enrolmentMerge1[ !duplicated(enrolmentMerge1$STUD_UDDRAMME_ID), ]
# nrow(enrolmentMerge1) # 249912
# count(subset(enrolmentMerge1,is.na(enrolmentMerge1$STED_ID))) # 223326 / 249912 # original: 215770 / 249873
# count(subset(enrolmentMerge1,enrolmentMerge1$STATUS == "åben" )) # 23271 / 249873

# data set : enrolmentMerge
# primary key : STUD_UDDRAMME_ID 
# Medialogy Bsc : select stype 'bsc' and STUDIERETNING 'Medialogi'

##############################
# prep high school grades

nrow(enrolmentMerge) # 249876
nrow(RAG_ADGANG) # 170470
count(data.frame(unique(enrolmentMerge$STUD_UDDRAMME_ID ))) # 249876
count(data.frame(unique(enrolmentMerge$PERSON_ID ))) # 145491
count(data.frame(unique(RAG_ADGANG$PERSON_ID ))) # 140920

enrolmentMergeHS <- merge(enrolmentMerge, RAG_ADGANG, by="PERSON_ID" , all.x=T)

count(data.frame(unique(enrolmentMergeHS$STUD_UDDRAMME_ID ))) # 249876
count(data.frame(unique(enrolmentMergeHS$PERSON_ID ))) # 145491

# remove duplicates
enrolmentMergeHS<-enrolmentMergeHS[ !duplicated(enrolmentMergeHS$STUD_UDDRAMME_ID), ]
nrow(enrolmentMergeHS) # 249876


##############################
# prep AAU grades and merit

# select only entries where knudetype is 'EKA'
rag_stud_uddelem_GRADES <- rag_stud_uddelem[rag_stud_uddelem$KNUDETYPE == 'EKA' , ]

# merge to get person_id
rag_stud_uddelem_GRADES <- sqldf('select rag_stud_uddelem_GRADES.*,
                        rag_stud_uddramm.person_id
                        from rag_stud_uddelem_GRADES
                        left join rag_stud_uddramm
                        on rag_stud_uddelem_GRADES.stud_uddramme_id = rag_stud_uddramm.stud_uddramme_id')

# merge views to get 'bedoemmelsesdata', by person_ID and uddelement_id
rag_stud_uddelem_GRADES <- merge(rag_stud_uddelem_GRADES, ret_resultat, 
                                 by.x=c('UDDELEMENT_ID','PERSON_ID'),
                                 by.y=c('AKTIVITET_ID','PERSON_ID'),
                                 all.x=T)

# remove duplicates
nrow(rag_stud_uddelem_GRADES) # 2601713
count(data.frame(unique(rag_stud_uddelem_GRADES$STUD_UDDELEM_ID))) # 2395634
rag_stud_uddelem_GRADES<-rag_stud_uddelem_GRADES[ !duplicated(rag_stud_uddelem_GRADES$STUD_UDDELEM_ID) , ]
nrow(rag_stud_uddelem_GRADES) # 2395634

# add activity name
AAUgradesMerge <- sqldf('select rag_stud_uddelem_GRADES.*,
                        beg_uddelement.DATO, beg_uddelement.NAVN
                        from rag_stud_uddelem_GRADES
                        inner join beg_uddelement
                        on rag_stud_uddelem_GRADES.UDDELEMENT_ID = beg_uddelement.UDDELEMENT_ID')
nrow(AAUgradesMerge) #2395634

# add variable for the exam evaluation date
AAUgradesMerge$bedom_dato <- as.Date(as.character(AAUgradesMerge$BEDOEMMELSESDATO) , "%Y-%m-%d")

# vectors for converting grades
gradesPassedLUVec<-c('02'=1,'2'=1,'4'=1,'7'=1,'10'=1,'12'=1,'00'=0,'0'=0,'-3'=0,'B'=2,'EB'=0,'U'=0,'I'=0)
gradesToNumsVec<-c('02'=2,'2'=2,'4'=4,'7'=7,'10'=10,'12'=12,'00'=0,'0'=0,'-3'=-3,'EB'=-5,'U'=-8,'B'=2,'I'=0)

AAUgradesMerge$ECTS <- AAUgradesMerge$AARSVAERK * 60
  

# see the medialogy dataset in importWithDBv2_MED.R

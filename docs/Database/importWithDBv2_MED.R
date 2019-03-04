
# import a script, but I run it manually, as it doesn't work on my computer
#source('importWithDBv2.R')





##############################
# prep Medialogy study plan (curriculum) - created manually
# TODO: need SPV for 2017?

# subset Medialogy Bsc

#subset enrolment data with: stype 'bsc' and STUDIERETNING 'Medialogi'
enrolmentMerge_MED <- subset(enrolmentMergeHS, enrolmentMergeHS$stype == "bsc" & grepl('Medialogi', enrolmentMergeHS$STUDIERETNING ))
nrow(enrolmentMerge_MED) # 3650
count(data.frame(unique(enrolmentMerge_MED$STUD_UDDRAMME_ID ))) # 3650

#merge by stud_uddramme_id for enrolment data and AAU grades
AAUgradesMerge_MED <- merge(enrolmentMerge_MED, AAUgradesMerge, by="STUD_UDDRAMME_ID", all.x=T)
nrow(AAUgradesMerge_MED) # 47474
count(data.frame(unique(AAUgradesMerge_MED$STUD_UDDRAMME_ID ))) # 3650
count(data.frame(unique(AAUgradesMerge_MED$PERSON_ID.x ))) # 3341

#converting the grades to numbers. 
#Moved here, as I get memory problems when running this before subsetting Medialogy.. 
AAUgradesMerge_MED$gradeNum<-gradesToNumsVec[as.character(AAUgradesMerge_MED$KARAKTER)]
AAUgradesMerge_MED$GPAgrade<-gradesToNumsVec[as.character(AAUgradesMerge_MED$KARAKTER)]
AAUgradesMerge_MED$isPassed<-gradesPassedLUVec[as.character(AAUgradesMerge_MED$KARAKTER)]

#### compare 'aktivitet' and 'navn' columns to find missing strings in SPV, only for the bachelor 
dfECTSstruct1 <- subset(dfECTSstruct, dfECTSstruct$type =="bachelor")
testDiff_inStruct <- data.frame(setdiff(dfECTSstruct1$aktivitet,AAUgradesMerge_MED$NAVN) )
nrow(testDiff_inStruct) # 0, if many something is very wrong with the string comparison

AAUgradesMerge_MED1 <- subset(AAUgradesMerge_MED, AAUgradesMerge_MED$AAR >= 2010)
nrow(AAUgradesMerge_MED1) #36617
testDiff_inGrades <- data.frame(setdiff(AAUgradesMerge_MED1$NAVN, dfECTSstruct1$aktivitet)  )
nrow(testDiff_inGrades) # 27

# can we add 'bedoemmelsesdato'?
table(AAUgradesMerge_MED$NAVN, AAUgradesMerge_MED$NAVN %in% dfECTSstruct1$aktivitet)

#### setup ECTS structure of the studyplans ----------------------------------------------------

dfECTSstruct$CourseSPVID<-seq(1:nrow(dfECTSstruct))
dfECTSstruct$CTdf<-as.factor(dfECTSstruct$CTdf)
dfECTSstruct$aktivitetText<-as.character(dfECTSstruct$aktivitet)
dfECTSstruct$fromDate<-as.Date(dfECTSstruct$fromDate,"%d/%m/%Y")
dfECTSstruct$toDate<-as.Date(dfECTSstruct$toDate,"%d/%m/%Y")

ectsSumSPbySemAndCT<-sqldf("select SPV, sem, CTdf, aktivitet, sum(nominalECTS) as nominalECTS from dfECTSstruct group by SPV, sem, CTdf, aktivitet  order by SPV, courseType, sem ")
semStruct<-sqldf("select distinct SPV, sem from dfECTSstruct")
CTdfs<-sqldf("select distinct CTdf from dfECTSstruct")
semStruct<-merge(semStruct,CTdfs)

# left join (all rows in x, adding matching columns from y)
sem<-join(semStruct,ectsSumSPbySemAndCT,by=c("SPV","sem","CTdf"), type="left")
sem$nominalECTS[is.na(sem$nominalECTS)]<-0
# alias: semStruct as a, sem as b
ectsSumSPbySemAndCT<-sqldf('select a.SPV as SPV, 
                           a.sem as sem, a.CTdf as CTdf, round(sum(b.nominalECTS+0),0) as nominalECTS
                           from semStruct as a, sem as b
                           where a.SPV=b.SPV and 
                           b.sem<=a.sem and 
                           a.CTdf=b.CTdf 
                           group by a.SPV, a.sem , a.CTdf')

#ectsSumSPbySemAndCTx<-ddply(ectsSumSPbySemAndCT,.(SPV,CTdf),transform,nominalECTS=cumsum(nominalECTS))
# gsub: substitude string 
ectsSumSPbySemAndCT$CTdf<-gsub("project","Pr",ectsSumSPbySemAndCT$CTdf)
ectsSumSPbySemAndCT$CTdf<-gsub("elective","El",ectsSumSPbySemAndCT$CTdf)
ectsSumSPbySemAndCT$CTdf<-factor(ectsSumSPbySemAndCT$CTdf, levels = c("T", "NT", "El","Pr"))
# paste string with a seperator
ectsSumSPbySemAndCT$CTdf<-paste(ectsSumSPbySemAndCT$CTdf, "nom", sep = "_")


dfAAUGradesWODistEnrol<-AAUgradesMerge_MED
dfAAUGradesWODistEnrol$DistFromEnrol<- NULL
# alias: dfAAUGradesWODistEnrol as a, dfECTSstruct as b, dfEnrolStatus as c
# dfAAUMarriedGrades<-sqldf('select distinct b.CTdf as bctdf, b.SPV as SPV, b.gradeType as gradeType, b.courseSPVID,b.fromDate, c.fradatosn as fradatoSNsemCalc, b.toDate, c.enrolID as enrolID, a.* 
#                           from dfAAUGradesWODistEnrol as a, dfECTSstruct as b, dfEnrolStatus as c 
#                           where a.type=c.type and c.studienr=a.studienr and 
#                           a.aktivitetText=b.aktivitetText and 
#                           c.fradatosn>= b.fromDate and 
#                           c.fradatosn<=b.toDate and 
#                           a.bedom_dato>=c.fradatosn and 
#                           (a.bedom_dato<= c.slutdatosn or c.slutdatosn is Null)')


dfEnrolStatus <- enrolmentMerge_MED
dfEnrolStatus$type <- ifelse(dfEnrolStatus$stype == "bsc", "bachelor", "kandidat")

dfAAUMarriedGrades<-sqldf('select distinct b.CTdf as bctdf, b.SPV as SPV, b.gradeType as gradeType, b.courseSPVID, b.fromDate, 
                          c.fradatosn as fradatoSNsemCalc, b.toDate, c.enrolID as enrolID, a.* 
                          from dfAAUGradesWODistEnrol as a, dfECTSstruct as b, dfEnrolStatus as c 
                          where a.stype=c.stype and c.STUD_UDDRAMME_ID=a.STUD_UDDRAMME_ID and 
                          a.NAVN=b.aktivitetText and 
                          c.fradatosn>= b.fromDate and 
                          c.fradatosn<=b.toDate and 
                          (a.bedom_dato<= c.slutdatosn or c.slutdatosn is Null)')

# the two variables are also in the import script but with different values for the grades
gradesToGPANumsVec<-c('2'=2,'02'=2,'4'=4,'7'=7,'10'=10,'12'=12,'0'=0,'00'=0,'-3'=-3,'EB'=-4,'U'=-5)
gradesToGPAPFVec<-c('EB'=-4,'U'=-5,'B'=2,'I'=0)

# TODO: I don't understand the meaning of this variable?? The name sounds like it calculates the semester date, but it doesn't
# the date when enrolled into the education (POSIXct represents calendar dates and times)
dfAAUMarriedGrades$fradatoSNsemCalc<-as.Date(format(as.POSIXct(as.numeric(dfAAUMarriedGrades$fradatoSNsemCalc*3600*24), origin='1970-01-01') , format="%Y-%m-%d"))

# converting PF and scale grades to numbers
dfAAUMarriedGrades$GPAgrade<-ifelse(dfAAUMarriedGrades$gradeType=="PF",gradesToGPAPFVec[as.character(dfAAUMarriedGrades$KARAKTER)],gradesToGPANumsVec[as.character(dfAAUMarriedGrades$KARAKTER)])
dfAAUMarriedGrades$rid<-seq(1:nrow(dfAAUMarriedGrades))


# TODO: need to check this again most likely this is about people taking exams in master that should be taken in BSc and therefore added as e.g. sem 7
# (exam year - year of current semester) * 2 + ((exam month - 14) - 2) / 7
dfAAUMarriedGrades$takenInSem<- (as.numeric(format(dfAAUMarriedGrades$bedom_dato-14,'%Y'))-
                                   as.numeric(format(dfAAUMarriedGrades$fradatoSNsemCalc,'%Y')))*2+ 
  floor((as.numeric(format(dfAAUMarriedGrades$bedom_dato-14,'%m'))-2)/7)

# selects the SPV and enrolID where the SVP fromDate is before enrolment date and SVP toDate is after enrolment
dfSPVSNR<-sqldf('select distinct b.SPV as SPV, c.STUD_UDDRAMME_ID as STUD_UDDRAMME_ID from dfECTSstruct as b, 
                dfEnrolStatus as c where b.type=c.type and c.fradatosn>= b.fromDate and c.fradatosn<=b.toDate ')

# error
testdfAAUGrades<-data.frame(AAUgradesMerge[,c("DistFromEnrol")]) # TODO: error


# ------ variables are not used later in the analysis


# nominal etcs points for each semester and each SPV
ectsSumSPbySPVAndCT<-dcast(ectsSumSPbySemAndCT, SPV~sem+CTdf)

#creating the summaries of achieved ECTS per semester 
#semester, bctdf, ECTSpassed, 
#create the structure for all
#create semesters and edutypes
semesters<-data.frame(seq(1:10));names(semesters)<-c("semester"); eduType<-data.frame(c("bachelor","kandidat"));names(eduType)<-"type"
#multiply semesters with edutypes
semSkeleton<-data.frame(merge(CTdfs,semesters));semSkeleton<-merge(semSkeleton,eduType)
#used SemEduTYpe multiplication to merge with students Enrolments into Edutypes

StudieNRSkeleton<-merge(distinct(dfEnrolStatus[,c("STUD_UDDRAMME_ID","type")]),semSkeleton,by.x =c("type"),by.y = c("type"))


# attained ECTS in a study activity of type bctdf (i.e. PR, T, NT, EL) for each STUD_UDDRAMME_ID
ECTSattainedMelted<- merge(dfAAUMarriedGrades[dfAAUMarriedGrades$isPassed>0,
                                              c("bctdf", "STUD_UDDRAMME_ID","takenInSem", "ECTS")], 
                           StudieNRSkeleton,by.x = c("STUD_UDDRAMME_ID","bctdf"),
                           by.y =c("STUD_UDDRAMME_ID","CTdf") ,all.y=TRUE)
# highest possible ETCS in a study activity of type bctdf (i.e. PR, T, NT, EL) for each STUD_UDDRAMME_ID

maxECTSTakenByCourseBySemester<-sqldf('select bctdf, STUD_UDDRAMME_ID, takenInSem, NAVN, stype, max(ECTS) as ECTS 
                                      from dfAAUMarriedGrades group by bctdf, STUD_UDDRAMME_ID, takenInSem, NAVN, stype')

# attempted ECTs in a study activity of type bctdf (i.e. PR, T, NT, EL) for each STUD_UDDRAMME_ID
ECTSattmptedMelted<- merge(maxECTSTakenByCourseBySemester[,c("bctdf", "STUD_UDDRAMME_ID","takenInSem", "ECTS")] , 
                           StudieNRSkeleton,by.x = c("STUD_UDDRAMME_ID","bctdf"),by.y =c("STUD_UDDRAMME_ID","CTdf") ,all.y=TRUE)

# prepare and clean up of attained and attempted ETCS variables
ECTSattainedMelted$what<-"atnBy"
ECTSattmptedMelted$what<-"atpIn"
ECTSattainedMelted[is.na(ECTSattainedMelted$ECTS),]$ECTS<-0
ECTSattmptedMelted[is.na(ECTSattmptedMelted$ECTS),]$ECTS<-0
# NAs in takenInSem is replaced with the semester value.
ECTSattainedMelted[is.na(ECTSattainedMelted$takenInSem),]$takenInSem<-ECTSattainedMelted[is.na(ECTSattainedMelted$takenInSem),]$semester
ECTSattmptedMelted[is.na(ECTSattmptedMelted$takenInSem),]$takenInSem<-ECTSattmptedMelted[is.na(ECTSattmptedMelted$takenInSem),]$semester

# keep the attained entries where takenInSem <= semester
ECTSattainedMelted<-ECTSattainedMelted[ECTSattainedMelted$takenInSem<=ECTSattainedMelted$semester,]
# keep the attempted entries where takenInSem == semester
ECTSattmptedMelted<-ECTSattmptedMelted[ECTSattmptedMelted$takenInSem==ECTSattmptedMelted$semester,]
ECTSattmptedMelted$takenInSem<-NULL
ECTSattainedMelted$takenInSem<-NULL

# combines attained and attempted ECTS and simplifies data
ECTSperf<-rbind(ECTSattmptedMelted,ECTSattainedMelted)
ECTSperf$bctdf<-gsub("project","Pr",ECTSperf$bctdf);ECTSperf$bctdf<-gsub("elective","El",ECTSperf$bctdf);
ECTSperf$bctdf<-factor(ECTSperf$bctdf, levels = c("T", "NT", "El","Pr"))
ECTSattainedMelted$bctdf<-gsub("project","Pr",ECTSattainedMelted$bctdf);ECTSattainedMelted$bctdf<-gsub("elective","El",ECTSattainedMelted$bctdf);
ECTSattainedMelted$bctdf<-factor(ECTSattainedMelted$bctdf, levels = c("T", "NT", "El","Pr"))



# reshapes and merges the data
ECTSovw<- dcast(ECTSperf,STUD_UDDRAMME_ID~semester+bctdf+what,value.var = "ECTS",sum)
ECTSovw<-merge(ECTSovw,dfSPVSNR,by="STUD_UDDRAMME_ID") 

# preparing GPAavg data
GPAavg<- merge(dfAAUMarriedGrades[,c("bctdf", "STUD_UDDRAMME_ID","takenInSem", "GPAgrade")] , 
               StudieNRSkeleton,by.x = c("STUD_UDDRAMME_ID","bctdf"),by.y =c("STUD_UDDRAMME_ID","CTdf") ,all.y=TRUE)
#GPAavg<- dfAAUMarriedGrades[!is.na(dfAAUMarriedGrades$GPAgrade),c("bctdf", "STUD_UDDRAMME_ID","takenInSem", "GPAgrade","semester")]
GPAavg<-GPAavg[order(GPAavg$STUD_UDDRAMME_ID, GPAavg$bctdf, GPAavg$semester,GPAavg$takenInSem),]
# keep data where takenInSem <= semester or NAs
GPAavg<-GPAavg[GPAavg$takenInSem<=GPAavg$semester|is.na(GPAavg$takenInSem),]

# preparing aggregations of GPAavg
GPAavgagg<-sqldf("select STUD_UDDRAMME_ID, bctdf, semester, avg(GPAgrade) as GPAavg from GPAavg group by 
                 STUD_UDDRAMME_ID, bctdf, semester")
GPAavgagg$bctdf<-gsub("project","Pr",GPAavgagg$bctdf);GPAavgagg$bctdf<-gsub("elective","El",GPAavgagg$bctdf);
GPAavgagg$bctdf<-paste(GPAavgagg$bctdf,"mGPA",sep = "_")
# reshapes data
GPAavgagg<-dcast(GPAavgagg,STUD_UDDRAMME_ID~semester+bctdf,value.var = "GPAavg",sum)

#need to verify we have all the students in ECTSovw TODO
ectsAggsAll<-merge(ECTSovw,ectsSumSPbySPVAndCT, by="SPV") 
count(ectsAggsAll)  # 3505


dfSTADSdata<-merge(dfEnrolStatus,ectsAggsAll, by="STUD_UDDRAMME_ID", all.x=T) 
dfSTADSdata<-merge(dfSTADSdata,GPAavgagg) 

count(data.frame(unique(dfSTADSdata$STUD_UDDRAMME_ID ))) # 3650
count(data.frame(unique(dfSTADSdata$PERSON_ID ))) # 3341
# it looks like we have all enrolments and all students


#########################################
# Check that finished students have attained enough ECTS points

# add a column of attained all nominal ECTS
dfSTADSdata$attained_all_El <- ifelse( dfSTADSdata$`10_El_atnBy` >=  dfSTADSdata$`6_El_nom`, 1,0)
dfSTADSdata$attained_all_T <- ifelse( dfSTADSdata$`10_T_atnBy` >=  dfSTADSdata$`6_T_nom`, 1,0)
dfSTADSdata$attained_all_NT <- ifelse( dfSTADSdata$`10_NT_atnBy` >=  dfSTADSdata$`6_NT_nom`, 1,0)
dfSTADSdata$attained_all_Pr <- ifelse( dfSTADSdata$`10_Pr_atnBy` >=  dfSTADSdata$`6_Pr_nom`, 1,0)

nrow(dfSTADSdata) # 3650
count(subset(dfSTADSdata, dfSTADSdata$attained_all_El==1)) # 2549, EL nom: 0
count(subset(dfSTADSdata, dfSTADSdata$attained_all_T==1)) # 1, T nom: 45
count(subset(dfSTADSdata, dfSTADSdata$attained_all_NT==1)) # 1, NT nom: 4
count(subset(dfSTADSdata, dfSTADSdata$attained_all_Pr==1)) # 419, Pr nom: 90

count(subset(dfSTADSdata, dfSTADSdata$STATUS == "afsluttet")) # 1132
count(subset(dfSTADSdata, dfSTADSdata$STATUS == "afsluttet" & dfSTADSdata$SPV == "BSc")) # 448
count(subset(dfSTADSdata, dfSTADSdata$STATUS == "afsluttet" & dfSTADSdata$SPV == "BSc2010")) # 481
count(subset(dfSTADSdata, dfSTADSdata$STATUS == "afsluttet" & dfSTADSdata$SPV == "BSc2014")) # 203

table(dfSTADSdata$STATUS, dfSTADSdata$AAR)
table(dfSTADSdata$SPV)


#########################################
# Prediction model


#########################################
# Write data table to LA database 

library(RMySQL)
libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep=''))

mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver);dbSendQuery(mydb,"SET NAMES utf8")

rs<-dbSendQuery(mydb, "SELECT * FROM map_SPVCmapping")
dfECTSstruct<- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)

###############################
##### Preparing the data #####
###############################

detach("package:RMySQL", unload=TRUE)


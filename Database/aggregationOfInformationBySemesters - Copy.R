

# import a script, but I run it manually, as it doesn't work on my computer
source('importDataAndgetInShape - Copy.R')



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
ectsSumSPbySemAndCT$CTdf<-gsub("project","Pr",ectsSumSPbySemAndCT$CTdf);
ectsSumSPbySemAndCT$CTdf<-gsub("elective","El",ectsSumSPbySemAndCT$CTdf);
ectsSumSPbySemAndCT$CTdf<-factor(ectsSumSPbySemAndCT$CTdf, levels = c("T", "NT", "El","Pr"))
# paste string with a seperator
ectsSumSPbySemAndCT$CTdf<-paste(ectsSumSPbySemAndCT$CTdf, "nom", sep = "_")

dfAAUGradesWODistEnrol<-dfAAUGrades;
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

# for 2017 cohort
# dfAAUMarriedGrades<-sqldf('select distinct b.CTdf as bctdf, b.SPV as SPV, b.gradeType as gradeType, b.courseSPVID,b.fromDate, c.fradatosn as fradatoSNsemCalc, b.toDate, c.enrolID as enrolID, a.* 
#                           from dfAAUGradesWODistEnrol as a, dfECTSstruct as b, dfEnrolStatus as c 
#                           where a.type=c.type and c.studienr=a.studienr and 
#                           a.aktivitetText=b.aktivitetText and 
#                           c.fradatosn>= b.fromDate and 
#                           c.fradatosn<=b.toDate and 
#                           (a.bedom_dato<= c.slutdatosn or c.slutdatosn is Null)')

# for all cohorts
# dfAAUMarriedGrades<-sqldf('select distinct b.CTdf as bctdf, b.SPV as SPV, b.gradeType as gradeType, b.courseSPVID,b.fromDate, b.toDate, a.* 
#                           from dfAAUGradesWODistEnrol as a, dfECTSstruct as b 
#                           a.aktivitetText=b.aktivitetText


# the two variables are also in the import script but with different values for the grades
gradesToGPANumsVec<-c('2'=2,'02'=2,'4'=4,'7'=7,'10'=10,'12'=12,'0'=0,'00'=0,'-3'=-3,'EB'=-4,'U'=-5)
gradesToGPAPFVec<-c('EB'=-4,'U'=-5,'B'=2,'I'=0)
# TODO: I don't understand the meaning of this variable?? The name sounds like it calculates the semester date, but it doesn't
# the date when enrolled into the education (POSIXct represents calendar dates and times)
dfAAUMarriedGrades$fradatoSNsemCalc<-as.Date(format(as.POSIXct(as.numeric(dfAAUMarriedGrades$fradatoSNsemCalc*3600*24), origin='1970-01-01') , format="%Y-%m-%d"))

# converting PF and scale grades to numbers
dfAAUMarriedGrades$GPAgrade<-ifelse(dfAAUMarriedGrades$gradeType=="PF",gradesToGPAPFVec[as.character(dfAAUMarriedGrades$KARAKTER)],gradesToGPANumsVec[as.character(dfAAUMarriedGrades$KARAKTER)])
dfAAUMarriedGrades$rid<-seq(1:nrow(dfAAUMarriedGrades))


# HKTODO the below overwrites the data already prepared in getin shape --- / BC: e.g. changed an entry of studienr: 20102814
# need to check this again most likely this is about people taking exams in master that should be taken in BSc and therefore added as e.g. sem 7
# (exam year - year of current semester) * 2 + ((exam month - 14) - 2) / 7
dfAAUMarriedGrades$takenInSem<- (as.numeric(format(dfAAUMarriedGrades$bedom_dato-14,'%Y'))-as.numeric(format(dfAAUMarriedGrades$fradatoSNsemCalc,'%Y')))*2+ 
                              floor((as.numeric(format(dfAAUMarriedGrades$bedom_dato-14,'%m'))-2)/7)
                             
# selects the SPV and enrolID where the SVP fromDate is before enrolment date and SVP toDate is after enrolment
dfSPVSNR<-sqldf('select distinct b.SPV as SPV, c.enrolID as enrolID from dfECTSstruct as b, dfEnrolStatus as c where b.type=c.type and c.fradatosn>= b.fromDate and c.fradatosn<=b.toDate ')
testdfAAUGrades<-data.frame(dfAAUGrades[,c("DistFromEnrol")])


# ------ finding missing rows and activities

missingRows<- dfAAUGradesWODistEnrol[!dfAAUGradesWODistEnrol$rowID %in% dfAAUMarriedGrades$rowID ,]
missingActivities<-data.frame(missingRows[!missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ]$aktivitetText)
missingRows1<-data.frame(missingRows[missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ])
missActivitetBefore2010<-sqldf("select reg_dato, aktivitet, fradatosn, startaar from missingRows where startaar<=2009") 
missingRows1<-missingRows1[!missingRows1$startaar %in% missActivitetBefore2010$startaar, ]

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
StudieNRSkeleton<-merge(distinct(dfEnrolStatus[,c("enrolID","type")]),semSkeleton,by.x =c("type"),by.y = c("type")  )

# attained ECTS in a study activity of type bctdf (i.e. PR, T, NT, EL) for each enrolID
ECTSattainedMelted<- merge(dfAAUMarriedGrades[dfAAUMarriedGrades$isPassed>0,
                          c("bctdf", "enrolID","takenInSem", "ECTS")], 
                          StudieNRSkeleton,by.x = c("enrolID","bctdf"),
                          by.y =c("enrolID","CTdf") ,all.y=TRUE)
# highest possible ETCS in a study activity of type bctdf (i.e. PR, T, NT, EL) for each enrolID
maxECTSTakenByCourseBySemester<-sqldf('select bctdf, enrolID, takenInSem, aktivitet, type, max(ECTS) as ECTS 
                                      from dfAAUMarriedGrades group by bctdf, enrolID, takenInSem, aktivitet, type')

# attempted ECTs in a study activity of type bctdf (i.e. PR, T, NT, EL) for each enrolID
ECTSattmptedMelted<- merge(maxECTSTakenByCourseBySemester[,c("bctdf", "enrolID","takenInSem", "ECTS")] , 
                           StudieNRSkeleton,by.x = c("enrolID","bctdf"),by.y =c("enrolID","CTdf") ,all.y=TRUE)

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
ECTSovw<- dcast(ECTSperf,enrolID~semester+bctdf+what,value.var = "ECTS",sum)
ECTSovw<-merge(ECTSovw,dfSPVSNR,by="enrolID")

# preparing GPAavg data
GPAavg<- merge(dfAAUMarriedGrades[,c("bctdf", "enrolID","takenInSem", "GPAgrade")] , StudieNRSkeleton,by.x = c("enrolID","bctdf"),by.y =c("enrolID","CTdf") ,all.y=TRUE)
#GPAavg<- dfAAUMarriedGrades[!is.na(dfAAUMarriedGrades$GPAgrade),c("bctdf", "enrolID","takenInSem", "GPAgrade","semester")]
GPAavg<-GPAavg[order(GPAavg$enrolID, GPAavg$bctdf, GPAavg$semester,GPAavg$takenInSem),]
# keep data where takenInSem <= semester or NAs
GPAavg<-GPAavg[GPAavg$takenInSem<=GPAavg$semester|is.na(GPAavg$takenInSem),]

# preparing aggregations of GPAavg
GPAavgagg<-sqldf("select enrolID, bctdf, semester, avg(GPAgrade) as GPAavg from GPAavg group by enrolID, bctdf, semester")
GPAavgagg$bctdf<-gsub("project","Pr",GPAavgagg$bctdf);GPAavgagg$bctdf<-gsub("elective","El",GPAavgagg$bctdf);
GPAavgagg$bctdf<-paste(GPAavgagg$bctdf,"mGPA",sep = "_")
# reshapes data
GPAavgagg<-dcast(GPAavgagg,enrolID~semester+bctdf,value.var = "GPAavg",sum)

#need to verify we have all the students in ECTSovw TODO
ectsAggsAll<-merge(ECTSovw,ectsSumSPbySPVAndCT, by="SPV")
count(ectsAggsAll)

#From2010s<-dfEnrolStatus[dfEnrolStatus$startaar>2009,]
ForSvante<-merge(dfEnrolStatus,dfEntryGradesAll) # 2012-2014: 1686 of 41 var
#Used in the 2012-2014 import.R: ForSvante<-merge(ForSvante, highSchoolData1, all=T) # 2879 of 46 var
ForSvante<-merge(ForSvante, ectsAggsAll) # 2012-2014: 1690 of 151 var
ForSvante<-ForSvante[ForSvante$startaar>2011,] # 2012-2014: 1415 of 151 var ()
ForSvante<-merge(ForSvante,GPAavgagg) # 2012-2014: 1411 of 191 var
#sqldf("select studienr, spv, count(studienr) from ForSvante group by studienr,SPV having count(studienr)>1 order by studienr")
# BC: remove merit duplicates (check why they are here)
ForSvante <- ForSvante[!is.na(ForSvante$CourseLocation),]

setwd('Z:/BNC/PBL development project/data/analysis_data/dropOut/data_2017cohortCPHAAL')
write.csv(ForSvante,file = "MedDataBSc2017_1107NEW2.csv",row.names=FALSE) 

# 2012-2014
#setwd('Z:/BNC/PBL development project/data/analysis_data/dropOut/data')
#write.csv(ForSvante,file = "MedDataBSc2012-2016.csv",row.names=FALSE) 

###########################
## AAU grades cohort 2017

dfAAUMarriedGradesWide <- sqldf("SELECT studienr, navn, statussn, udmeldsn, aktivitet, gradeNum FROM dfAAUMarriedGrades")
dfAAUMarriedGradesWide <- dfAAUMarriedGradesWide[dfAAUMarriedGradesWide$studienr !=20157596, ] 
dfAAUMarriedGradesWide <- reshape(dfAAUMarriedGradesWide, idvar = c("studienr","navn", "statussn", "udmeldsn"), timevar = c("aktivitet"), direction = "wide")
dfAAUMarriedGradesWide <- dfAAUMarriedGradesWide[, -c(14:15)]
#dfAAUMarriedGradesWide <- dfAAUMarriedGradesWide[-nrow(dfAAUMarriedGradesWide),]
names(dfAAUMarriedGradesWide) <- c("studienr","navn", "statussn", "udmeldsn", "p1", "p0", "AVS", "GPRO", "PV", "P2", "ID", "MMA", "PFI")

setwd('Z:/BNC/PBL development project/data/analysis_data/dropOut/data_2017cohortCPHAAL')
write.csv(dfAAUMarriedGradesWide,file = "MedDataBSc2017_grades.csv",row.names=FALSE) 

personSTADSid<-read.csv("2008-18_ram_op_all.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)


##################################################################
#### Write dataset to database
##################################################################
#MedDataBSc2017 <- read.csv("MedDataBSc2017.csv", header = TRUE, sep = ",", encoding="utf8") 

libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"/config.R",sep=''))

library(RMySQL)
mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver);dbSendQuery(mydb,"SET NAMES utf8")
#  fileEncoding = "UTF-8", overwrite = T,
#dbWriteTable(mydb, value = MedDataBSc2017, name = "tbl_ForSvante2", temporary = TRUE, row.names=FALSE)

# tbl_personIDs


personIDs <- dfEnrolStatusBsc[,c(2,3)]
personIDs <- subset(personIDs,  personIDs$studienr %in% uniqueDropMedsFrAll$studienr)
personIDs <- merge(personIDs, uniqueDropMedsFrAll,by="studienr")
personIDs$idNumber <- NA
personIDs$map_personIDsID <- seq(1:nrow(personIDs))
write.csv(personIDs,file = "personIDs.csv",row.names=FALSE) 
#dbWriteTable(mydb, value = personIDs, name = "tbl_personIDs", temporary = TRUE, row.names=FALSE, append=TRUE)


dbDisconnect(mydb)
detach("package:RMySQL", unload=TRUE)



##################################################################
#### HK and BC stopped here - OLD code below
##################################################################


#dfKvote1<-sqldf('select distinct land from dfKvote ')
#dfM1<-sqldf('select distinct studienr, MAT_Niveau, MATGrade, ENG_Niveau, ENGGrade, DAN_Niveau, DANGrade from dfM ')
#KvoteHSGrades<-merge(dfKvote1,dfM1) # need to remove studienr duplicates
#ForSvante2<-merge(ForSvante,KvoteHSGrades, by = "studienr")

#highSchoolData1<-sqldf('select fullname as navn, gender, ADGGRU, zip, residenceBeforeEnrolment, ageAtEnrolment from highSchoolData')
#highSchoolVariables<-unique(merge(dfKvote1,dfM1, by = "studienr"))

#ForSvante3<-merge(highSchoolVariables,ForSvante2)
#ForSvante3<-merge(highSchoolData1,ForSvante3,by = c("navn"))
#ForSvante3<-ForSvante3[ForSvante3$stype=="bachelor",]

perc.rank <- function(x) trunc(rank(x))/length(x)
MScstudentsGPAavgs<-sqldf("select type, enrolID, sum(ECTS/5*GPAgrade)/(sum(ECTS)/5) as GPAavg, sum(ECTS) as ECTS from dfAAUMarriedGrades where GPAgrade is not Null and type='kandidat'  group by type, enrolID having sum(ECTS)>=80")
MScstudentsGPAavgs$rank<-perc.rank(MScstudentsGPAavgs$GPAavg)

# final check of data ----------------------------------------------------
missingInSPV<-data.frame(unique(dfAAUGradesWODistEnrol[!dfAAUGradesWODistEnrol$aktivitet %in% dfECTSstruct$aktivitet,]$aktivitet ) )
missingInSPV['studentCounts'] <- 0
for (i in 1:nrow(missingInSPV)){
  missingInSPV[i,2] <- count(dfAAUGradesWODistEnrol$aktivitet, missingInSPV[i,1])
}


# add campus variable
dfAAUMarriedGradeFile <- dfAAUMarriedGrades
#dfAAUMarriedGradeFile['Campus'] <- NA
#dfAAUMarriedGradeFile <- merge(ForSvante2[,c(2,175)], dfAAUMarriedGradeFile, by="enrolID") # missing data..

#ideally the data frame is empty
# anonymize ---------------------------------------------------------------

ForSvante$studienr<-NULL
ForSvante2$studienr<-NULL
ForSvante3$studienr<-NULL
#ForSvante4$studienr<-NULL
ForSvante$navn<-NULL
ForSvante2$navn<-NULL
ForSvante3$navn<-NULL
#ForSvante4$navn<-NULL

dfAAUMarriedGradeFile$navn <- NULL
dfAAUMarriedGradeFile$studienr<-NULL

# write stuff out ---------------------------------------------------------


ForSvante<-ForSvante[!duplicated(ForSvante), ]
ForSvante2<-ForSvante2[!duplicated(ForSvante2), ]

write.csv(ForSvante,file = "MedDataBScMSc-MoreRowsFewerColumns.csv") # 1714 rows 167 variables
write.csv(ForSvante2,file = "MedData2.csv") # 1826 rows 145 variables # currently not used
write.csv(ForSvante3,file = "MedDataBSc.csv") # 1007 rows 184 variables 
#write.csv(ForSvante4,file = "MedData4.csv") # 1852 rows 139 variables
write.csv(dfAAUMarriedGradeFile,file = "dfAAUMarriedGrades.csv")

#studenCntKand<-sqldf("SELECT S.startaar, S.stype, C.cnt FROM ForSvante2 S INNER JOIN  (SELECT enrolID, count(enrolID) as cnt FROM ForSvante2 WHERE stype='kandidat' GROUP BY startaar ) C ON S.enrolID = C.enrolID  ")
#studenCntBach<-sqldf("SELECT S.startaar, S.stype, C.cnt FROM ForSvante2 S INNER JOIN  (SELECT enrolID, count(enrolID) as cnt FROM ForSvante2 WHERE stype='bachelor' GROUP BY startaar ) C ON S.enrolID = C.enrolID  ")
#the below is much faster than the above calls a
studentCnts<-sqldf("select startaar, stype, count(startaar) from ForSvante2 group by startaar, stype order by stype, startaar")

### Comment: Error in FUN(X[[i]], ...) : object 'type' not found
ECTSovwx <- dcast(ECTSattmptedMelted,type+studienr~semester+bctdf+what,value.var = "ECTS",sum)

# other aggregate approaches not finished ECTSatt <- ECTSattainedMelted %>%  group_by(Category) %>% 
#  summarise(Frequency = sum(Frequency))
#  sqldf("select studienr, type, bctdf, b.semester as bySem, ECTS  from dfAAUMarriedGrades, semSkeleton as b where b.semester>=takenInSem ")


#todo: sum up staa by enrolmentID and filter out people coming from elsewhere not qualifying for bachelor based on staa
# check  ectsatt to sort out students we shouldn't have e.g. coming from elsewhere
#add the nominal ECTS for that semester
#people that have more than 90 ECTS 
# e.g. in projects 20082897
#e.g. 20125436 is missing second sem proj in marriedGradesFrame

# get errors in the lines below
testxovw<-merge(ECTSovw[,c("studienr","type")],ECTSovwx[,c("studienr","type")],all.x=TRUE)
testyovw<-sqldf("select a.studienr, a.type, b.studienr, b.type from ECTSovw as a left outer join ECTSovwx as b using (studienr,type) ")


#sqldf("select studienr, fradatosn,strftime('%m', fradatosn) from dfEnrolStatus where strftime('%m', fradatosn)<>'09'")
#sqldf("SELECT strftime('%m','now')")


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
is.odd <- function(x) x %% 2 != 0 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
library(gsheet)
library(splines)
library(car)
library(sandwich)
#library(RcmdrMisc)

## some testing
################## COMMENT AGAIN
##hk comment
#hello 

dfECTSstruct<-gsheet2tbl('https://docs.google.com/spreadsheets/d/10xp3CLhDkgCG2p3M2f8GrGQ4j5kNaqrdJg1cFBAb7DQ/edit?usp=sharing')
#used to be  <-read.csv("course_SPV.csv", header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")

dfECTSstruct$CourseSPVID<-seq(1:nrow(dfECTSstruct))
dfECTSstruct$CTdf<-as.factor(dfECTSstruct$CTdf)
dfECTSstruct$aktivitetText<-as.character(dfECTSstruct$aktivitet)
dfECTSstruct$fromDate<-as.Date(dfECTSstruct$fromDate,"%d/%m/%Y")
dfECTSstruct$toDate<-as.Date(dfECTSstruct$toDate,"%d/%m/%Y")

ectsSumSPbySemAndCT<-sqldf("select SPV, sem, CTdf, aktivitet, sum(nominalECTS) as nominalECTS from dfECTSstruct group by SPV, sem, CTdf order by SPV, courseType, sem ")
semStruct<-sqldf("select distinct SPV, sem from dfECTSstruct")
CTdfs<-sqldf("select distinct CTdf from dfECTSstruct")
semStruct<-merge(semStruct,CTdfs)
sem<-join(semStruct,ectsSumSPbySemAndCT,by=c("SPV","sem","CTdf"), type="left")
sem$nominalECTS[is.na(sem$nominalECTS)]<-0
ectsSumSPbySemAndCT<-sqldf('select a.SPV as SPV, 
                           a.sem as sem, a.CTdf as CTdf, sum(b.nominalECTS+0) as nominalECTS
                           from semStruct as a, sem as b
                           where a.SPV=b.SPV and 
                           b.sem<=a.sem and 
                           a.CTdf=b.CTdf 
                           group by a.SPV, a.sem , a.CTdf')
#ectsSumSPbySemAndCT<-ddply(ectsSumSPbySemAndCT,.(SPV,CTdf),transform,nominalECTS=cumsum(nominalECTS))
dfAAUGradesWODistEnrol<-dfAAUGrades;
dfAAUGradesWODistEnrol$DistFromEnrol<- NULL
dfAAUMarriedGrades<-sqldf('select distinct b.CTdf as bctdf, b.courseSPVID, b.fromDate, c.fradatosn, b.toDate, a.* from dfAAUGradesWODistEnrol as a, dfECTSstruct as b, dfEnrolStatus as c where a.type=c.stype and c.studienr=a.studienr and a.aktivitetText=b.aktivitetText and c.fradatosn>= b.fromDate and c.fradatosn<=b.toDate ')
dfAAUMarriedGrades$takenInSem<-(as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%y'))-as.numeric(format(dfAAUMarriedGrades$fradatosn,'%y')))*2+ floor((as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%m'))-2)/6)
dfAAUMarriedGrades2<-merge(dfAAUGradesWODistEnrol,dfECTSstruct,by="aktivitet")
dfAAUMarriedGrades2<-dfAAUMarriedGrades2[dfAAUMarriedGrades2$fradatosn >= dfAAUMarriedGrades2$fromDate & dfAAUMarriedGrades2$fradatosn <=dfAAUMarriedGrades2$toDate,]
testdfAAUGrades<-data.frame(dfAAUGrades[,c("DistFromEnrol")])

missingRows<- dfAAUGradesWODistEnrol[!dfAAUGradesWODistEnrol$rowID %in% dfAAUMarriedGrades$rowID ,]
missingActivities<-data.frame(missingRows[!missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ]$aktivitetText)

missingRows1<-data.frame(missingRows[missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ])
missActivitetBefore2010<-sqldf("select reg_dato, aktivitet, fradatosn, startaar from missingRows where startaar<=2009") 
missingRows1<-missingRows1[!missingRows1$startaar %in% missActivitetBefore2010$startaar, ]

ectsSumSPbySemAndCT<-dcast(ectsSumSPbySemAndCT, SPV+sem~CTdf)
ectsSumSPbySemAndCT[is.na(ectsSumSPbySemAndCT$elective),]$elective<-0

#creating the summaries of achieved ECTS per semester 
#semester, bctdf, ECTSpassed, 
#create the structure for all
semesters<-data.frame(seq(1:10));names(semesters)<-c("semester"); eduType<-data.frame(c("bachelor","kandidat"));names(eduType)<-"type"
semSkeleton<-data.frame(merge(CTdfs,semesters));semSkeleton<-merge(semSkeleton,eduType)
StudieNRSkeleton<-merge(distinct(dfEnrolStatus[,c("studienr","stype")]),semSkeleton,by.x =c("stype"),by.y = c("type")  )


ECTSattainedMelted<- merge(dfAAUMarriedGrades[dfAAUMarriedGrades$isPassed==1,c("bctdf", "studienr","type","takenInSem", "ECTS")] , StudieNRSkeleton,by.x = c("studienr", "type","bctdf"),by.y =c("studienr", "stype","CTdf") ,all.y=TRUE)
ECTSattmptedMelted<- merge(dfAAUMarriedGrades[,c("bctdf", "studienr","type","takenInSem", "ECTS")] , StudieNRSkeleton,by.x = c("studienr", "type","bctdf"),by.y =c("studienr", "stype","CTdf") ,all.y=TRUE)

ECTSattainedMelted$what<-"atnBy"
ECTSattmptedMelted$what<-"atpIn"
ECTSattainedMelted[is.na(ECTSattainedMelted$ECTS),]$ECTS<-0
ECTSattmptedMelted[is.na(ECTSattmptedMelted$ECTS),]$ECTS<-0
ECTSattainedMelted[is.na(ECTSattainedMelted$takenInSem),]$takenInSem<-ECTSattainedMelted[is.na(ECTSattainedMelted$takenInSem),]$semester
ECTSattmptedMelted[is.na(ECTSattmptedMelted$takenInSem),]$takenInSem<-ECTSattmptedMelted[is.na(ECTSattmptedMelted$takenInSem),]$semester

ECTSattainedMelted<-ECTSattainedMelted[ECTSattainedMelted$takenInSem<=ECTSattainedMelted$semester,]
ECTSattmptedMelted<-ECTSattmptedMelted[ECTSattmptedMelted$takenInSem==ECTSattmptedMelted$semester,]
ECTSattmptedMelted$takenInSem<-NULL
ECTSattainedMelted$takenInSem<-NULL

ECTSperf<-rbind(ECTSattmptedMelted,ECTSattainedMelted)
ECTSperf$bctdf<-gsub("project","Pr",ECTSperf$bctdf);ECTSperf$bctdf<-gsub("elective","El",ECTSperf$bctdf);
ECTSperf$bctdf<-factor(ECTSperf$bctdf, levels = c("T", "NT", "El","Pr"))


ECTSattainedMelted$bctdf<-gsub("project","Pr",ECTSattainedMelted$bctdf);ECTSattainedMelted$bctdf<-gsub("elective","El",ECTSattainedMelted$bctdf);
ECTSattainedMelted$bctdf<-factor(ECTSattainedMelted$bctdf, levels = c("T", "NT", "El","Pr"))

ECTSatt<- dcast(ECTSattainedMelted,type+studienr~semester+bctdf,value.var = "ECTS",sum)
ECTSovw<- dcast(ECTSperf,type+studienr~semester+bctdf+what,value.var = "ECTS",sum)
ECTSovwx<- dcast(ECTSattmptedMelted,type+studienr~semester+bctdf+what,value.var = "ECTS",sum)

# other aggregate approaches not finished ECTSatt <- ECTSattainedMelted %>%  group_by(Category) %>% 
#  summarise(Frequency = sum(Frequency))
#  sqldf("select studienr, type, bctdf, b.semester as bySem, ECTS  from dfAAUMarriedGrades, semSkeleton as b where b.semester>=takenInSem ")


#todo: sum up staa by enrolmentID and filter out people coming from elsewhere not qualifying for bachelor based on staa
# check  ectsatt to sort out students we shouldn't have e.g. coming from elsewhere
#add the nominal ECTS for that semester
#people that have more than 90 ECTS 
# e.g. in projects 20082897
#e.g. 20125436 is missing second sem proj in marriedGradesFrame
# 
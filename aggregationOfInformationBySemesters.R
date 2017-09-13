
myWD<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/stats-on-grades'} else {"~/git/AAU/DropOutProject/analysis/"}
setwd(myWD)
source('importDataAndgetInShape.R')

# setup ECTS structure ----------------------------------------------------


#Setup the ECTS structure of the studyplans
dfECTSstruct$CourseSPVID<-seq(1:nrow(dfECTSstruct))
dfECTSstruct$CTdf<-as.factor(dfECTSstruct$CTdf)
dfECTSstruct$aktivitetText<-as.character(dfECTSstruct$aktivitet)
dfECTSstruct$fromDate<-as.Date(dfECTSstruct$fromDate,"%d/%m/%Y")
dfECTSstruct$toDate<-as.Date(dfECTSstruct$toDate,"%d/%m/%Y")

ectsSumSPbySemAndCT<-sqldf("select SPV, sem, CTdf, aktivitet, sum(nominalECTS) as nominalECTS from dfECTSstruct group by SPV, sem, CTdf, aktivitet  order by SPV, courseType, sem ")
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
ectsSumSPbySemAndCT$CTdf<-gsub("project","Pr",ectsSumSPbySemAndCT$CTdf);ectsSumSPbySemAndCT$CTdf<-gsub("elective","El",ectsSumSPbySemAndCT$CTdf);
ectsSumSPbySemAndCT$CTdf<-factor(ectsSumSPbySemAndCT$CTdf, levels = c("T", "NT", "El","Pr"))
ectsSumSPbySemAndCT$CTdf<-paste(ectsSumSPbySemAndCT$CTdf, "nom", sep = "_")



dfAAUGradesWODistEnrol<-dfAAUGrades;
dfAAUGradesWODistEnrol$DistFromEnrol<- NULL
dfAAUMarriedGrades<-sqldf('select distinct b.CTdf as bctdf, b.SPV as SPV, b.gradeType as gradeType, b.courseSPVID, 
                          b.fromDate, c.fradatosn as fradatoSNsemCalc, b.toDate, c.enrolID as enrolID, 
                          a.* from dfAAUGradesWODistEnrol as a, 
                          dfECTSstruct as b, 
                          dfEnrolStatus as c where a.type=c.stype and c.studienr=a.studienr 
                          and a.aktivitetText=b.aktivitetText 
                          and c.fradatosn>= b.fromDate 
                          and c.fradatosn<=b.toDate 
                          and a.bedom_dato>=c.fradatosn 
                          and (a.bedom_dato<= c.slutdatosn or c.slutdatosn is Null)')

gradesToGPANumsVec<-c('02'=2,'4'=4,'7'=7,'10'=10,'12'=12,'00'=0,'-3'=-3,'EB'=-4,'U'=-5)
gradesToGPAPFVec<-c('EB'=NA,'U'=NA,'B'=NA,'I'=NA)
dfAAUMarriedGrades$fradatoSNsemCalc<-as.Date(format(as.POSIXct(as.numeric(dfAAUMarriedGrades$fradatoSNsemCalc*3600*24), origin='1970-01-01') , format="%Y-%m-%d"))
dfAAUMarriedGrades$GPAgrade<-ifelse(dfAAUMarriedGrades$gradeType=="PF",gradesToGPAPFVec[as.character(dfAAUMarriedGrades$KARAKTER)],gradesToGPANumsVec[as.character(dfAAUMarriedGrades$KARAKTER)])
dfAAUMarriedGrades$rid<-seq(1:nrow(dfAAUMarriedGrades))

#HK the below overwrites the data already prepared in getin shape --- need to check this again most likely this is about people taking exams in master that should be taken in BSc and therefore added as e.g. sem 7
#dfAAUMarriedGrades$takenInSem<-ifelse(as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%m'))<6,
#                                      (as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%y'))-as.numeric(format(dfAAUMarriedGrades$fradatoSNsemCalc,'%y')))*2+ ceiling((as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%m')))/6) ,
#                                      (as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%y'))-as.numeric(format(dfAAUMarriedGrades$fradatoSNsemCalc,'%y')))*2+ floor((as.numeric(format(dfAAUMarriedGrades$bedom_dato,'%m'))-2)/6))


#dfSPVSNR<-distinct(dfAAUMarriedGrades, SPV, enrolID)
dfSPVSNR<-sqldf('select distinct b.SPV as SPV, c.enrolID as enrolID from dfECTSstruct as b, dfEnrolStatus as c where b.type=c.stype and c.fradatosn>= b.fromDate and c.fradatosn<=b.toDate ')

dfAAUMarriedGrades2<-merge(dfAAUGradesWODistEnrol,dfECTSstruct,by="aktivitet")
dfAAUMarriedGrades2<-dfAAUMarriedGrades2[dfAAUMarriedGrades2$fradatosn >= dfAAUMarriedGrades2$fromDate & dfAAUMarriedGrades2$fradatosn <=dfAAUMarriedGrades2$toDate,]
testdfAAUGrades<-data.frame(dfAAUGrades[,c("DistFromEnrol")])

missingRows<- dfAAUGradesWODistEnrol[!dfAAUGradesWODistEnrol$rowID %in% dfAAUMarriedGrades$rowID ,]
missingActivities<-data.frame(missingRows[!missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ]$aktivitetText)

missingRows1<-data.frame(missingRows[missingRows$aktivitetText %in% dfECTSstruct$aktivitetText, ])
missActivitetBefore2010<-sqldf("select reg_dato, aktivitet, fradatosn, startaar from missingRows where startaar<=2009") 
missingRows1<-missingRows1[!missingRows1$startaar %in% missActivitetBefore2010$startaar, ]


ectsSumSPbySPVAndCT<-dcast(ectsSumSPbySemAndCT, SPV~sem+CTdf)
### Comment: I get errors, and table is long-format.
ectsSumSPbySemAndCT<-dcast(ectsSumSPbySemAndCT, SPV+sem~CTdf)
ectsSumSPbySemAndCT[is.na(ectsSumSPbySemAndCT$El_nom),]$El_nom<-0


#creating the summaries of achieved ECTS per semester 
#semester, bctdf, ECTSpassed, 
#create the structure for all
#create semesters and edutypes
semesters<-data.frame(seq(1:10));names(semesters)<-c("semester"); eduType<-data.frame(c("bachelor","kandidat"));names(eduType)<-"type"
#multiply semesteers with edutypes
semSkeleton<-data.frame(merge(CTdfs,semesters));semSkeleton<-merge(semSkeleton,eduType)
#used SemEduTYpe multiplication to merge with students Enrolments into Edutypes
StudieNRSkeleton<-merge(distinct(dfEnrolStatus[,c("enrolID","stype")]),semSkeleton,by.x =c("stype"),by.y = c("type")  )
#StudieNRSkeleton<-merge(distinct(dfEnrolStatus[,c("studienr", "stype")]),semSkeleton,by.x =c("stype"),by.y = c("type")  )

ECTSattainedMelted<- merge(dfAAUMarriedGrades[dfAAUMarriedGrades$isPassed==1,c("bctdf", "enrolID","takenInSem", "ECTS")] , StudieNRSkeleton,by.x = c("enrolID","bctdf"),by.y =c("enrolID","CTdf") ,all.y=TRUE)

maxECTSTakenByCourseBySemester<-sqldf('select bctdf, enrolID, takenInSem, aktivitet, max(ECTS) as ECTS from dfAAUMarriedGrades group by bctdf, enrolID, takenInSem, aktivitet')

#ECTSattmptedMelted<- merge(dfAAUMarriedGrades[,c("bctdf", "enrolID","takenInSem", "ECTS")] , StudieNRSkeleton,by.x = c("enrolID","bctdf"),by.y =c("enrolID","CTdf") ,all.y=TRUE)

ECTSattmptedMelted<- merge(maxECTSTakenByCourseBySemester[,c("bctdf", "enrolID","takenInSem", "ECTS")] , StudieNRSkeleton,by.x = c("enrolID","bctdf"),by.y =c("enrolID","CTdf") ,all.y=TRUE)


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

ECTSatt<- dcast(ECTSattainedMelted,enrolID~semester+bctdf,value.var = "ECTS",sum)
ECTSatmp<-dcast(ECTSattmptedMelted,enrolID~semester+bctdf,value.var = "ECTS",sum)
ECTSovw<- dcast(ECTSperf,enrolID~semester+bctdf+what,value.var = "ECTS",sum)
ECTSovw<-merge(ECTSovw,dfSPVSNR,by="enrolID")

GPAavg<- merge(dfAAUMarriedGrades[,c("bctdf", "enrolID","takenInSem", "GPAgrade")] , StudieNRSkeleton,by.x = c("enrolID","bctdf"),by.y =c("enrolID","CTdf") ,all.y=TRUE)
#GPAavg<- dfAAUMarriedGrades[!is.na(dfAAUMarriedGrades$GPAgrade),c("bctdf", "enrolID","takenInSem", "GPAgrade","semester")]
GPAavg<-GPAavg[order(GPAavg$enrolID, GPAavg$bctdf, GPAavg$semester,GPAavg$takenInSem),]
GPAavg<-GPAavg[GPAavg$takenInSem<=GPAavg$semester|is.na(GPAavg$takenInSem),]
GPAavgagg<-sqldf("select enrolID, bctdf, semester, avg(GPAgrade) as GPAavg from GPAavg group by enrolID, bctdf, semester")
GPAavgagg$bctdf<-gsub("project","Pr",GPAavgagg$bctdf);GPAavgagg$bctdf<-gsub("elective","El",GPAavgagg$bctdf);
GPAavgagg$bctdf<-paste(GPAavgagg$bctdf,"mGPA",sep = "_")
#GPAavgagg<-ddply(GPAavg, .(enrolID, bctdf,semester),  GPAavg=mean(GPAgrade))
GPAavgagg<-dcast(GPAavgagg,enrolID~semester+bctdf,value.var = "GPAavg",sum)

#need to verify we have all the students in ECTSovw TODO
ectsAggsAll<-merge(ECTSovw,ectsSumSPbySPVAndCT, by="SPV")

#From2010s<-dfEnrolStatus[dfEnrolStatus$startaar>2009,]
ForSvante<-merge(dfEnrolStatus,ectsAggsAll)
ForSvante<-ForSvante[ForSvante$startaar>2009,]
ForSvante<-merge(ForSvante,GPAavgagg)
sqldf("select studienr, spv, count(studienr) from ForSvante group by studienr,SPV having count(studienr)>1 order by studienr")

dfKvote1<-sqldf('select studienr, priop, UDD_KODE, kvotient, kvote, land, Campus from dfKvote ')
dfM1<-sqldf('select studienr, MAT_Niveau, MATGrade, ENG_Niveau, ENGGrade, DAN_Niveau, DANGrade from dfM ')
ForSvante1<-merge(dfKvote1,dfM1) # need to remove studienr duplicates
ForSvante2<-unique(merge(ForSvante,ForSvante1), by = "enrolID")
ForSvante2$studienr<-NULL

write.csv(ForSvante,file = "MedData.csv")
write.csv(ForSvante2,file = "MedData1.csv")

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
# 
testxovw<-merge(ECTSovw[,c("studienr","type")],ECTSovwx[,c("studienr","type")],all.x=TRUE)
testyovw<-sqldf("select a.studienr, a.type, b.studienr, b.type from ECTSovw as a left outer join ECTSovwx as b using (studienr,type) ")


sqldf("select studienr, fradatosn,strftime('%m', fradatosn) from dfEnrolStatus where strftime('%m', fradatosn)<>'09'")
sqldf("SELECT strftime('%m','now')")


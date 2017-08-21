library(xlsx)
library(sqldf)
# library(RH2)
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
is.odd <- function(x) x %% 2 != 0 
library(gsheet)

#library(Rcmdr)
#library(reshape)

################ import ######################################
#steps to take - get data from qlikview (grades, enrolStatus) and from Cristina Draghici the kvote and GymData
#with clickview data import into GoogleSheets go to bottom: REMOVE::::  Selection Status: Uddannelse: Medialogiand 
# export i.e. download  as CSV and move to folder /Users/hendrik/Google Drive/dropOutInitiative/data/
#grades B-passed, I-failed, EB - not graded (not allowed to sit exam), U - no show but they specific meanings regarding when you can take the re-exam.


#ThisYear=2017
#dfMed2SS2<-dfMed2SS2[as.numeric(format(as.Date(dfMed2SS2$Timestamp, format="%d/%m/%Y %H:%M:%S"),"%Y"))==ThisYear,]





# import all files --------------------------------------------------------

myWD1<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Dropbox/drop out initiative/dataAnalysis'} else {"~/SVN/01Projects/dropOut/data/"}
setwd(myWD1)

dfMed1Q999<-gsheet2tbl('https://docs.google.com/spreadsheets/d/19_UD0a2lh-u3ES1ZU6KJ0BASQK7lQmM_CePdWSyts5s/edit#gid=0')
dfMed1Interviews<-gsheet2tbl('https://docs.google.com/spreadsheets/d/19_UD0a2lh-u3ES1ZU6KJ0BASQK7lQmM_CePdWSyts5s/edit#gid=1292286222')
#dfMed1Q999$Name <-NULL
dfMed1Q999<-dfMed1Q999[,1:7]

courseStudyPlanStructure<-read.csv("course_SPV.csv",header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")

dfpppStudents<-read.csv("P0P1P2StudentsFromCharlotte.txt",header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")

dfEnrolStatusMsc<-read.csv("MedEnrolMScThroughJul2017.csv",header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")
#dfEnrolStatusMsc$fradatosn<-as.Date(as.character(dfEnrolStatusMsc$fradatosn) , "%d.%m.%Y")
dfEnrolStatusMsc$stype<-as.factor("kandidat")
dfEnrolStatusBsc<-read.csv("MedEnrolBScThroughJul2017.csv",header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")
dfEnrolStatusBsc$stype<-as.factor("bachelor")
dfEnrolStatusBsc$Studieordningskode<-as.factor(NA)
dfEnrolStatus<-rbind(dfEnrolStatusMsc,dfEnrolStatusBsc)
dfEnrolStatus<-dfEnrolStatus[!(dfEnrolStatus$udmeldsn=='Afvist på grund af manglende kvalifikationer'),]
dfEnrolStatus$enrolID<-seq(1:nrow(dfEnrolStatus))
dfEnrolStatus$navn<-NULL

dfEnrolStatus$fradatosn<-as.Date(as.character(dfEnrolStatus$fradatosn) , "%d.%m.%Y")
dfEnrolStatus$slutdatosn<-as.Date(as.character(dfEnrolStatus$slutdatosn) , "%d.%m.%Y")
dfEnrolStatus$yearOfEnrolment <- dfEnrolStatus$startaar
dfEnrolStatus$startaar<-as.numeric(as.character(dfEnrolStatus$startaar))
dfEnrolStatus$EndSemester<-ifelse(is.na(dfEnrolStatus$slutdatosn) ,NA, ifelse(as.numeric(format(dfEnrolStatus$fradatosn,'%m')<6),(format(dfEnrolStatus$slutdatosn,'%y')-dfEnrolStatus$startaar)*2+ ceiling((as.numeric(format(dfEnrolStatus$slutdatosn,'%m')))/6), (format(dfEnrolStatus$slutdatosn,'%y')-dfEnrolStatus$startaar)*2+ floor((as.numeric(format(dfEnrolStatus$slutdatosn,'%m'))-2)/6)))

dfSchoolGrades<-read.csv("kot_medialogi_2011_2016_gymfag.csv",header = TRUE, fill=TRUE, sep = ",")

dfKvote<-read.csv("kot_medialogi_2011_2016_kvote.csv",header = TRUE, fill=TRUE, sep = ",")
dfAAUGrades<-read.csv("MEDgradesTilAug32017.csv",header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")
dfAktivitetsKode<-sqldf("select distinct aktiv_kode, aktivitet from dfAAUGrades")
#OLD data: dfAAUGrades<-read.csv("gradesTil2017Mar.csv",header = TRUE, fill=TRUE, sep = ",")
dfAAUGrades$bedom_dato<- as.Date(as.character(dfAAUGrades$bedom_dato) , "%d.%m.%Y")
dfAAUGrades$reg_dato<- as.Date(as.character(dfAAUGrades$reg_dato) , "%d.%m.%Y")

#match grades with enrolment data
dfAAUGrades<-merge(dfAAUGrades,dfEnrolStatus[,c("studienr","startaar","fradatosn","slutdatosn","statussn","udmeldsn")])
dfAAUGrades<-dfAAUGrades[dfAAUGrades$bedom_dato>=dfAAUGrades$fradatosn & (dfAAUGrades$bedom_dato<=dfAAUGrades$slutdatosn|is.na(dfAAUGrades$slutdatosn)),]

#dfAAUGrades<-dfAAUGrades[dfAAUGrades$bedom_dato>dfAAUGrades$fradatosn,]
dfAAUGrades$isIntl<-ifelse(dfAAUGrades$Land=="Danmark",0,1)
dfAAUGrades$rowID<-seq(1:nrow(dfAAUGrades))
#remove double entries from re-enrolments same studienr exists twice in dfEnrolStatus
dfAAUGrades$DistFromEnrol<-difftime(dfAAUGrades$bedom_dato, dfAAUGrades$fradatosn, tz, units = "days")
distHelp<-sqldf("select studienr, aktiv_kode, bedom_dato, min(bedom_dato-fradatosn) As MinDistanceDays from dfAAUGrades group by studienr, aktiv_kode, bedom_dato")
dfAAUGrades<-merge(dfAAUGrades,distHelp,all.x = TRUE)
dfAAUGrades$aktivitetText<-as.character(dfAAUGrades$aktivitet)
dfAAUGrades<-dfAAUGrades[dfAAUGrades$DistFromEnrol==dfAAUGrades$MinDistanceDays,]
dfAAUGrades$takenInYear<-as.numeric(format(dfAAUGrades$bedom_dato,'%Y'))
#dfAAUGrades$takenInSem<-ifelse(as.numeric(format(dfAAUGrades$fradatosn,'%m')<6),(dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2+ ceiling((as.numeric(format(dfAAUGrades$bedom_dato,'%m')))/6), (dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2+ floor((as.numeric(format(dfAAUGrades$bedom_dato,'%m'))-2)/6))#-ifelse(as.numeric(format(dfAAUGrades$bedom_dato,'%m'))=1,1,0)

#ifelse(as.numeric(format(dfAAUGrades[dfAAUGrades$studienr==20136609,]$fradatosn,'%m')<6),(dfAAUGrades[dfAAUGrades$studienr==20136609,]$takenInYear-dfAAUGrades[dfAAUGrades$studienr==20136609,]$startaar)*2+ ceiling((as.numeric(format(dfAAUGrades[dfAAUGrades$studienr==20136609,]$bedom_dato,'%m')))/6),
 #      (dfAAUGrades[dfAAUGrades$studienr==20136609,]$takenInYear-dfAAUGrades[dfAAUGrades$studienr==20136609,]$startaar)*2+ floor((as.numeric(format(dfAAUGrades[dfAAUGrades$studienr==20136609,]$bedom_dato,'%m'))-2)/6))#-ifelse(as.numeric(format(dfAAUGrades$bedom_dato,'%m'))=1,1,0)


dfAAUGrades$monthSemMod<-floor((as.numeric(format(dfAAUGrades$bedom_dato,'%m'))+1)/6)
#EB-couldn't take exam ('hand in blank'), U - not allowed to go to the exam, I - fail, B - pass
gradesPassedLUVec<-c('02'=1,'4'=1,'7'=1,'10'=1,'12'=1,'00'=0,'-3'=0,'B'=1,'EB'=0,'U'=0,'I'=0)
gradesToNumsVec<-c('02'=2,'4'=4,'7'=7,'10'=10,'12'=12,'00'=0,'-3'=-3,'EB'=-5,'U'=-8,'B'=2,'I'=0)


jobHoursVector<-c('10+ hours a week'=12,'0-5 hours a week'=2.5,'5-10 hours a week'=7.5,'No'=0)
pensumVector<-c('Need to catch up a little'=-1,'Far behind'=-3,'Following just fine'=0,"I'm way ahead"=2)
EduPrioVector<-c('1st priority'=1,'2nd priority'=2)
EducationLevelVector<-c('Upper secondary school'=13,'Lower secondary school'=10,'Vocational education'=14,'Academic degree'=17)

dfMed1Interviews$jobHoursPerWeek<-jobHoursVector[as.character(dfMed1Interviews$'Are you currently/planning on having a student job?')]
dfMed1Interviews$AVSCatchUp<-pensumVector[as.character(dfMed1Interviews$'How are you keeping up with the courses? [AVS]')]
dfMed1Interviews$PVCatchUp<-pensumVector[as.character(dfMed1Interviews$'How are you keeping up with the courses? [PV]')]
dfMed1Interviews$GPROCatchUp<-pensumVector[as.character(dfMed1Interviews$'How are you keeping up with the courses? [GPRO]')]
dfMed1Interviews$MedStudyPrio<-EduPrioVector[as.character(dfMed1Interviews$'How did you prioritize the Medialogy education when you applied?')]
dfMed1Interviews$FatherEduYrs<-EducationLevelVector[as.character(dfMed1Interviews$'What is the background of your parents? [Father]')]
dfMed1Interviews$MotherEduYrs<-EducationLevelVector[as.character(dfMed1Interviews$'What is the background of your parents? [Mother]')]
dfMed1Interviews$ParentsEduMax<-ifelse(dfMed1Interviews$FatherEduYrs>dfMed1Interviews$MotherEduYrs,dfMed1Interviews$FatherEduYrs,dfMed1Interviews$MotherEduYrs)
dfMed1Interviews$ParentsEduAvg<-(dfMed1Interviews$FatherEduYrs+dfMed1Interviews$MotherEduYrs)/2
dfMed1Interviews$MedHappyWith<-dfMed1Interviews$'How happy are you with studying Medialogy?'
dfMed1Interviews$MedBelongHere<-dfMed1Interviews$'What is your sense of belonging to Medialogy?'
dfMed1Interviews$WantMScDeg<-ifelse(dfMed1Interviews$'Do you plan on taking a master degree?'!="",grepl("Yes", dfMed1Interviews$'Do you plan on taking a master degree?', fixed=TRUE),NA) 
dfMed1Interviews$WantMedMScDeg<-ifelse(dfMed1Interviews$'Do you plan on taking a master degree?'!="",grepl("Medialogy", dfMed1Interviews$'Do you plan on taking a master degree?', fixed=TRUE),NA) 
colsdfmInt <- sapply(dfMed1Interviews, is.logical)
dfMed1Interviews[,colsdfmInt] <- lapply(dfMed1Interviews[,colsdfmInt], as.numeric)

#library(Rcmdr)

factorListFromInterviews<-c("studienr", "picOnMoodle","hoursWorkedPerWeek", "jobHoursPerWeek","AVSCatchUp","GPROCatchUp","MedStudyPrio","ParentsEduMax","ParentsEduAvg","MedHappyWith","MedBelongHere", "WantMScDeg","WantMedMScDeg")
dfInterviews<-dfMed1Interviews[,factorListFromInterviews]

CourseAcronymsLUVec<-c('Grundlæggende programmering'='GPRO',
                       'Problembaseret læring i videns'='PV',
                       'Animation og grafisk design'='AGD',
                       'Audio-Visuel Sketching'='AVS',
                       'Fysisk interface design'='PID',
                       'Matematik til multimedie-appli'='MMA',
                       'Programmering af interaktive s'='PFI',
                       'Interaktionsdesign'='ID',
                       'A/V produktion'='AVP',
                       'Avanceret A/V-produktion'='AAVP',
                       'Billedbehandling'='IP',
                       'Computergrafik programmering'='CGP',
                       'Design af brugeroplevelsen for'='UXD',
                       'Design og analyse af eksperime'='DAE',
                       'Embodied Interaction'='EI',
                       'Forskning i Medialogi'='RIM',
                       'Foundations in Medialogy'='FoM',
                       'Foundations in Medialogy (Comp'='FoCG',
                       'Foundations in Medialogy (Inte'='FoI',
                       'Foundations in Medialogy (Spil'='FoG',
                       'Kreativ innovation og entrepre'='Ent',
                       'Kreativ leg - teknologisk udfo'='P0',
                       'Lyd- og musikbehandling'='AMP',
                       'Lydbehandling'='AP',
                       'Mediesociologi og psykologi'='MSP',
                       'Multimodal perception og kogni'='MMPC',
                       'Modellering af fysiske systeme'='MPS',
                       'Multivariat statistik og mønst'='MVSP',
                       'Narrativer i digital kultur'='NDK',
                       'Objektorienteret Software Engi'='OSE',
                       'Perception'='Per',
                       'Proceduremæssig programmering'='PP',
                       'Programmering af komplekse sof'='PCS',
                       'Prototyping og fremstillingste'='PFT',
                       'Realtids interfaces og interak'='RTII',
                       'Rendering af computergrafik'='CGR',
                       'Rendering og animation'='RaA',
                       'Screen Media'='SM'
)



# prep entry grades dfEntryGradesAll -------------------------------------------------------

dfGradesPerson<-sqldf('select studienr, GYMFAG, NIVEAU, avg(KARAKTER) as avgGrade from dfSchoolGrades group by studienr, GYMFAG, NIVEAU')
matGrade<-sqldf('select studienr, NIVEAU as "MAT_Niveau", avgGrade as MATGrade from dfGradesPerson where GYMFAG = "MAT" group by studienr, NIVEAU order by studienr, NIVEAU')
matGrade<-matGrade[ !duplicated(matGrade$studienr), ]

engGrade<-sqldf('select studienr, NIVEAU as "ENG_Niveau", avgGrade as ENGGrade from dfGradesPerson where GYMFAG = "ENG" group by studienr, NIVEAU order by studienr, NIVEAU')
engGrade<-engGrade[ !duplicated(engGrade$studienr), ]
danGrade<-sqldf('select studienr, NIVEAU as "DAN_Niveau", avgGrade as DANGrade from dfGradesPerson where GYMFAG = "DAN" group by studienr, NIVEAU order by studienr, NIVEAU')
danGrade<-danGrade[ !duplicated(danGrade$studienr), ]
dfAllGradesXTab <-merge(merge(matGrade,engGrade,all.x = TRUE,all.y = TRUE),danGrade,all.x = TRUE,all.y = TRUE)
rm("matGrade","engGrade","danGrade")
#now contains all SchoolGrades and Kvote information
dfEntryGradesAll<-merge(dfAllGradesXTab,dfKvote,all.y = TRUE,all.x = TRUE)
dfEntryGradesAll$DANGradeX<-ifelse(is.na(dfEntryGradesAll$DANGrade),dfEntryGradesAll$ENGGrade,dfEntryGradesAll$DANGrade)

# prep AAU grades allHardByStudent---------------------------------------------------------


dfAAUGrades$gradeNum<-gradesToNumsVec[as.character(dfAAUGrades$KARAKTER)]
dfAAUGrades$GPAgrade<-gradesToNumsVec[as.character(dfAAUGrades$KARAKTER)]

dfAAUGrades$isPassed<-gradesPassedLUVec[as.character(dfAAUGrades$KARAKTER)]
dfAAUGrades$aktivitetShort<-CourseAcronymsLUVec[as.character(dfAAUGrades$aktivitet)]
#sqldf('select distinct aktivitet, aktiv_kode from dfAAUGrades order by aktivitet')

dfhard1st<-sqldf('select studienr, aktivitetShort, max(isPassed) as `1`  from dfAAUGrades where "forsoeg.nr." = 1 and aktivitetShort 
                 in ("GPRO",  "PID", "MMA","PFI","ID","AVS") group by studienr, aktivitetShort, karakter')
dfhard2nd<-sqldf('select studienr, aktivitetShort, max(isPassed) as `2` from dfAAUGrades where "forsoeg.nr." < 3  and aktivitetShort 
                 in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort, karakter')
df1stGrades<-sqldf('select studienr, aktivitetShort, gradeNum as `1g` from dfAAUGrades where "forsoeg.nr." = 1  and aktivitetShort 
                   in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort ')
df2ndGrades<-sqldf('select studienr, aktivitetShort, max(gradeNum) as `2g` from dfAAUGrades where "forsoeg.nr." =2 and aktivitetShort 
                   in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort ')
#dfLastGrades<-sqldf('select studienr, aktivitetShort, gradeNum as Lg, avg(takenInSem) as takenInSem from dfAAUGrades where "Sidste.Fors." = "Ja"  and aktivitetShort in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort ')

sqldf("select aktivitetShort, avg(takenInSem) from dfLastGrades group by aktivitetshort")
#head(sqldf('select studienr from dfhard2nd where studienr not in (select studienr from dfhard1st)'))

dfhard<-merge(dfhard1st,dfhard2nd,all.y = TRUE,all.x = TRUE)
dfhard<-melt(dfhard,id.vars=c("studienr","aktivitetShort"))
allHardByStudent<-dcast(dfhard,studienr~aktivitetShort+variable,fun.aggregate = sum, fill=NA_real_)

dfGradesByTry<-merge(df1stGrades,df2ndGrades,all.y = TRUE,all.x = TRUE)
dfGradesByTry<-merge(dfGradesByTry,dfLastGrades[, names(dfLastGrades) != "takenInSem"],all.y = TRUE,all.x = TRUE)
dfGradesByTry<-melt(dfGradesByTry,id.vars=c("studienr","aktivitetShort"))
#dfGradesByTry$value<-gradesToNumsVec[as.character(dfGradesByTry$value)]
dfGradesByTry<-dcast(dfGradesByTry,studienr~aktivitetShort+variable,fun.aggregate = sum,fill=NA_real_)

#dfLastGrades<-melt(dfLastGrades,id.vars=c("studienr","aktivitetShort"))


# prep enrol info dfM has Enrol status ---------------------------------------------------------
dfM<-dfEnrolStatus
lookupDropOutsVector=c('afslutte'= 0, 'afbrudt'=1, '~ben'=0,'orlov'=0)
lookupDropOutsiUniVector=c(Afsluttet= 0, Afbrudt=1, Indskrevet=0, 'Afbrudt (School skift)'=0,'Afbrudt(Fak skift)'=0,'Afbrudt (SN skift)'=0)
lookupDropOutsUdMeldsn=c('Afbrudt af institutionen'= 1, 'Afbrudt af den studerende'=1, 'Afbrudt ved studieskift'=1,Indskrevet=0, 'Indskrevet'=0,'Afsluttet'=0)

# MISSING FIELD IN CURRENT FILE dfM$isDropOutButInUni<-lookupDropOutsiUniVector[as.character(dfM$statussn)]
#dfM$isDropOut<-lookupDropOutsVector[as.character(dfM$statussn)]
dfM$isDropOut<-lookupDropOutsUdMeldsn[as.character(dfM$udmeldsn)]

dfM<-merge(dfM,dfEntryGradesAll,by= 'studienr', all.y = TRUE,all.x = TRUE)
dfM$snapShotYear<-2017
dfM$campus<-factor(dfM$Campus,levels=c("Aalborg","Kbh.","Esbjerg"))

dfM<-merge(dfM,allHardByStudent,by="studienr",all.x = TRUE,all.y = TRUE)

dfM<-merge(dfM,dfGradesByTry,by="studienr",all.x = TRUE,all.y = TRUE)
dfM<-merge(dfM,dfAAUGrades[!duplicated(dfAAUGrades$studienr),c("studienr","isIntl")],by="studienr",all.x = TRUE)
#dfM<-merge(dfM,dfInterviews,by="studienr")

#dfAll$aktivitet<-as.factor(dfAll$aktivitet)

dfM$mathGradeBinned<-cut(dfM$MATGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18))
dfM$mathGradeBinHighGran<-cut(dfM$MATGrade,breaks=c(-6,1,3,4,5,6,7,9,11,18),right = FALSE)
dfM$ENGGradeBinned<-cut(dfM$ENGGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18),right = FALSE)


dfM<-dfM[!is.na(dfM$MATGrade),]
dfM$mathLevelABC<-dfM$MAT_Niveau
dfM$mathLevel<-ifelse(dfM$MAT_Niveau %in% c("B","C"),"B","A" )
dfM$NAVN<-NULL
dfM$Campus<-NULL
dfPFI<-dfM[dfM$startaar==2015 & !is.na(dfM$startaar),]

dfAll<-merge(dfMed1Q999,dfEnrolStatus,all.x = TRUE)
dfAll<-merge(dfAll,dfInterviews,by="studienr")

lookupDropOutsVector=c('Afsluttet'= 0, 'Afbrudt'=1, 'Indskrevet'=0, 'Afbrudt (School skift)'=1,'Afbrudt(Fak skift)'=1,'Afbrudt (SN skift)'=1,'Afbrudt af den studerende'=1,'Afbrudt af institutionen'=1)
lookupDropOutsiUniVector=c('Afsluttet'= 0, 'Afbrudt'=1, 'Indskrevet'=0, 'Afbrudt (School skift)'=0,'Afbrudt(Fak skift)'=0,'Afbrudt (SN skift)'=0,'Afbrudt af den studerende'=1,'Afbrudt af institutionen'=1)
dfAll$isDropOutButInUni<-lookupDropOutsiUniVector[as.character(dfAll$udmeldsn)]
dfAll$isDropOut<-lookupDropOutsVector[as.character(dfAll$udmeldsn)]

dfAll$DropOutQ999Combo<-ifelse(dfAll$isDropOut==1|dfAll$P0Q999==1|dfAll$P1Q999==1|dfAll$P2Q999==1,1,0)

#remove side entries without data - need from Christina draghizi = missing 10 data points including
dfAll<-dfAll[!is.na(dfAll$isDropOutButInUni),]
dfAll<-merge(dfM,dfAll,all.y = TRUE)


dfMed2Aal<-merge(dfMed1Q999,dfEnrolStatus,all.x = TRUE)
dfMed2Aal<-merge(dfMed2Aal,dfInterviews,by="studienr",all.x = TRUE)
dfMed2Aal<-merge(dfMed2Aal,dfEntryGradesAll,by="studienr",all.x = TRUE)
dfMed2Aal<-merge(dfMed2Aal,dfGradesByTry,by="studienr",all.x = TRUE)
dfMed2Aal<-merge(dfMed2Aal,allHardByStudent,by="studienr",all.x = TRUE)
dfMed2Aal$isDropOutButInUni<-lookupDropOutsiUniVector[as.character(dfMed2Aal$udmeldsn)]
dfMed2Aal$isDropOut<-lookupDropOutsVector[as.character(dfMed2Aal$udmeldsn)]
dfMed2Aal$DropOutQ999Combo<-ifelse(dfMed2Aal$isDropOut==1|dfMed2Aal$P0Q999==1|dfMed2Aal$P1Q999==1|dfMed2Aal$P2Q999==1,1,0)
dfMed2Aal$interviewTaken<-ifelse(is.na(dfMed2Aal$MedHappyWith),0,1)
dfPredList<-c("interviewTaken","picOnMoodle","MATGrade","MAT_Niveau")
dfPredListAndInterview<-c(dfPredList, factorListFromInterviews)

FirstSemFactorList<-c('isDropOut', "interviewTaken", "DANGradeX","MATGrade","ENGGrade", "picOnMoodle","hoursWorkedPerWeek", "jobHoursPerWeek","AVSCatchUp","GPROCatchUp","MedStudyPrio","ParentsEduMax","ParentsEduAvg","MedHappyWith","MedBelongHere", "WantMScDeg","WantMedMScDeg")

dfMed2AalX<-dfMed2Aal[dfMed2Aal$interviewTaken==1 & !is.na(dfMed2Aal$isDropOut) &!is.na(dfMed2Aal$ENGGrade) &!is.na(dfMed2Aal$MATGrade) & !is.na(dfMed2Aal$DANGradeX) & !is.na(dfMed2Aal$GPROCatchUp) & !is.na(dfMed2Aal$hoursWorkedPerWeek),FirstSemFactorList]

myWD1<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Dropbox/drop out initiative/dataAnalysis'} else {"~/git/AAU/DropOutProject/analysis/"}
setwd(myWD1)


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
#library(DBI)


# From Bianca: why different grade values in import/aggregation files?
# From Bianca: Should we remove dropout data from SVN, now that we have it all in Filesharer?


is.odd <- function(x) x %% 2 != 0 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep=''))


################ import ######################################
#steps to take - get data from qlikview (grades, enrolStatus) and from Cristina Draghici the kvote and GymData
#with clickview data import into GoogleSheets go to bottom: REMOVE::::  Selection Status: Uddannelse: Medialogiand 
# export i.e. download  as CSV and move to folder /Users/hendrik/Google Drive/dropOutInitiative/data/
#grades B-passed, I-failed, EB - not graded (not allowed to sit exam), U - no show but they specific meanings regarding when you can take the re-exam.


############################
## get data from database ##
############################

library(RMySQL)
mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver);dbSendQuery(mydb,"SET NAMES utf8")

rs<-dbGetQuery(mydb, 'set character set latin1')
rs<-dbSendQuery(mydb, "SELECT * FROM tbl_optag")
dfEnrolStatusBsc<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

# local check that character_set_results and character_set_server are of the similar values
# this might be different on other pc's
dbGetQuery(mydb, "show variables like 'character_set%'") # In my case: latin1 and latin1
# TODO: perhaps we can solve this by changing character_set_server to UTF8, e.g. in a config file?

rs<-dbSendQuery(mydb, "SELECT * FROM map_personIDs")
personIDs<- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM map_SPVCmapping")
dfECTSstruct<- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM tbl_HSgrades")
dfSchoolGrades<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM tbl_AAUgrades")
dfAAUGrades<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM tbl_frafaldAAUmed")
dfDropMed<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM tbl_frafaldAAUall")
dfDropAAUall<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM tbl_Q999")
Q999<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "SELECT * FROM LA.SPP_participation") # removed ;
SSPparticipation<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

rs<-dbSendQuery(mydb, "call SSPWide()")
SSPWide<-fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

dbDisconnect(mydb)
#not detaching RMySQL will make sqldf not work
detach("package:RMySQL", unload=TRUE)


###########################
# temp. data import and override
#setwd('Z:/BNC/PBL development project/data/analysis_data/dropOut/data')
#dfECTSstruct<-read.csv("course_SPV2017.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

###########################


# BC: temp merit import
# gives lowest passing grades in merit courses
setwd('Z:/BNC/PBL development project/data/analysis_data/dropOut/data_2017cohortCPHAAL')
merit2017<-read.csv("2008-18_merit_MED.csv", header = TRUE, fill=TRUE, sep = ",", check.names=TRUE, encoding="UTF-8", stringsAsFactors=FALSE)
merit2017<-merge(merit2017,personIDs, by.x = "X.U.FEFF.person_id", by.y = "PERSON_ID")
merit2017$KARAKTER <- 'B'
merit2017$ECTS <- merit2017$staa * 60 
merit2017$`Seneste Fors.` <- "Ja"
merit2017$`Forsoeg nr.` <- 1
merit2017$type <- "bachelor"
names(merit2017)[5] <- "aktivitet"
names(merit2017)[6] <- "bedom_dato"
names(merit2017)[7] <- "reg_dato"
names(merit2017)[14] <- "navn"
merit2017 <- merit2017[,c(4:7,12,14,17:20)]

#dfAAUGrades1 <- dfAAUGrades
dfAAUGrades <- bind_rows(dfAAUGrades,merit2017)



# subsetting based on startsn and slutsn
dfDropMedsFromAll<-dfDropAAUall[dfDropAAUall$startsn=='Medieteknologi'|dfDropAAUall$slutsn=='Medieteknologi',] # 2624
uniqueDropMedsFrAll<-sqldf('select distinct navn as fullName, studienr from dfDropMedsFromAll group by navn') 
count(uniqueDropMedsFrAll) # check: 2581

dfEnrolStatusBsc$fradatosn = as.Date(as.character(dfEnrolStatusBsc$fra_dato), "%d/%m/%Y")
dfEnrolStatusBsc$stype<-as.factor("bachelor")
dfEnrolStatusBsc$Studieordningskode<-as.factor(NA)
dfEnrolStatus<-dfEnrolStatusBsc
#FOR FUTURE WORK WHEN MSC data becomes relevant rbind(dfEnrolStatusMsc,dfEnrolStatusBsc)
dfEnrolStatus<-dfEnrolStatus[!(dfEnrolStatus$udmeld_begrundelse=='Afvist på grund af manglende kvalifikationer'),]
dfEnrolStatus<-dfEnrolStatus[!(dfEnrolStatus$udmeld_begrundelse=='Ikke accepteret tilbudt plads'),]
dfEnrolStatus$enrolID<-seq(1:nrow(dfEnrolStatus))
#dfEnrolStatus$navn<-NULL

#remove all students who did not start in september / HK: this will be problematic and solved differently
#dfEnrolStatus<-dfEnrolStatus[as.numeric(format(dfEnrolStatus$fradatosn,'%m'))==9,]

#dfEnrolStatus$fradatosn<-as.Date(as.character(dfEnrolStatus$fradatosn) , "%d.%m.%Y")

# BC commented this out and moved: 
####dfEnrolStatus$slutdatosn<-as.Date(as.character(dfEnrolStatus$udmeld_dato) , "%d/%m/%Y")
#dfEnrolStatus$yearOfEnrolment <- as.numeric(format(dfEnrolStatus$fradatosn,'%Y'))
dfEnrolStatus$startaar <- as.numeric(format(dfEnrolStatus$fradatosn,'%Y'))
dfEnrolStatus$EndSemester<-ifelse(is.na(dfEnrolStatus$slutdatosn) , NA,
                                    ifelse(as.numeric(format(dfEnrolStatus$slutdatosn,'%m'))>1 & as.numeric(format(dfEnrolStatus$slutdatosn,'%m'))<9,
                                           (as.numeric(format(dfEnrolStatus$slutdatosn,'%Y'))-dfEnrolStatus$startaar)*2, 
                                           (as.numeric(format(dfEnrolStatus$slutdatosn,'%Y'))-dfEnrolStatus$startaar)*2+ floor((as.numeric(format(dfEnrolStatus$slutdatosn,'%m'))-2)/7)
                                           )
                                  )                                  

#dfAAUGrades$takenInSem<-ifelse(dfAAUGrades$startMonth==9, ifelse(dfAAUGrades$examMonth>1 & dfAAUGrades$examMonth<9, 
#                                                                 (dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2,
#                                                                 (dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2+ floor((as.numeric(format(dfAAUGrades$bedom_dato-14,'%m'))-2)/7))  



# dfKvote<-read.csv("kot_medialogi_2011_2016_kvote.csv",header = TRUE, fill=TRUE, sep = ",") /HK need to check later whether all names are the same
dfAAUGrades$isLastTry<- ifelse(dfAAUGrades$`Seneste Fors.`=="Ja",1,0)
dfAAUGrades$isProj<- ifelse(dfAAUGrades$ECTS>5,1,0)

dfRetrier<-sqldf("select studienr, max(`Forsoeg nr.`) as MaxTry from dfAAUGrades group by studienr")
dfRetrier$isOffender <-ifelse(dfRetrier$MaxTry>3,1,0)

dfAAUGrades<-merge(dfAAUGrades,dfRetrier)
#sqldf("select isOffender, isProj, avg(GPAgrade) from dfAAUGrades where isLastTry=1 group by isOffender, isProj")
dfAktivitetsKode<-sqldf("select distinct aktiv_kode, aktivitet from dfAAUGrades")
dfAAUGrades$bedom_dato<- as.Date(as.character(dfAAUGrades$bedom_dato) , "%d.%m.%Y")
dfAAUGrades$reg_dato<- as.Date(as.character(dfAAUGrades$reg_dato) , "%d.%m.%Y")

#OLD: dfDropMed$fradatosn<-as.Date(as.character(dfDropMed$fradatosn) , "%d.%m.%Y")
#OLD: dfDropMed$fra_dato<-dfDropMed$fradatosn
dfDropMedsFromAll$fradatosn<-as.Date(as.character(dfDropMedsFromAll$fradatosn) , "%d.%m.%Y")
dfDropMedsFromAll$fra_dato<-dfDropMedsFromAll$fradatosn


# BC: changed the merge to include slutdatosn for students with "tidligere-optag"
dfEnrolStatus<-merge(dfEnrolStatus,dfDropMedsFromAll[,c("studienr","statussn","udmeldsn","fradatosn","slutdatosn")],by=c("studienr","fradatosn"),all.x=TRUE)
count(dfEnrolStatus) # 203

# BC: handling early dropouts with no registration
dfEnrolStatus$statussn <- ifelse(is.na(dfEnrolStatus$slutdatosn), "afbrudt", dfEnrolStatus$statussn)
dfEnrolStatus$udmeldsn <- ifelse(is.na(dfEnrolStatus$slutdatosn), dfEnrolStatus$udmeld_aarsag, dfEnrolStatus$udmeldsn)
dfEnrolStatus$udmeld_dato<-as.Date(as.character(dfEnrolStatus$udmeld_dato) , "%d/%m/%Y")
dfEnrolStatus$slutdatosn<-as.Date(as.character(dfEnrolStatus$slutdatosn) , "%d.%m.%Y")
dfEnrolStatus$slutdatosn<-if_else(is.na(dfEnrolStatus$slutdatosn), dfEnrolStatus$udmeld_dato, dfEnrolStatus$slutdatosn)

# BC: assumming that students with slutdatosn before 2017-08-31 are active,
# whereas students with slutdatosn after 2017-08-31 dropped out (
dfEnrolStatus$statussn <- if_else(dfEnrolStatus$slutdatosn <= as.Date('2017-08-31'), "åben", dfEnrolStatus$statussn)
dfEnrolStatus$statussn <- ifelse(is.na(dfEnrolStatus$slutdatosn), "åben", dfEnrolStatus$statussn)
dfEnrolStatus$udmeldsn <- ifelse(dfEnrolStatus$statussn == "åben", "Indskrevet", dfEnrolStatus$udmeldsn)

#match grades with enrolment data / BC: used columns from dfDropMedsFromAll instead to include statussn for Tidl-optag students (added in previous step)
dfAAUGrades<-merge(dfAAUGrades,dfEnrolStatus[,c("studienr","startaar","fradatosn","slutdatosn","statussn","udmeldsn","stype")],by.x = c("studienr","type"),by.y = c("studienr","stype"))
count(dfAAUGrades) #1622

#################### !!!!!
#NEEDS CHECKING this removes a few rows / BC: don't understand why we have this? Commented out, so it doesn't delete the merit grades that I added:
#dfAAUGrades1 <- dfAAUGrades
#dfAAUGrades1<-dfAAUGrades1[dfAAUGrades1$bedom_dato>=dfAAUGrades1$fradatosn & (dfAAUGrades1$bedom_dato<=dfAAUGrades1$slutdatosn+1|is.na(dfAAUGrades1$slutdatosn)),]
#dfAAUGrades<-dfAAUGrades[dfAAUGrades$bedom_dato>=dfAAUGrades$fradatosn,] # & (dfAAUGrades$bedom_dato<=dfAAUGrades$slutdatosn+1|is.na(dfAAUGrades$slutdatosn)),]
#count(dfAAUGrades) #1607
# OK: first condition removes 15 old grades from one student (20126013)
# Perhaps OK: second condition removes 4 grades entries, don't understand this

################
#test <- dfAAUGrades[ !(dfAAUGrades$bedom_dato>=dfAAUGrades$fradatosn & (dfAAUGrades$bedom_dato<=dfAAUGrades$slutdatosn+1|is.na(dfAAUGrades$slutdatosn))) ,]
# 4 students in test
#test2 <- dfAAUGrades[ !(dfAAUGrades$bedom_dato>=dfAAUGrades$fradatosn) ,]
# 1 student in test2
################


#dfAAUGrades<-dfAAUGrades[dfAAUGrades$bedom_dato>dfAAUGrades$fradatosn,]
dfAAUGrades$isIntl<-ifelse(dfAAUGrades$Land=="Danmark",0,1)
dfAAUGrades$rowID<-seq(1:nrow(dfAAUGrades))
#remove double entries from re-enrolments same studienr exists twice in dfEnrolStatus
dfAAUGrades$DistFromEnrol<-difftime(dfAAUGrades$bedom_dato, dfAAUGrades$fradatosn, tz, units = "days")
distHelp<-sqldf("select studienr, aktiv_kode, bedom_dato, min(bedom_dato-fradatosn) As MinDistanceDays from dfAAUGrades group by studienr, aktiv_kode, bedom_dato")
dfAAUGrades<-merge(dfAAUGrades,distHelp,all.x = TRUE)
dfAAUGrades$aktivitetText<-as.character(dfAAUGrades$aktivitet)
dfAAUGrades<-dfAAUGrades[dfAAUGrades$DistFromEnrol==dfAAUGrades$MinDistanceDays,]
dfAAUGrades$startMonth <-ifelse(as.numeric(format(dfAAUGrades$fradatosn,'%m'))>4,9,2)
dfAAUGrades$examMonth<-as.numeric(format(dfAAUGrades$bedom_dato,'%m'))
dfAAUGrades$takenInYear<-as.numeric(format(dfAAUGrades$bedom_dato,'%Y'))

#HKtodo: need to work on people who did not start in september / BC: I believe that we fixed this
dfAAUGrades$takenInSem<-ifelse(dfAAUGrades$startMonth==9, ifelse(dfAAUGrades$examMonth>1 & dfAAUGrades$examMonth<9, 
                                                                 (dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2,
                                                                 (as.numeric(format(dfAAUGrades$bedom_dato-14,'%Y'))-dfAAUGrades$startaar)*2+ floor((as.numeric(format(dfAAUGrades$bedom_dato-14,'%m'))-2)/7)),NA )             
                               #  continue here                                                        ifelse(dfAAUGrades$examMonth>1 & dfAAUGrades$examMonth<9, )     )                                                        




#hard coded data corrections for three students 
#need to check whether this is important or not... hard coded corrections / BC: I don't think so for 2017, but maybe for other years
#OLD:dfAAUGrades[dfAAUGrades$aktivitetText=="Sansning af medier (Computerg",]$aktivitetText<-"Sansning af medier (Computergr"

#1+(dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2) 
#as.numeric(format(dfAAUGrades$bedom_dato,'%Y')) ,2)

#dfEnrolStatus$EndSemester<-ifelse(is.na(dfEnrolStatus$slutdatosn) ,NA, ifelse(as.numeric(format(dfEnrolStatus$fradatosn,'%m')<6),
#                            (as.numeric(format(dfEnrolStatus$slutdatosn,'%Y'))-dfEnrolStatus$startaar)*2+ ceiling((as.numeric(format(dfEnrolStatus$slutdatosn,'%m')))/6), 
#                             (as.numeric(format(dfEnrolStatus$slutdatosn,'%Y'))-dfEnrolStatus$startaar)*2+ floor((as.numeric(format(dfEnrolStatus$slutdatosn,'%m'))-2)/6)))

                               
#dfAAUGrades$takenInSem<-ifelse(as.numeric(format(dfAAUGrades$fradatosn,'%m')<6),
#                               (dfAAUGrades$takenInYear-dfAAUGrad201es$startaar)*2+ ceiling((as.numeric(format(dfAAUGrades$bedom_dato,'%m')))/6),
#                               (dfAAUGrades$takenInYear-dfAAUGrades$startaar)*2+ floor((as.numeric(format(dfAAUGrades$bedom_dato,'%m'))-2)/6))#-ifelse(as.numeric(format(dfAAUGrades$bedom_dato,'%m'))=1,1,0)

#ifelse(as.numeric(format(dfAAUGrades[dfAAUGrades$studienr==20136609,]$fradatosn,'%m')<6),(dfAAUGrades[dfAAUGrades$studienr==20136609,]$takenInYear-dfAAUGrades[dfAAUGrades$studienr==20136609,]$startaar)*2+ ceiling((as.numeric(format(dfAAUGrades[dfAAUGrades$studienr==20136609,]$bedom_dato,'%m')))/6),
 #      (dfAAUGrades[dfAAUGrades$studienr==20136609,]$takenInYear-dfAAUGrades[dfAAUGrades$studienr==20136609,]$startaar)*2+ floor((as.numeric(format(dfAAUGrades[dfAAUGrades$studienr==20136609,]$bedom_dato,'%m'))-2)/6))#-ifelse(as.numeric(format(dfAAUGrades$bedom_dato,'%m'))=1,1,0)

dfAAUGrades$monthSemMod<-floor((as.numeric(format(dfAAUGrades$bedom_dato,'%m'))+1)/6)
#EB-couldn't take exam ('hand in blank'), U - not allowed to go to the exam, I - fail, B - pass
gradesPassedLUVec<-c('02'=1,'2'=1,'4'=1,'7'=1,'10'=1,'12'=1,'00'=0,'0'=0,'-3'=0,'B'=2,'EB'=0,'U'=0,'I'=0)
gradesToNumsVec<-c('02'=2,'2'=2,'4'=4,'7'=7,'10'=10,'12'=12,'00'=0,'0'=0,'-3'=-3,'EB'=-5,'U'=-8,'B'=2,'I'=0)


#CHECK if these are all accounted for in AAU grades with entries in map_SPVCmapping
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

dfGradesPerson<-sqldf('select studienr, GYMFAG, NIVEAU, max(KARAKTER) as avgGrade, count(KARAKTER) as GCount  from dfSchoolGrades group by studienr, GYMFAG, NIVEAU')
#check that nobody has multiple entries level A and B in a subject - the below dataframe should be empty
gradeCheck<-sqldf('select studienr, GYMFAG,  count (avgGrade) from dfGradesPerson group by studienr, GYMFAG having count (avgGrade)>1 ')
lookupNIVEAUVector=c('A'= 36, 'B'=24, 'C'=12)
dfGradesPerson$GradeCorrN<-dfGradesPerson$avgGrade+lookupNIVEAUVector[as.character(dfGradesPerson$NIVEAU)]
#create helper structure to identify maximum grade attained with A>B>C
GradeHelpx<-sqldf('select studienr, GYMFAG, max(GradeCorrN) as GradeCorrN from dfGradesPerson group by studienr, gymfag')
#use the helper to limit grades to max grades
dfGradesPerson<-merge(dfGradesPerson,GradeHelpx)

matGrade<-sqldf('select studienr, NIVEAU as "MAT_Niveau", avgGrade as MATGrade from dfGradesPerson where GYMFAG = "MAT" group by studienr, NIVEAU order by studienr, NIVEAU')
MATCheck<-sqldf('select studienr, count(MATGrade) from matGrade group by studienr')
matGrade<-matGrade[ !duplicated(matGrade$studienr), ]

engGrade<-sqldf('select studienr, NIVEAU as "ENG_Niveau", avgGrade as ENGGrade from dfGradesPerson where GYMFAG = "ENG" group by studienr, NIVEAU order by studienr, NIVEAU')
engGrade<-engGrade[ !duplicated(engGrade$studienr), ]
danGrade<-sqldf('select studienr, NIVEAU as "DAN_Niveau", avgGrade as DANGrade from dfGradesPerson where GYMFAG = "DAN" group by studienr, NIVEAU order by studienr, NIVEAU')
danGrade<-danGrade[ !duplicated(danGrade$studienr), ]
dfAllGradesXTab <-merge(merge(matGrade,engGrade,all.x = TRUE,all.y = TRUE),danGrade,all.x = TRUE,all.y = TRUE)
rm("matGrade","engGrade","danGrade")
#now contains all SchoolGrades and Kvote information
#NEED TO use subset of dfoptag
#OLD: dfEntryGradesAll<-merge(dfAllGradesXTab,dfEnrolStatusBsc[,c("studienr","kvotient","priop","kvote")],all.y = TRUE,all.x = TRUE)
dfEntryGradesAll<-merge(dfAllGradesXTab,dfEnrolStatus[,c("studienr","kvotient","priop","kvote")],all.y = TRUE,all.x = TRUE)

dfEntryGradesAll$DANGradeX<-ifelse(is.na(dfEntryGradesAll$DANGrade),dfEntryGradesAll$ENGGrade,dfEntryGradesAll$DANGrade)

# prep AAU grades allHardByStudent---------------------------------------------------------

dfAAUGrades$gradeNum<-gradesToNumsVec[as.character(dfAAUGrades$KARAKTER)]
dfAAUGrades$GPAgrade<-gradesToNumsVec[as.character(dfAAUGrades$KARAKTER)]
dfAAUGrades$isPassed<-gradesPassedLUVec[as.character(dfAAUGrades$KARAKTER)]
#OLD:dfAAUGrades$CourseLocation<-substr(dfAAUGrades$aktiv_kode,3,3)
dfAAUGrades$CourseLocation <-ifelse( (dfAAUGrades$CourseLocation<-substr(dfAAUGrades$aktiv_kode,3,3)) == 'K',3,4) #map to tbl_campi
dfAAUGrades$aktivitetShort<-CourseAcronymsLUVec[as.character(dfAAUGrades$aktivitet)]
#sqldf('select distinct aktivitet, aktiv_kode from dfAAUGrades order by aktivitet')

dfhard1st<-sqldf('select studienr, aktivitetShort, max(isPassed) as `1`  from dfAAUGrades where "forsoeg nr." = 1 and aktivitetShort 
                 in ("GPRO",  "PID", "MMA","PFI","ID","AVS") group by studienr, aktivitetShort, karakter')
dfhard2nd<-sqldf('select studienr, aktivitetShort, max(isPassed) as `2` from dfAAUGrades where "forsoeg nr." < 3  and aktivitetShort 
                 in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort, karakter')
df1stGrades<-sqldf('select studienr, aktivitetShort, gradeNum as `1g` from dfAAUGrades where "forsoeg nr." = 1  and aktivitetShort 
                   in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort ')
df2ndGrades<-sqldf('select studienr, aktivitetShort, max(gradeNum) as `2g` from dfAAUGrades where "forsoeg nr." =2 and aktivitetShort 
                   in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort ')
dfLastGrades<-sqldf('select studienr, aktivitetShort, gradeNum as Lg, avg(takenInSem) as takenInSem from dfAAUGrades where "Seneste Fors." = "Ja"  and aktivitetShort in ("GPRO",  "PID", "MMA","PFI","ID","AVS")  group by studienr, aktivitetShort ')

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
studentsWithoutEnrolStatus<-dfEnrolStatus[is.na(dfEnrolStatus$statussn),c("studienr", "statussn","enrolID","email")]
#NEEDS TO BE UPDATED WITH a clear matching to a medialogy studienr (in case students have multiple studienr) or match to the studienr that is closest to this one, 
# looking into the past of fra_datos from the date of the SSP  
studienrAndNames<-dfDropMedsFromAll[,c("navn","studienr")];studienrAndNames<-sqldf('select distinct navn as fullName, studienr from studienrAndNames')
#check  head(dfDropMed[(dfDropMed$studienr %in% dfDropMedsFromAll$studienr),]$studienr)

SSPWide<-merge(SSPWide, SSPparticipation)
SSPWide$SSPdate<-as.Date(SSPWide$`Started on`, format = "%d/%m/%Y %I:%M")
#HK: careful, currently the next command will lose rows in SSPWide (without all.x=TRUE) or with all.x=TRUE some will not have a studienr.... WHY?
SSPWide<-merge(SSPWide,studienrAndNames,all.x = TRUE)

###################################################
# BC: checks student numbers - I dont understand???
n <- data.frame(unique(dfAAUGrades$studienr)) #189
m <- data.frame(unique(dfEnrolStatus$studienr)) #203
s <- data.frame(SSPWide$studienr) #190
names(m) <- "studienr"
names(n) <- "studienr"
names(s) <- "studienr"
sqldf('SELECT * FROM m EXCEPT SELECT * FROM n') #14 studienr in dfEnrolStatus
sqldf('SELECT * FROM m EXCEPT SELECT * FROM s') #18 studienr

m <- m[!n]
m
##################################################

# prep enrol info dfM has Enrol status ---------------------------------------------------------
# BC: add Q999 to dfEnrolStatus
Q999 <- merge(Q999,uniqueDropMedsFrAll)
dfEnrolStatus$Q999 <- ifelse(dfEnrolStatus$studienr %in% Q999$studienr, 1,0)

# BC: include CourseLocation and nationality (country) 
dfEnrolStatus <- merge(dfEnrolStatus,dfAAUGrades[,c("CourseLocation","Land","studienr")], by = c("studienr"))
dfEnrolStatus<- dfEnrolStatus[!duplicated(dfEnrolStatus), ]

dfM<-dfEnrolStatus
lookupDropOutsVector=c('afslutte'= 0, 'afbrudt'=1, '~ben'=0,'orlov'=0)
lookupDropOutsiUniVector=c(Afsluttet= 0, Afbrudt=1, Indskrevet=0, 'Afbrudt (School skift)'=0,'Afbrudt(Fak skift)'=0,'Afbrudt (SN skift)'=0)
lookupDropOutsUdMeldsn=c('Afbrudt af institutionen'= 1, 'Afbrudt af den studerende'=1, 'Afbrudt ved studieskift'=1,Indskrevet=0, 'Indskrevet'=0,'Afsluttet'=0)





# MISSING FIELD IN CURRENT FILE dfM$isDropOutButInUni<-lookupDropOutsiUniVector[as.character(dfM$statussn)]
#dfM$isDropOut<-lookupDropOutsVector[as.character(dfM$statussn)]
dfM$isDropOut<-lookupDropOutsUdMeldsn[as.character(dfM$udmeldsn)]

removeColumns <- c("kvotient", "priop", "kvote")
dfEntryGradesAll1 <- dfEntryGradesAll
dfEntryGradesAll1 <- dfEntryGradesAll1[ , !(names(dfEntryGradesAll1) %in% removeColumns)]
dfM<-merge(dfM,dfEntryGradesAll1,by= 'studienr', all.y = TRUE,all.x = TRUE)
dfM$snapShotYear<-2018
dfM$campus<-factor(dfM$Campus,levels=c("Aalborg","Kbh.","Esbjerg"))

dfM<-merge(dfM,allHardByStudent,by="studienr",all.x = TRUE,all.y = TRUE)
dfM<-merge(dfM,dfGradesByTry,by="studienr",all.x = TRUE,all.y = TRUE)
dfM<-merge(dfM,dfAAUGrades[!duplicated(dfAAUGrades$studienr),c("studienr","isIntl")],by="studienr",all.x = TRUE)

dfM$mathGradeBinned<-cut(dfM$MATGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18))
dfM$mathGradeBinHighGran<-cut(dfM$MATGrade,breaks=c(-6,1,3,4,5,6,7,9,11,18),right = FALSE)
dfM$ENGGradeBinned<-cut(dfM$ENGGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18),right = FALSE)

#the following will drop a few students - not sure why we drop people that might not be necessary and better to carry them forward
dfM<-dfM[!is.na(dfM$MATGrade),]
dfM$mathLevelABC<-dfM$MAT_Niveau
dfM$mathLevel<-ifelse(dfM$MAT_Niveau %in% c("B","C"),"B","A" )
dfM$NAVN<-NULL
#dfM$Campus<-NULL
#dfPFI<-dfM[dfM$startaar==2015 & !is.na(dfM$startaar),]


####### runs the aggregation rmarkdown file

#rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/PBLstats-on-grades/docs/aggregationOfInformationBySemesters.Rmd", 
#                  output_format = "pdf_document", output_file = "aggregationOfInformationBySemesters.pdf")



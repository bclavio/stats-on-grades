library(tidyr)

#Grades data
MedDataBSc2017_grades <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/MedDataBSc2017_grades.csv", encoding = 'ANSI',stringsAsFactors = F)
MedDataBSc2017_grades <- MedDataBSc2017_grades[,-c(2,4)]

#Highschool data
MedDataBSc2017 <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/MedDataBSc2017_1107.csv", encoding="ANSI", stringsAsFactors=FALSE)
MedDataBSc2017 <- MedDataBSc2017[,c(2,3,22,36:39)]

#SSP data
library(RMySQL)
libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep='')) # MAC and Windows
mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver)
rs<-dbSendQuery(mydb, "call SSPWide()")
SSPWide<-fetch(rs, n=-1)
dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)

#Getting studienr and campus
SSPAAL<-read.csv("Y:/analysis_data/SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPAAL$campus <- 'AAL'
SSPCPH <- read.csv("Y:/analysis_data/SSP/SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPCPH$campus <- 'CPH'
SSP <- rbind(SSPAAL,SSPCPH)
names(SSP)[1] <- 'Surname'
SSP <- unite_(SSP,'Name', c("First name","Surname"), sep = ' ')
SSP <- SSP[,c(1,4,121)]
which(!SSPWide$email %in% SSP$`Email address`)
namestudyno <- read.csv("Y:/analysis_data/dropOut/data/bsc.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
namestudyno <- namestudyno[,c(18,19)]
namestudyno <- namestudyno[!duplicated(namestudyno),]
SSPIncStudyNo <- merge(SSP,namestudyno, by.x='Name',by.y='navn')
studienrEmail <- SSPIncStudyNo[,-1]
names(studienrEmail)[1] <- 'email'

SSPWide <- merge(SSPWide,studienrEmail)

MedDataBSc2017SSP <- merge(MedDataBSc2017,SSPWide,by='studienr',all.x = T)
MedDataBSc2017SSP <- MedDataBSc2017SSP[,-(8:10)]

#merge with grades
MedDataGrades <- merge(MedDataBSc2017_grades,MedDataBSc2017SSP,by='studienr')

###################################
############math###################
###################################

#math midterm for AAL
dfmidtermmath <- read.table("Y:/MT-March.csv", quote="\"", comment.char="",stringsAsFactors = F)
names(dfmidtermmath) <- c('studienr','MMAmidterm')
MedDataGradesMath <- merge(MedDataGrades,dfmidtermmath,by='studienr',all.x = T)
MedDataGradesMath <- MedDataGradesMath[!is.na(MedDataGradesMath$MMA),]
MedDataGradesMath$MMApassfail <- ifelse(MedDataGradesMath$MMA>=2,'passed','failed')

################## Both campus #############
####Plots

#############Regression for grade

#####Choose variables with lasso
#without GPRO
#with GPRO

#####Choose variables with stepwise search
#without GPRO
#with GPRO

#####choose variables with best subset selection
#without GPRO
#with GPRO

#####compare with crossvalidation

############Classification passed failed

#######logistic regression
##lasso
##stepwise

#######Cart

#######linear regression models

######compare with cross validation

##############only Aalborg#########
library(tidyr)
library(ggplot2)
library(car)
library(data.table)

#SSP
questions<-read.csv("Y:/analysis_data/SSP/QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
SSPAAL<-read.csv("Y:/analysis_data/SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPCPH <- read.csv("Y:/analysis_data/SSP/SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPAAL$Campus <- 'AAL'
SSPCPH$Campus <- 'CPH'
SSPAAL <- SSPAAL[-nrow(SSPAAL),]
names(SSPAAL)[1] <- 'Surname'
SSPAAL <- unite_(SSPAAL,'Name', c("First name","Surname"), sep = ' ')
names(SSPCPH)[1] <- 'Surname'
SSPCPH <- unite_(SSPCPH,'Name', c("First name","Surname"), sep = ' ')

#Filling in manual answers
SSPManualAAL <-read.csv("Y:/analysis_data/SSP/SSPanswersTestAAL 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
SSPManualAAL <- unite_(SSPManualAAL,'Name', c("First name","Surname"), sep = ' ')
which(!SSPAAL$Name%in%SSPManualAAL$Name)
#Removing the one person who is in progress
SSPAAL <- SSPAAL[-91,]
#Check that all observation coinside and insert the values
all.equal(SSPAAL$Name,SSPManualAAL$Name)
SSPAAL[,c('StudyHours','StudyRelatedWorkHours','OtherWorkHours')] <- SSPManualAAL[,c('Response 93','Response 95','Response 96')]

SSPManualCPH <-read.csv("Y:/analysis_data/SSP/SSPanswersTestCPH 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
SSPManualCPH <- unite_(SSPManualCPH,'Name', c("First name","Surname"), sep = ' ')
#Check that all observation coinside and insert the values
all.equal(SSPCPH$Name,SSPManualCPH$Name)
SSPCPH[,c('StudyHours','StudyRelatedWorkHours','OtherWorkHours')] <- SSPManualCPH[,c('Response 93','Response 95','Response 96')]
#There is one student who has reported 1015 in Q96. That must be a mistake and since
#the answers to Q93 and Q95 are 15 this value is used instead (Possibly meant 10-15)
SSPCPH$OtherWorkHours[SSPCPH$OtherWorkHours==1015] <- 15

SSP <- rbind(SSPAAL,SSPCPH)
SSP[SSP=='-'] <- NA
SSP <- SSP[,-c(2,3,5,6,7,8)]
SSP[,3:114] <- apply(SSP[,3:114],MARGIN = 2, as.numeric)

WeekWork <- SSP$StudyHours+SSP$StudyRelatedWorkHours+SSP$OtherWorkHours
unrealistic <- which(WeekWork>126)
SSP[unrealistic,116:118] <- NA


#sorting questions into categories by summing grades
Qnolab <- questions[,c('Category','QuestionLabel','QuestionNo')]
Qnolab <- Qnolab[!duplicated(Qnolab),]
lab <- Qnolab$QuestionLabel[Qnolab$QuestionNo]
names(SSP)[4:114] <- lab
cat <- Qnolab$Category[!duplicated(Qnolab$Category)]

for (i in 1:length(cat)){
  category <- cat[i]
  col <- names(SSP) %in% Qnolab$QuestionLabel[Qnolab$Category==category]
  SSP[,category] <- rowSums(SSP[,col], na.rm = TRUE)
}

SSP <- SSP[,-(4:114)]
#Adding studynumber for merging
namestudyno <- read.csv("Y:/analysis_data/dropOut/data/bsc.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
namestudyno <- namestudyno[,c(18,19)]
namestudyno <- namestudyno[!duplicated(namestudyno),]
SSPIncStudyNo <- merge(SSP,namestudyno, by.x='Name',by.y='navn')

which(duplicated(SSPIncStudyNo[,-22]))
SSPIncStudyNo <- SSPIncStudyNo[-c(29,33,76,77,103,107,108,110,138),]

#Dropout status without Q999 and other predictors
dropandADGGRU <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/Optag_2017_bac_medialogi_adggru.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
dropandADGGRU <- dropandADGGRU[,-c(2:10,12,20,22)]
#exclude students who didn't show up
dropandADGGRU <- dropandADGGRU[!dropandADGGRU$udmeld_aarsag %in%c('Ikke fremmødt','Ikke accepteret tilbudt plads'),]

#Dropout status with Q999
dropincQ999 <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/dropoutMed2017incQ999.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
names(dropincQ999)[19] <- 'Name'
dropincQ999 <- dropincQ999[-(181:183),-c(1:14)]
Q999inc <- dropincQ999[,c(4,8)]
which(!Q999inc$studienr%in%dropandADGGRU$studienr)

#Using this status when possible
dropData <- merge(dropandADGGRU,Q999inc,by='studienr',all.x = T)
dropData$udmeldsnALL[is.na(dropData$udmeldsnALL)] <- dropData$udmeld_aarsag[is.na(dropData$udmeldsnALL)]
dropData$udmeldsnALL[dropData$udmeldsnALL==''] <- 'Indskrevet'
dropData <- dropData[,-2]

#Merging with SSP
dropSSPADGGRU <- merge(dropData,SSPIncStudyNo,by='studienr',all.x = T)

#Highschool grades and levels
highschool <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/Gymnasiale_fag_karakterer_optag_2017_bac_medialogi.csv", header = TRUE, fill=TRUE, sep = ";", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
highschool$studienr <- as.character(highschool$studienr)
highschool <- highschool[,-c(1,5,6,7)]
test <- aggregate(highschool$KARAKTER, by = list('Studienr'=highschool$studienr,'NIVEAU'=highschool$NIVEAU,'GYMFAG'=highschool$GYMFAG), max)
names(test)[4] <- 'maxKarakter'
test$NIVEAU <- as.numeric(factor(test$NIVEAU))

which(!dropSSPADGGRU$studienr %in% test$Studienr)

test2 <- as.data.table(test)
test2 <- test2[test2[, .I[which.min(NIVEAU)], by=list(Studienr,GYMFAG)]$V1]
test2 <- data.frame(test2)
test2$NIVEAU <- recode(test2$NIVEAU,"1='A';2='B';3='C'")


dataMAT <- test2[test2$GYMFAG=='MAT',]
names(dataMAT)[c(1,2,4)]<- c('studienr','NiveauMAT','GradeMAT')
dataMAT <- dataMAT[,-3]

dataDAN <- test2[test2$GYMFAG=='DAN',]
names(dataDAN)[c(1,2,4)]<- c('studienr','NiveauDAN','GradeDAN')
dataDAN <- dataDAN[,-3]

dataENG <- test2[test2$GYMFAG=='ENG',]
names(dataENG)[c(1,2,4)]<- c('studienr','NiveauENG','GradeENG')
dataENG <- dataENG[,-3]

dataAll <- merge(dropSSPADGGRU,dataMAT,by='studienr',all.x = T)
dataAll <- merge(dataAll,dataDAN,by='studienr',all.x = T)
dataAll <- merge(dataAll,dataENG,by='studienr',all.x = T)

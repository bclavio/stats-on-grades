library(readxl)
library(tidyr)

###########################################
########Loading and preparing data#########
###########################################
dfmidtermmath <- read.table("Y:/MT-March.csv", quote="\"", comment.char="",stringsAsFactors = F)
names(dfmidtermmath) <- c('studienr','scoreMath')

#Getting identification data
bsc <- read_excel("Y:/analysis_data/dropOut/data/bsc.xls")
bsc <- bsc[,c(1,3,18,19)]

#Giving all the students name, start year and education
math <- merge(dfmidtermmath,bsc,by='studienr')
#Removing some students who got in the data twice because they have changed education
math <- math[-c(5,16,17),]


##SSP grades AAL
SSPAAL<-read.csv("Y:/analysis_data/SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPAAL <- SSPAAL[-nrow(SSPAAL),]
names(SSPAAL)[1] <- 'Surname'
SSPAAL <- unite_(SSPAAL,'Name', c("First name","Surname"), sep = ' ')
SSPManualAAL <-read.csv("Y:/analysis_data/SSP/SSPanswersTestAAL 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
SSPManualAAL <- unite_(SSPManualAAL,'Name', c("First name","Surname"), sep = ' ')
which(!SSPAAL$Name%in%SSPManualAAL$Name)
#Removing the one person who is in progress
SSPAAL <- SSPAAL[-91,]
#Check that all observation coinside and insert the values
all.equal(SSPAAL$Name,SSPManualAAL$Name)
SSPAAL[,c('Q. 93 /0.09','Q. 95 /0.09','Q. 96 /0.09')] <- SSPManualAAL[,c('Response 93','Response 95','Response 96')]
#Renaming questions
Q <- rep('Q',111)
num <- 1:111
dat <- data.frame(Q,num)
dat <- unite_(dat,'names', c("Q","num"), sep = '')
names(SSPAAL)[9] <- 'Grade'
names(SSPAAL)[10:120] <- dat[,1]
SSPAAL[,9:120]<- apply(SSPAAL[,9:120],MARGIN = 2,as.numeric)
#Making categories
questions<-read.csv("Y:/analysis_data/SSP/QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
Qnolab <- questions[,c('Category','QuestionLabel','QuestionNo')]
Qnolab <- Qnolab[!duplicated(Qnolab),]
lab <- Qnolab$QuestionLabel[Qnolab$QuestionNo]
SSP <- SSPAAL
names(SSP)[10:120] <- lab
cat <- factor(questions$Category)

for (i in 1:length(levels(cat))){
  category <- levels(cat)[i]
  col <- names(SSP) %in% Qnolab$QuestionLabel[Qnolab$Category==category]
  SSP[,120+i] <- rowMeans(SSP[,col], na.rm = TRUE)
}
names(SSP)[121:134] <- levels(cat)
names(SSP)[10:120]<- names(SSPAAL)[10:120]
#Tidying up
SSP <- SSP[,-c(2,3,5,6,7,8,120)]
#Replacing missing values with mean of colum
length(which(is.na(SSP)))
#Only 5 values
for (i in 3:113){
  SSP[is.na(SSP[,i]),i] <- mean(SSP[,i],na.rm=TRUE)
}


math <- merge(math,SSP,by.x = 'navn',by.y = 'Name', all.x = T)


##GPRO final and MT grades
dfAALMidGrades<-read.csv("Y:/2018_SLERD_Paper_Analysis/Paper/15-02-2018_GPRO-overview-MT-exam.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE,encoding = 'UTF-8')
dfAALMidGrades<-dfAALMidGrades[(dfAALMidGrades$"education"!="CPH"), ]
dfAALMEAGPRO <- dfAALMidGrades[dfAALMidGrades$education=='MEA',]


math <- merge(math,dfAALMEAGPRO,by='studienr',all.x = T); math <- math[,-c(1,2,5,6,132:136,139,140)]
math$startaar[is.na(math$Grade)]
#Removing 5 people who started before 2017 becaus they did not take SSP. Maybe they are retaking the course?
math <- math[!is.na(math$Grade),]
math <- math[,-2]
names(math)[c(2,127,128)] <- c('SSPgrade','GPROscore','GPROgrade')

#Adding passed/failed variable with exam not taken as UB
math$PassFail <- math$scoreMath=='UB'
math$PassFail[math$PassFail] <- 'UB'
math$scoreMath <- as.numeric(math$scoreMath)
math$PassFail[math$scoreMath<15] <- 'fail'
math$PassFail[math$scoreMath>=15] <- 'pass'

#Formatting data
math[,127:128] <- apply(math[,127:128],2,as.numeric)
math$PassFail <- factor(math$PassFail)
###############################################
###################Analysis####################
###############################################

table(math$PassFail)
mean(math$PassFail=='pass')
#only 9 percent passed
attach(math)
plot(scoreMath~GPROscore,data = math)
plot(scoreMath~Q82, data = math)
plot(scoreMath~SSPgrade)
abline(lm(scoreMath~SSPgrade))
plot(GPROscore~PassFail)
plot(Q82~PassFail)
plot(SSPgrade~PassFail)

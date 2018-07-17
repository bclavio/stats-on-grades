library(tidyr)
library(ggplot2)
library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)

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
#Only using students who showed up for the exam
MedDataGradesMath <- MedDataGradesMath[MedDataGradesMath$MMA>=-3,]
#one student is missing kvotient and MAT. Imputing these
MedDataGradesMath$kvotient[is.na(MedDataGradesMath$kvotient)] <- mean(MedDataGradesMath$kvotient,na.rm = T)
MedDataGradesMath$MATGrade[is.na(MedDataGradesMath$MATGrade)] <- mean(MedDataGradesMath$MATGrade,na.rm = T)
MedDataGradesMath$MAT_Niveau[is.na(MedDataGradesMath$MAT_Niveau)] <- 'A'
#Not using English because of many missing values
MedDataGradesMath <- MedDataGradesMath[,-c(16,17)]

################## Both campus #############
####Plots
ggplot(data=MedDataGradesMath,aes(x=kvotient,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=MATGrade,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=MAT_Niveau,y=MMA))+geom_boxplot()+geom_count()
ggplot(data=MedDataGradesMath,aes(x=adgangsgrundlag,y=MMA))+geom_boxplot()+geom_count()
ggplot(data=MedDataGradesMath,aes(x=GPRO,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=AVS,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=PV,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=p1,y=MMA))+geom_count()+geom_smooth(method = 'lm')

#in adgangsgrundlag there are so few students from HHX and HF that these collapsed into one
#previous analyses of dropout have suggested that they are similar and so does plots of MMA grade against adgangsgrundlag
MedDataGradesMath$adgangsgrundlag[MedDataGradesMath$adgangsgrundlag%in%c('HF','HHX')] <- 'HF/HHX'
MedDataGradesMath$adgangsgrundlag[MedDataGradesMath$adgangsgrundlag=='Ny student (påb. 2005 -)'] <- 'STX'
MedDataGradesMath$adgangsgrundlag[MedDataGradesMath$adgangsgrundlag=='Udenlandsk adg.grundlag'] <- 'Udland'

#Using adgangsgrundlag to identify international students
MedDataGradesMath$isInt <- ifelse(MedDataGradesMath$adgangsgrundlag=='Udland','International','Not international')

#############Regression for grade
#####Choose variables with stepwise search
fit.null <- lm(MMA~1,data = MedDataGradesMath)
names(MedDataGradesMath)[29] <- 'selfC'

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMath)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))
add1(fit.null,formNotAAUgrades,test = 'F')

fitBICnoAAU <- step(fit.null,formNotAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(fitBICnoAAU)

fit.inter <- update(fitBICnoAAU,.~.+MATGrade*MAT_Niveau)
summary(fit.inter)
anova(fit.inter,fitBICnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMath)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fit.null,formAAUgrades,test = 'F')
formAAUnotGPRO <- as.formula(paste('~',paste(names(MedDataGradesMath)[c(3,5,7)],collapse = '+')))
add1(fit.null,formAAUnotGPRO,test = 'F')

MedDataGradesMathGPRO <- MedDataGradesMath[!is.na(MedDataGradesMath$GPRO),]
formGPRO <- as.formula(paste('~',paste(names(MedDataGradesMath)[c(6,12:32,36)],collapse = '+')))
fit.null.subset <- lm(MMA~1,data = MedDataGradesMathGPRO)
fitBICAAU <- step(fit.null.subset,formGPRO,k=log(nrow(MedDataGradesMathGPRO)))
summary(fitBICAAU)

#####compare with crossvalidation
CVlm <- function(lmfit,data,k=10){
  idx <- sample(1:k,nrow(data),replace = T)
  form <- formula(lmfit)
  response <- as.character(attributes(lmfit$terms)$variables[[2]])
  CV <- rep(NA,k)
  for( i in 1:k){
    train <- data[idx!=i,]
    test <- data[idx==i,]
    fit <- lm(form,data=train)
    obs <- test[,response]
    pred <- predict(fit,test)
    CV[i] <- rmse(obs,pred)
  }
  return(CV)
}

mean(CVlm(fit.null,data = MedDataGradesMath))
mean(CVlm(fitBICnoAAU,MedDataGradesMath))

mean(CVlm(fit.null.subset,data = MedDataGradesMath))
mean(CVlm(fitBICAAU,MedDataGradesMath))

##############only Aalborg#########
MedDataGradesMathAAL <- MedDataGradesMath[MedDataGradesMath$campus=='AAL',]
MedDataGradesMathAAL$MMAmidterm <- as.numeric(MedDataGradesMathAAL$MMAmidterm)
#####Choose variables with stepwise search
fit.null <- lm(MMA~1,data = MedDataGradesMathAAL)

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,34,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))
add1(fit.null,formNotAAUgrades,test = 'F')

fitBICnoAAU <- step(fit.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitBICnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fit.null,formAAUgrades,test = 'F')
formAAUnotGPRO <- as.formula(paste('~',paste(names(MedDataGradesMathAAL)[c(3,5,7)],collapse = '+')))
add1(fit.null,formAAUnotGPRO,test = 'F')

MedDataGradesMathAALGPRO <- MedDataGradesMathAAL[!is.na(MedDataGradesMathAAL$GPRO),]
formGPRO <- as.formula(paste('~',paste(names(MedDataGradesMathAAL)[c(6,12:32,34,36)],collapse = '+')))
fit.null.subset <- lm(MMA~1,data = MedDataGradesMathAALGPRO)
fitBICAAU <- step(fit.null.subset,formGPRO,k=log(nrow(MedDataGradesMathAALGPRO)))
summary(fitBICAAU)

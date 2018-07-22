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
MedDataBSc2017 <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/MedDataBSc2017_1107NEW2.csv", encoding="ANSI", stringsAsFactors=FALSE)
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
#3 students are missing kvotient and MAT. Imputing these
MedDataGradesMath$kvotient[is.na(MedDataGradesMath$kvotient)] <- mean(MedDataGradesMath$kvotient,na.rm = T)
MedDataGradesMath$MATGrade[is.na(MedDataGradesMath$MATGrade)] <- mean(MedDataGradesMath$MATGrade,na.rm = T)
MedDataGradesMath$MAT_Niveau[is.na(MedDataGradesMath$MAT_Niveau)] <- 'B'
#Not using English because of many missing values
MedDataGradesMath <- MedDataGradesMath[,-c(16,17)]

####Plots
ggplot(data=MedDataGradesMath,aes(x=kvotient,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=MATGrade,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=MAT_Niveau,y=MMA))+geom_boxplot()+geom_count()
ggplot(data=MedDataGradesMath,aes(x=adgangsgrundlag,y=MMA))+geom_boxplot()+geom_count()
ggplot(data=MedDataGradesMath,aes(x=GPRO,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=AVS,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=PV,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=p1,y=MMA))+geom_count()+geom_smooth(method = 'lm')
ggplot(data=MedDataGradesMath,aes(x=campus,y=MMA))+geom_boxplot()+geom_count()
ggplot(data=MedDataGradesMath,aes(x=campus,y=MATGrade))+geom_boxplot()+geom_count()


#in adgangsgrundlag there are so few students from HHX and HF that these collapsed into one
#previous analyses of dropout have suggested that they are similar and so does plots of MMA grade against adgangsgrundlag
MedDataGradesMath$adgangsgrundlag[MedDataGradesMath$adgangsgrundlag%in%c('HF','HHX')] <- 'HF/HHX'
MedDataGradesMath$adgangsgrundlag[MedDataGradesMath$adgangsgrundlag=='Ny student (påb. 2005 -)'] <- 'STX'
MedDataGradesMath$adgangsgrundlag[MedDataGradesMath$adgangsgrundlag=='Udenlandsk adg.grundlag'] <- 'Udland'

#Using adgangsgrundlag to identify international students
MedDataGradesMath$isInt <- ifelse(MedDataGradesMath$adgangsgrundlag=='Udland','International','Not international')

#############Regression for grade
fit.campus <- lm(MMA~campus,data = MedDataGradesMath)
summary(fit.campus)

fit.ADGGRU <- lm(MMA~adgangsgrundlag,data = MedDataGradesMath)
summary(fit.ADGGRU)

#####Choose variables with stepwise search
fit.null <- lm(MMA~1,data = MedDataGradesMath)
names(MedDataGradesMath)[30] <- 'selfC'

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMath)[c(12:33,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))
add1(fit.null,formNotAAUgrades,test = 'F')

fitBICnoAAU <- step(fit.null,formNotAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(fitBICnoAAU)

fit.inter <- update(fitBICnoAAU,.~.+MATGrade*MAT_Niveau)
summary(fit.inter)
anova(fit.inter,fitBICnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMath)[c(3,5:7,12:33,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fit.null,formAAUgrades,test = 'F')

fitBICAAU <- step(fit.null,formAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(fitBICAAU)
fitBICAAUNew <- update(fitBICAAU,.~.-adgangsgrundlag+isInt)
summary(fitBICAAUNew)

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
mean(CVlm(fitBICAAU,MedDataGradesMath))

#############Only Aalborg################

#including midterm

MedDataGradesMathAAL <- MedDataGradesMath[MedDataGradesMath$campus=='AAL',]
MedDataGradesMathAAL <- MedDataGradesMathAAL[MedDataGradesMathAAL$MMAmidterm!='UB',]
MedDataGradesMathAAL$MMAmidterm <- as.numeric(MedDataGradesMathAAL$MMAmidterm)

fitAAL.null <- lm(MMA~1,data = MedDataGradesMathAAL)

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,34,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))
add1(fitAAL.null,formNotAAUgrades,test = 'F')

fitAALBICnoAAU <- step(fitAAL.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fitAAL.null,formAAUgrades,test = 'F')

fitAALBICAAU <- step(fitAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICAAU)
fitAALBICAAUNew <- update(fitAALBICAAU,.~.+MMAmidterm*GPRO)
summary(fitAALBICAAUNew)

#excluding midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))
add1(fitAAL.null,formNotAAUgrades,test = 'F')

fitAALBICnoAAUnoMid <- step(fitAAL.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICnoAAUnoMid)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fitAAL.null,formAAUgrades,test = 'F')

fitAALBICAAUnoMid <- step(fitAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICAAUnoMid)

mean(CVlm(fitAAL.null,data = MedDataGradesMathAAL))
mean(CVlm(fitAALBICnoAAU,MedDataGradesMathAAL))
mean(CVlm(fitAALBICAAU,MedDataGradesMathAAL))
mean(CVlm(fitAALBICnoAAUnoMid,MedDataGradesMathAAL))
mean(CVlm(fitAALBICAAUnoMid,MedDataGradesMathAAL))

#Predicting passed failed with linear models
MedDataGradesMath$MMApassfail <- factor(MedDataGradesMath$MMApassfail,levels = c('passed','failed'))
MedDataGradesMathAAL$MMApassfail <- factor(MedDataGradesMathAAL$MMApassfail,levels = c('passed','failed'))


predLinear <- function(fit,Newdata){
  pred <- predict(fit,Newdata)
  ifelse(pred>=2,'passed','failed')
  }

CVlmpred <- function(fit,data){
CV <- rep(NA,10)
FP <- rep(NA,10)
FN <- rep(NA,10)
idx <- sample(1:10,nrow(data),replace = TRUE)
for (i in 1:10){
  train <- data[idx!=i,]
  test <- data[idx==i,]
  obstest <- test$MMApassfail
  lm <- lm(formula(fit),data=train)
  pred <- predLinear(lm,test)
  CV[i] <- mean(pred==obstest,na.rm=TRUE)
  tab <- table(factor(pred, levels = c('passed','failed')),obstest)
  if(sum(tab[,1])==0){FP[i] <- 0}
  else{FP[i] <- tab[2,1]/sum(tab[,1])}
  if(sum(tab[,2])==0){FN[i] <- 0}
  else{FN[i] <- tab[1,2]/sum(tab[,2])}
}
return(list('accuracy'=CV,'FP'=FP,'FN'=FN))
}

CVfitnoAAU <-  CVlmpred(fitBICnoAAU,MedDataGradesMath)
CVfitAAU <- CVlmpred(fitBICAAU,MedDataGradesMath)

CVfitAALnoAAU <- CVlmpred(fitAALBICnoAAU, MedDataGradesMathAAL)
CVfitAALAAU <- CVlmpred(fitAALBICAAU, MedDataGradesMathAAL)

CVfitAALnoAAUnoMid <- CVlmpred(fitAALBICnoAAUnoMid, MedDataGradesMathAAL)
CVfitAALAAUnoMid <- CVlmpred(fitAALBICAAUnoMid, MedDataGradesMathAAL)

mean(CVfitnoAAU$accuracy)
mean(CVfitAAU$accuracy)

mean(CVfitAALnoAAU$accuracy)
mean(CVfitAALAAU$accuracy)

mean(CVfitAALnoAAUnoMid$accuracy)
mean(CVfitAALAAUnoMid$accuracy)

#Predicting passed failed with logistic regression
#both campi
log.null <- glm(MMApassfail~1,family = binomial,data=MedDataGradesMath)

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMath)[c(12:33,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))

logBICnoAAU <- step(log.null,formNotAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(logBICnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMath)[c(3,5:7,12:33,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))

logBICAAU <- step(log.null,formAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(logBICAAU)

logCV <- function(fit,k=10, data,thres=0.5){
  CV <- rep(0,k)
  FP <- rep(0,k)
  FN <- rep(0,k)
  idx <- sample(1:k,nrow(data),replace = TRUE)
  for (i in 1:k){
    train <- data[idx!=i,]
    test <- data[idx==i,]
    fittrain <- glm(fit$formula, data = train, family = binomial)
    pred <- predict(fittrain,test,type='response')
    pred <-ifelse(pred>thres, 'failed','passed')
    CV[i] <- mean(pred==test$MMApassfail)
    tab <- table(factor(pred, levels = c('passed','failed')),test$MMApassfail)
    if(sum(tab[,1])==0){FP[i] <- 0}
    else{FP[i] <- tab[2,1]/sum(tab[,1])}
    if(sum(tab[,2])==0){FN[i] <- 0}
    else{FN[i] <- tab[1,2]/sum(tab[,2])}
  }
  accuracy <- CV
  FP <- FP
  FN <- FN
  return(list('accuracy'=accuracy,'FP'=FP,'FN'=FN))
}

CVlognoAAU <- logCV(logBICnoAAU,data = MedDataGradesMath)
CVlogAAU <- logCV(logBICAAU,data=MedDataGradesMath)

#Aalborg
logAAL.null <- glm(MMApassfail~1,family=binomial,data = MedDataGradesMathAAL)

#including midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,34,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))

logAALBICnoAAU <- step(logAAL.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))

logAALBICAAU <- step(logAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICAAU)

#excluding midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))

logAALBICnoAAUnoMid <- step(logAAL.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICnoAAUnoMid)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))

logAALBICAAUnoMid <- step(logAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICAAUnoMid)

CVlogAALnoAAU <- logCV(logAALBICnoAAU, data = MedDataGradesMathAAL)
#the same
CVlogAALAAU <- logCV(logAALBICAAU, data = MedDataGradesMathAAL)
CVlogAALnoAAUnoMid <- logCV(logAALBICnoAAUnoMid, data = MedDataGradesMathAAL)
#the same
CVlogAALAAUnoMid <- logCV(logAALBICAAUnoMid, data = MedDataGradesMathAAL)

###Predicting with CART
#both campi
#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMath)[c(12:33,36)]
formNotAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsNotAAUgrades,collapse = '+')))

CARTnoAAU <- rpart(formNotAAUgrades,MedDataGradesMath)
rpart.plot(CARTnoAAU)
plotcp(CARTnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMath)[c(3,5:7,12:33,36)]
formAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsAAUgrades,collapse = '+')))

CARTAAU <- rpart(formAAUgrades,MedDataGradesMath)
rpart.plot(CARTAAU)
plotcp(CARTAAU)

#Aalborg
#including midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,34,36)]
formNotAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsNotAAUgrades,collapse = '+')))

CARTAALnoAAU <- rpart(formNotAAUgrades,MedDataGradesMathAAL)
rpart.plot(CARTAALnoAAU)
plotcp(CARTAALnoAAU)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsAAUgrades,collapse = '+')))

CARTAALAAU <- rpart(formAAUgrades,MedDataGradesMathAAL)
rpart.plot(CARTAALAAU)

#excluding midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsNotAAUgrades,collapse = '+')))

CARTnoAAUnoMid <- rpart(formNotAAUgrades,MedDataGradesMathAAL)
rpart.plot(CARTnoAAUnoMid)
plotcp(CARTnoAAUnoMid)
CARTnoAAUnoMid <- rpart(formNotAAUgrades,MedDataGradesMathAAL,cp=0.17)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsAAUgrades,collapse = '+')))

CARTAALAAUnoMid <- rpart(formAAUgrades,MedDataGradesMathAAL)
rpart.plot(CARTAALAAUnoMid)
plotcp(CARTAALAAUnoMid)
CARTAALAAUnoMid <- rpart(formAAUgrades,MedDataGradesMathAAL,cp=0.2)

#########CV heere
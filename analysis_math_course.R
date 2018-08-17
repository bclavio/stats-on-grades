library(tidyr)
library(ggplot2)
library(glmnet)
library(hydroGOF)
library(rpart)
library(rpart.plot)
library(xtable)

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

table(MedDataGradesMath$adgangsgrundlag)
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

xtable(summary(fitBICnoAAU)$coefficients,digits = 3)

fit.inter <- update(fitBICnoAAU,.~.+MATGrade*MAT_Niveau)
summary(fit.inter)
anova(fit.inter,fitBICnoAAU)

res <- rstandard(fitBICnoAAU)
hist(res)
qqnorm(res)
qqline(res)
plot(res~fitBICnoAAU$fitted.values)
plot(res~MedDataGradesMath$MATGrade)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMath)[c(3,5:7,12:33,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fit.null,formAAUgrades,test = 'F')

fitBICAAU <- step(fit.null,formAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(fitBICAAU)

xtable(summary(fitBICAAU)$coefficients,digits = 3)

fitBICAAUNew <- update(fitBICAAU,.~.-adgangsgrundlag+isInt)
summary(fitBICAAUNew)

res <- rstandard(fitBICAAUNew)
hist(res)
qqnorm(res)
qqline(res)
plot(res~fitBICAAUNew$fitted.values)
plot(res~MedDataGradesMath$GPRO)

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

CV.null <- CVlm(fit.null,data = MedDataGradesMath)
CV.noAAU <- CVlm(fitBICnoAAU,MedDataGradesMath)
CV.AAU <- CVlm(fitBICAAU,MedDataGradesMath)

Model <- factor(c('lm.null','lm.0','lm.1'),levels = c('lm.null','lm.0','lm.1'))
CVmean <- c(mean(CV.null),mean(CV.noAAU),mean(CV.AAU))
se <- 1/sqrt(10)*c(sd(CV.null),sd(CV.noAAU),sd(CV.AAU))
ggplot()+geom_point(aes(Model,CVmean)) + 
  geom_errorbar(aes(x=Model, ymin=CVmean-se, ymax=CVmean+se),width=0.25)+
  ylab('Crossvalidation RMSE')



#############Only Aalborg################
MedDataGradesMathAAL <- MedDataGradesMath[MedDataGradesMath$campus=='AAL',]
MedDataGradesMathAAL <- MedDataGradesMathAAL[MedDataGradesMathAAL$MMAmidterm!='UB',]
MedDataGradesMathAAL$MMAmidterm <- as.numeric(MedDataGradesMathAAL$MMAmidterm)

fitAAL.null <- lm(MMA~1,data = MedDataGradesMathAAL)

#including midterm and AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fitAAL.null,formAAUgrades,test = 'F')

fitAALBICAAU <- step(fitAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICAAU)

xtable(summary(fitAALBICAAU),digits = 3)

fitAALBICAAUNew <- update(fitAALBICAAU,.~.+MMAmidterm*GPRO)
summary(fitAALBICAAUNew)

res <- rstandard(fitAALBICAAU)
hist(res)
qqnorm(res)
qqline(res)
plot(res~fitAALBICAAU$fitted.values)
plot(res~MedDataGradesMathAAL$MMAmidterm)
plot(res~MedDataGradesMathAAL$GPRO)

#excluding midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))
add1(fitAAL.null,formNotAAUgrades,test = 'F')

fitAALBICnoAAUnoMid <- step(fitAAL.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICnoAAUnoMid)

xtable(summary(fitAALBICnoAAUnoMid)$coefficients,digits = 3)

res <- rstandard(fitAALBICnoAAUnoMid)
hist(res)
qqnorm(res)
qqline(res)
plot(res~fitAALBICnoAAUnoMid$fitted.values)
plot(res~MedDataGradesMathAAL$kvotient)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))
add1(fitAAL.null,formAAUgrades,test = 'F')

fitAALBICAAUnoMid <- step(fitAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(fitAALBICAAUnoMid)

xtable(summary(fitAALBICAAUnoMid)$coefficients,digits = 3)

res <- rstandard(fitAALBICAAUnoMid)
hist(res)
qqnorm(res)
qqline(res)
plot(res~fitAALBICAAUnoMid$fitted.values)
plot(res~MedDataGradesMathAAL$GPRO)

CV.AAL.null <- CVlm(fitAAL.null,data = MedDataGradesMathAAL)
CV.AAL.AAU <- CVlm(fitAALBICAAU,MedDataGradesMathAAL)
CV.AAL.noAAUnoMid <- CVlm(fitAALBICnoAAUnoMid,MedDataGradesMathAAL)
CV.AAL.AAUnoMid <- CVlm(fitAALBICAAUnoMid,MedDataGradesMathAAL)

Model <- factor(c('lm.AAL.null','lm.AAL.1.5','lm.AAL.0','lm.AAL.1'),levels = c('lm.AAL.null','lm.AAL.0','lm.AAL.1','lm.AAL.1.5'))
CVmean <- c(mean(CV.AAL.null),mean(CV.AAL.AAU),mean(CV.AAL.noAAUnoMid),mean(CV.AAL.AAUnoMid))
se <- 1/sqrt(10)*c(sd(CV.AAL.null),sd(CV.AAL.AAU),sd(CV.AAL.noAAUnoMid),sd(CV.AAL.AAUnoMid))
ggplot()+geom_point(aes(Model,CVmean)) + 
  geom_errorbar(aes(x=Model, ymin=CVmean-se, ymax=CVmean+se),width=0.25)+
  ylab('Crossvalidation RMSE')

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

CVfitnull <- CVlmpred(fit.null,MedDataGradesMath)
CVfitnoAAU <-  CVlmpred(fitBICnoAAU,MedDataGradesMath)
CVfitAAU <- CVlmpred(fitBICAAU,MedDataGradesMath)

CVfitAALnull <- CVlmpred(fitAAL.null,MedDataGradesMathAAL)
CVfitAALAAU <- CVlmpred(fitAALBICAAU, MedDataGradesMathAAL)
CVfitAALnoAAUnoMid <- CVlmpred(fitAALBICnoAAUnoMid, MedDataGradesMathAAL)
CVfitAALAAUnoMid <- CVlmpred(fitAALBICAAUnoMid, MedDataGradesMathAAL)


#Predicting passed failed with logistic regression
#both campi
log.null <- glm(MMApassfail~1,family = binomial,data=MedDataGradesMath)

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMath)[c(12:33,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))

logBICnoAAU <- step(log.null,formNotAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(logBICnoAAU)

xtable(summary(logBICnoAAU)$coefficients,digits = 3)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMath)[c(3,5:7,12:33,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))

logBICAAU <- step(log.null,formAAUgrades,k=log(nrow(MedDataGradesMath)))
summary(logBICAAU)

xtable(summary(logBICAAU)$coefficients,digits = 3)

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

CVlognull <- logCV(log.null,data=MedDataGradesMath)
CVlognoAAU <- logCV(logBICnoAAU,data = MedDataGradesMath)
CVlogAAU <- logCV(logBICAAU,data=MedDataGradesMath)

#Aalborg
logAAL.null <- glm(MMApassfail~1,family=binomial,data = MedDataGradesMathAAL)

#including midterm

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))

logAALBICAAU <- step(logAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICAAU)

xtable(summary(logAALBICAAU)$coefficients,digits = 3)

#excluding midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('~',paste(predictorsNotAAUgrades,collapse = '+')))

logAALBICnoAAUnoMid <- step(logAAL.null,formNotAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICnoAAUnoMid)

xtable(summary(logAALBICnoAAUnoMid)$coefficients,digits = 3)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('~',paste(predictorsAAUgrades,collapse = '+')))

logAALBICAAUnoMid <- step(logAAL.null,formAAUgrades,k=log(nrow(MedDataGradesMathAAL)))
summary(logAALBICAAUnoMid)
#Same as before

CVlogAALnull <- logCV(logAAL.null,data=MedDataGradesMathAAL)
CVlogAAL <- logCV(logAALBICAAU, data = MedDataGradesMathAAL)
CVlogAALnoMid <- logCV(logAALBICAAUnoMid, data = MedDataGradesMathAAL)

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
CARTAAU <- rpart(formAAUgrades,MedDataGradesMath,cp=0.13)

CVcart <- function(tree,data, k=10){
  accuracy <- rep(0,k)
  FP <- rep(0,k)
  FN <- rep(0,k)
  idx <- sample(1:k,nrow(data),replace = TRUE)
  for (i in 1:k){
    train <- data[idx!=i,]
    test <- data[idx==i,]
    formula <- formula(tree)
    cp <- tree$cptable[nrow(tree$cptable),1]
    fittrain <- rpart(formula, data = train, cp=cp)
    pred <- predict(fittrain,test,type='class')
    accuracy[i] <- mean(pred==test$MMApassfail)
    tab <- table(factor(pred, levels = c('passed','failed')),test$MMApassfail)
    if(sum(tab[,1])==0){FP[i] <- 0}
    else{FP[i] <- tab[2,1]/sum(tab[,1])}
    if(sum(tab[,2])==0){FN[i] <- 0}
    else{FN[i] <- tab[1,2]/sum(tab[,2])}
  }
  return(list('accuracy'=accuracy,'FP'=FP,'FN'=FN))
}

CV.CART.AAU <- CVcart(CARTAAU,MedDataGradesMath)

#Aalborg
#including midterm

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,34,36)]
formAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsAAUgrades,collapse = '+')))

CARTAALAAU <- rpart(formAAUgrades,MedDataGradesMathAAL,cp=0.076)
rpart.plot(CARTAALAAU)
plotcp(CARTAALAAU)

CV.CART.AAL.AAU <- CVcart(CARTAALAAU,MedDataGradesMathAAL)

#excluding midterm

#without AAUgrades
predictorsNotAAUgrades <- names(MedDataGradesMathAAL)[c(12:32,36)]
formNotAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsNotAAUgrades,collapse = '+')))

CARTnoAAUnoMid <- rpart(formNotAAUgrades,MedDataGradesMathAAL)
rpart.plot(CARTnoAAUnoMid)
plotcp(CARTnoAAUnoMid)
CARTnoAAUnoMid <- rpart(formNotAAUgrades,MedDataGradesMathAAL,cp=0.17)

CV.CART.AAL.noAAUnoMid <- CVcart(CARTnoAAUnoMid,MedDataGradesMathAAL)

#with AAUgrades
predictorsAAUgrades <- names(MedDataGradesMathAAL)[c(3,5:7,12:32,36)]
formAAUgrades <- as.formula(paste('MMApassfail ~',paste(predictorsAAUgrades,collapse = '+')))

CARTAALAAUnoMid <- rpart(formAAUgrades,MedDataGradesMathAAL)
rpart.plot(CARTAALAAUnoMid)
plotcp(CARTAALAAUnoMid)
CARTAALAAUnoMid <- rpart(formAAUgrades,MedDataGradesMathAAL,cp=0.2)

CV.CART.AAL.AAUnoMid <- CVcart(CARTAALAAUnoMid,MedDataGradesMathAAL)

###Comparing with cross validation
#Both campi
Model <- factor(c('lm.null','lm.0','lm.1','log.null','log.0','log.1','CART.1'),levels = c('lm.null','lm.0','lm.1','log.null','log.0','log.1','CART.1'))
accuracy <- c(mean(CVfitnull$accuracy),mean(CVfitnoAAU$accuracy),mean(CVfitAAU$accuracy),mean(CVlognull$accuracy),mean(CVlognoAAU$accuracy),mean(CVlogAAU$accuracy),mean(CV.CART.AAU$accuracy))
se <- (1/sqrt(10))*c(sd(CVfitnull$accuracy),sd(CVfitnoAAU$accuracy),sd(CVfitAAU$accuracy),sd(CVlognull$accuracy),sd(CVlognoAAU$accuracy),sd(CVlogAAU$accuracy),sd(CV.CART.AAU$accuracy))
ggplot()+geom_point(aes(Model,accuracy)) + 
  geom_errorbar(aes(x=Model, ymin=accuracy-se, ymax=accuracy+se),width=0.25)

FP <- c(mean(CVfitnull$FP),mean(CVfitnoAAU$FP),mean(CVfitAAU$FP),mean(CVlognull$FP),mean(CVlognoAAU$FP),mean(CVlogAAU$FP),mean(CV.CART.AAU$FP))
se <- (1/sqrt(10))*c(sd(CVfitnull$FP),sd(CVfitnoAAU$FP),sd(CVfitAAU$FP),sd(CVlognull$FP),sd(CVlognoAAU$FP),sd(CVlogAAU$FP),sd(CV.CART.AAU$FP))
ggplot()+geom_point(aes(Model,FP)) + 
  geom_errorbar(aes(x=Model, ymin=FP-se, ymax=FP+se),width=0.25)

FN <- c(mean(CVfitnull$FN),mean(CVfitnoAAU$FN),mean(CVfitAAU$FN),mean(CVlognull$FN),mean(CVlognoAAU$FN),mean(CVlogAAU$FN),mean(CV.CART.AAU$FN))
se <- (1/sqrt(10))*c(sd(CVfitnull$FN),sd(CVfitnoAAU$FN),sd(CVfitAAU$FN),sd(CVlognull$FN),sd(CVlognoAAU$FN),sd(CVlogAAU$FN),sd(CV.CART.AAU$FN))
ggplot()+geom_point(aes(Model,FN)) + 
  geom_errorbar(aes(x=Model, ymin=FN-se, ymax=FN+se),width=0.25)

#AAlborg
Model <- factor(c('lm.AAL.null','lm.AAL.1.5','lm.AAL.0','lm.AAL.1','log.AAL.null','log.AAL.1.5','log.AAL.0-1','CART.AAL.1.5','CART.AAL.0','CART.AAL.1'),levels = c('lm.AAL.null','lm.AAL.0','lm.AAL.1','lm.AAL.1.5','log.AAL.null','log.AAL.0-1','log.AAL.1.5','CART.AAL.0','CART.AAL.1','CART.AAL.1.5'))
accuracy <- c(mean(CVfitAALnull$accuracy),mean(CVfitAALAAU$accuracy),mean(CVfitAALnoAAUnoMid$accuracy),mean(CVfitAALAAUnoMid$accuracy),mean(CVlogAALnull$accuracy),mean(CVlogAAL$accuracy),mean(CVlogAALnoMid$accuracy),mean(CV.CART.AAL.AAU$accuracy),mean(CV.CART.AAL.noAAUnoMid$accuracy),mean(CV.CART.AAL.AAUnoMid$accuracy))
se <- (1/sqrt(10))*c(sd(CVfitAALnull$accuracy),sd(CVfitAALAAU$accuracy),sd(CVfitAALnoAAUnoMid$accuracy),sd(CVfitAALAAUnoMid$accuracy),sd(CVlogAALnull$accuracy),sd(CVlogAAL$accuracy),sd(CVlogAALnoMid$accuracy),sd(CV.CART.AAL.AAU$accuracy),sd(CV.CART.AAL.noAAUnoMid$accuracy),sd(CV.CART.AAL.AAUnoMid$accuracy))
ggplot()+geom_point(aes(Model,accuracy)) + 
  geom_errorbar(aes(x=Model, ymin=accuracy-se, ymax=accuracy+se),width=0.25)

FP <- c(mean(CVfitAALnull$FP),mean(CVfitAALAAU$FP),mean(CVfitAALnoAAUnoMid$FP),mean(CVfitAALAAUnoMid$FP),mean(CVlogAALnull$FP),mean(CVlogAAL$FP),mean(CVlogAALnoMid$FP),mean(CV.CART.AAL.AAU$FP),mean(CV.CART.AAL.noAAUnoMid$FP),mean(CV.CART.AAL.AAUnoMid$FP))
se <- (1/sqrt(10))*c(sd(CVfitAALnull$FP),sd(CVfitAALAAU$FP),sd(CVfitAALnoAAUnoMid$FP),sd(CVfitAALAAUnoMid$FP),sd(CVlogAALnull$FP),sd(CVlogAAL$FP),sd(CVlogAALnoMid$FP),sd(CV.CART.AAL.AAU$FP),sd(CV.CART.AAL.noAAUnoMid$FP),sd(CV.CART.AAL.AAUnoMid$FP))
ggplot()+geom_point(aes(Model,FP)) + 
  geom_errorbar(aes(x=Model, ymin=FP-se, ymax=FP+se),width=0.25)

FN <- c(mean(CVfitAALnull$FN),mean(CVfitAALAAU$FN),mean(CVfitAALnoAAUnoMid$FN),mean(CVfitAALAAUnoMid$FN),mean(CVlogAALnull$FN),mean(CVlogAAL$FN),mean(CVlogAALnoMid$FN),mean(CV.CART.AAL.AAU$FN),mean(CV.CART.AAL.noAAUnoMid$FN),mean(CV.CART.AAL.AAUnoMid$FN))
se <- (1/sqrt(10))*c(sd(CVfitAALnull$FN),sd(CVfitAALAAU$FN),sd(CVfitAALnoAAUnoMid$FN),sd(CVfitAALAAUnoMid$FN),sd(CVlogAALnull$FN),sd(CVlogAAL$FN),sd(CVlogAALnoMid$FN),sd(CV.CART.AAL.AAU$FN),sd(CV.CART.AAL.noAAUnoMid$FN),sd(CV.CART.AAL.AAUnoMid$FN))
ggplot()+geom_point(aes(Model,FN)) + 
  geom_errorbar(aes(x=Model, ymin=FN-se, ymax=FN+se),width=0.25)


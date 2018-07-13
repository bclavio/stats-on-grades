library(RMySQL)
library(tidyr)
library(ggplot2)


MedDataBSc2017 <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/MedDataBSc2017_1107.csv", encoding="ANSI", stringsAsFactors=FALSE)

libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep='')) # MAC and Windows
mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver)
rs<-dbSendQuery(mydb, "call SSPWide()")
SSPWide<-fetch(rs, n=-1)
dbClearResult(dbListResults(mydb)[[1]])
dbDisconnect(mydb)

SSPAAL<-read.csv("Y:/analysis_data/SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPCPH <- read.csv("Y:/analysis_data/SSP/SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSP <- rbind(SSPAAL,SSPCPH)
names(SSP)[1] <- 'Surname'
SSP <- unite_(SSP,'Name', c("First name","Surname"), sep = ' ')
SSP <- SSP[,c(1,4)]
which(!SSPWide$email %in% SSP$`Email address`)
namestudyno <- read.csv("Y:/analysis_data/dropOut/data/bsc.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
namestudyno <- namestudyno[,c(18,19)]
namestudyno <- namestudyno[!duplicated(namestudyno),]
SSPIncStudyNo <- merge(SSP,namestudyno, by.x='Name',by.y='navn')
studienrEmail <- SSPIncStudyNo[,-1]
names(studienrEmail)[1] <- 'email'

SSPWide <- merge(SSPWide,studienrEmail)

MedDataBSc2017 <- merge(MedDataBSc2017,SSPWide,by='studienr',all.x = T)
MedDataBSc2017$dropout <- ifelse(MedDataBSc2017$udmeld_aarsag!=''|MedDataBSc2017$Q999==1,'dropout','active')
MedData0 <- MedDataBSc2017[,-c(1:2,6:20,23:34,42:182)]

MedData0[MedData0==''] <- NA
MedData0[MedData0=='.'] <- NA

MedData0$merit_staa[is.na(MedData0$merit_staa)] <- 0

#Removing one students with many missing predictors
MedData0 <- MedData0[-13,]

#Setting unrealistic working hours to NA
MedData0$nonsAct[MedData0$nonsAct>60] <- NA

which(is.na(MedData0[,-(7:12)]))
#only 24 missing values (excluding highschool subjects). Replacing numeric with average and categorical with most common
for (i in c(1:6,13:ncol(MedData0))) {
  if(any(is.na(MedData0[,i]))){
    if(class(MedData0[,i])=='numeric'){
      MedData0[is.na(MedData0[,i]),i] <- mean(MedData0[,i],na.rm = T)
    }
    else{
      tab <- table(MedData0[,i])
      MedData0[is.na(MedData0[,i]),i] <- names(tab[which.max(tab)])
    }
  }
}
MedData0$dropout <- factor(MedData0$dropout)
MedData0$CourseLocation <- factor(MedData0$CourseLocation)

##########Analysis##############
#There are many NA's in ENG and DAN but because people have not necesarrily taken the subjects
#(international not taking danish)
#it does not make much sense to impute these values.
#Therefore it is investigated wether they are significant.
fit.null <- glm(dropout~1,family = binomial(link='logit'), data = MedData0)
add1(fit.null,~DANGrade+ENGGrade+ENG_Niveau,test='LRT')
#None of them seems to be significant so they are excluded from further analysis
MedData0 <- MedData0[,-c(9:12)]

#There are only four students missing mathgrade. Checking whether math is significant
add1(fit.null,~MATGrade+MAT_Niveau,test='LRT')
#Grades seems to be significant, so keeping MAT. Assuming that all students must have
#had math on some level these values are imputed as before.
MedData0$MATGrade[is.na(MedData0$MATGrade)] <- mean(MedData0$MATGrade,na.rm = T)
MedData0$MAT_Niveau[is.na(MedData0$MAT_Niveau)] <- 'B'

#Investigating what is significant, now using all the data because no NA's
full <- glm(factor(dropout)~., family = binomial(link='logit'), data = MedData0)
formula <- formula(full)

add1(fit.null,formula,test='LRT')
#The only ones that stands out as significant are MATGrade and pAcadAbil
fit <- glm(dropout~MATGrade+pAcadAbil, family = binomial(link='logit'), data = MedData0)
summary(fit)
drop1(fit,test = 'LRT')

fitBIC <- step(fit.null,formula,k=log(nrow(MedData0)))
summary(fitBIC)

fitAIC <- step(fit.null,formula,k=2)
summary(fitAIC)

ggplot(data=MedData0)+geom_boxplot(aes(x=dropout,y=pAcadAbil))
ggplot(data=MedData0)+geom_boxplot(aes(x=dropout,y=MATGrade))

#Check whether the relationship is linear
plot(MedData0$pAcadAbil)
c <- cut(MedData0$pAcadAbil,4)
fitc <- glm(dropout~c, family = binomial(link='logit'), data = MedData0)
summary(fitc)
plot(c(0.045,0.063,0.081),c(0.3567,-0.1613,-0.8283))
#linear connection seems to be fine

#Doing cross-validation
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
    pred <-ifelse(pred>thres, 'dropout','active')
    CV[i] <- mean(pred==test$dropout)
    tab <- table(factor(pred, levels = c('active','dropout')),test$dropout)
    FP[i] <- tab[2,1]/sum(tab[,1])
    if(sum(tab[,2])==0){FN[i] <- 0}
    else{FN[i] <- tab[1,2]/sum(tab[,2])}
  }
  accuracy <- CV
  FP <- FP
  FN <- FN
  return(list('accuracy'=accuracy,'FP'=FP,'FN'=FN))
}

CVBIC <- logCV(fitBIC,k=10,data=MedData0)
mean(CVBIC$accuracy)
mean(CVBIC$FN)
mean(CVBIC$FP)

CVAIC <- logCV(fitAIC,k=10,data=MedData0,thres = 0.3)
mean(CVAIC$accuracy)
mean(CVAIC$FN)
mean(CVAIC$FP)

thresholds <- seq(0,0.5,0.01)
res <- lapply(thresholds, logCV,fit=fitBIC,data=MedData0,k=10)

f <- function(list){c(mean(list$accuracy),sd(list$accuracy),mean(list$FN),sd(list$FN),mean(list$FP),sd(list$FP))}
res2 <- sapply(res, f)

ggplot()+geom_point(aes(x=thresholds,y=res2[1,],col='accuracy'))+
  geom_point(aes(x=thresholds,y=res2[3,],col='FN'))+
  geom_point(aes(x=thresholds,y=res2[5,],col='FP'))+
  geom_line(aes(x=thresholds,y=res2[1,],col='accuracy'))+
  geom_line(aes(x=thresholds,y=res2[3,],col='FN'))+
  geom_line(aes(x=thresholds,y=res2[5,],col='FP'))+
  xlab('Threshold')+
  ylab('Value')+
  scale_color_discrete(name='Meassure')+
  ggtitle('Accuracy, false postive and false negative rates from 10-fold crossvalidation')

resAIC <- lapply(thresholds, logCV,fit=fitAIC,data=MedData0,k=10)

res2AIC <- sapply(resAIC, f)

ggplot()+geom_point(aes(x=thresholds,y=res2AIC[1,],col='accuracy'))+
  geom_point(aes(x=thresholds,y=res2AIC[3,],col='FN'))+
  geom_point(aes(x=thresholds,y=res2AIC[5,],col='FP'))+
  geom_line(aes(x=thresholds,y=res2AIC[1,],col='accuracy'))+
  geom_line(aes(x=thresholds,y=res2AIC[3,],col='FN'))+
  geom_line(aes(x=thresholds,y=res2AIC[5,],col='FP'))+
  xlab('Threshold')+
  ylab('Value')+
  scale_color_discrete(name='Meassure')+
  ggtitle('Accuracy, false postive and false negative rates from 10-fold crossvalidation')

summary(fitAIC)
summary(fitBIC)

normpAcadAbil <- (MedData0$pAcadAbil-min(MedData0$pAcadAbil))/(max(MedData0$pAcadAbil)-min(MedData0$pAcadAbil))
fitBICnorm <- glm(dropout~normpAcadAbil,family = binomial(link='logit'), data = MedData0)
summary(fitBICnorm)

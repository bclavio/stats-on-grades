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

#Removing one students without many missing predictors
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

##########Analysis##############
full <- glm(factor(dropout)~., family = binomial(link='logit'), data = MedData0)
null <- glm(factor(dropout)~1,family = binomial(link='logit'), data = MedData0)
formula <- formula(full)

add1(null,formula,test='LRT')

fit <- step(full,k=4)
summary(fit)

fit2 <- glm(factor(dropout)~kvotient,family = binomial(link='logit'), data = MedData0)
summary(fit2)
fit2$deviance-null$deviance

hist(MedData0$merit_staa)
table(MedData0$merit_staa,MedData0$dropout)

library(rpart)
library(rpart.plot)
cart <- rpart(dropout~adgangsgrundlag,MedData0,minsplit=2)
rpart.plot(cart)
summary(cart)
table(MedData0$kommune_nu)

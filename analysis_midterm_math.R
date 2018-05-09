library(readxl)
library(tidyr)
library(glmnet)
library(leaps)

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

math$GPROPassFail <- math$GPROgrade=='U'|math$GPROgrade=='EB'
math$GPROPassFail[math$GPROPassFail] <- 'UB'
math$GPROgrade <- as.numeric(math$GPROgrade)
math$GPROPassFail[math$GPROgrade<2] <- 'fail'
math$GPROPassFail[math$GPROgrade>=2] <- 'pass'


#Formatting data
math$GPROscore <- as.numeric(math$GPROscore) 
math$PassFail <- factor(math$PassFail)
math$GPROPassFail <- factor(math$GPROPassFail)
###############################################
###################Analysis####################
###############################################

table(math$PassFail)
mean(math$PassFail=='pass')
#only 9 percent passed
plot(scoreMath~GPROscore,data = math)
plot(scoreMath~Q82, data = math)
plot(scoreMath~SSPgrade, data = math)
abline(lm(scoreMath~SSPgrade), data = math)
plot(GPROscore~PassFail, data = math)
plot(Q82~PassFail, data = math)
plot(SSPgrade~PassFail, data = math)

#############Lasso regression for predicting MATHscore############
#making data with individual questions and categories and removing students with no score in midterm (didn't show up)
MATHSSPQ <- math[,c(1:112,127,128)]
MATHSSPQ <- MATHSSPQ[!is.na(MATHSSPQ$scoreMath),]
length(which(is.na(MATHSSPQ$GPROgrade)))
#Six people have not taken exam in programming so in order to use this as predictor they have to be removed as well
#(alternatively they could be coded as something but that would not shot their skills if they didn't take the exam)
MATHSSPQ <- MATHSSPQ[!is.na(MATHSSPQ$GPROgrade),]
any(is.na(MATHSSPQ))

MATHSSPcat <- math[,c(1,113:128)]
MATHSSPcat <- MATHSSPcat[!is.na(MATHSSPcat$scoreMath),]
MATHSSPcat <- MATHSSPcat[!is.na(MATHSSPcat$GPROgrade),]
any(is.na(MATHSSPcat))

x <- as.matrix(MATHSSPQ[,-1])
y <- MATHSSPQ$scoreMath
CV.lambda <- cv.glmnet(x,y,family='gaussian')
plot(CV.lambda)
CV.lambda$lambda.1se
#1.796185
coef(CV.lambda,s=1.796185)
#only Q82 and GPROgrade
lmQ.post.lasso <- lm(scoreMath~Q82+GPROgrade, data = MATHSSPQ)
summary(lmQ.post.lasso)
confint(lmQ.post.lasso)
table(factor(MATHSSPQ$Q82))
#there are not many people in the extreme answers


x <- as.matrix(MATHSSPcat[,-1])
y <- MATHSSPcat$scoreMath
CV.lambda <- cv.glmnet(x,y,family='gaussian')
plot(CV.lambda)
CV.lambda$lambda.1se
#2.225688
coef(CV.lambda,s=2.225688)
#only GPROgrade
lmcat.post.lasso <- lm(scoreMath~GPROgrade, data = MATHSSPcat)
summary(lmcat.post.lasso)
confint(lmcat.post.lasso)
#The importance of Q82 seems to be lost when averaging over categories

##############Best subset selection#######################
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

x <- as.matrix(MATHSSPQ[,-1])
y <- MATHSSPQ$scoreMath
subsets <- regsubsets(x,y,nvmax = 10,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names <- names(MATHSSPQ)[-1]

CVsub <- rep(NA,nrow(variables))
sesub <- rep(NA,nrow(variables))
AICsub <- rep(NA,nrow(variables))
BICsub <- rep(NA,nrow(variables))
for(i in 1:nrow(variables)){
  dat <- MATHSSPQ[,c('scoreMath',names[variables[i,]])]
  lm <- lm(scoreMath~.,data=dat)
  CV <- CVlm(lm,dat)
  CVsub[i] <- mean(CV)
  sesub[i] <- (1/sqrt(10))*sd(CV)
  AICsub[i] <- AIC(lm)
  BICsub[i] <- AIC(lm,k=log(nrow(MATHSSPQ)))
}

ggplot()+geom_point(aes(1:20,AICsub,col='AIC'))+geom_line(aes(1:20,AICsub,col='AIC'))+geom_point(aes(1:20,BICsub,col='BIC'))+geom_line(aes(1:20,BICsub,col='BIC'))+guides(colour=guide_legend(title=''))+ylab('Værdi')+xlab('Antal prædiktorer')+scale_x_continuous(breaks = 1:20)

ggplot()+geom_point(aes(1:20,CVsub)) + geom_errorbar(aes(x=1:20, ymin=CVsub-sesub, ymax=CVsub+sesub),width=0.25) + geom_hline(yintercept=CVsub[which.min(CVsub)]+sesub[which.min(CVsub)], linetype='dashed')+scale_x_continuous(breaks=1:20)+xlab('Antal prædiktorer')+ylab('Krydsvalideringsscore')

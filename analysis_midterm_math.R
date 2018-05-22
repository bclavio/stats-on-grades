library(readxl)
library(tidyr)
library(glmnet)
library(leaps)
library(hydroGOF)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)
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
math$PassFail[math$scoreMath<12.5] <- 'fail'
math$PassFail[math$scoreMath>=12.5] <- 'pass'

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
#only 16 percent passed
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

lmnull <- lm(scoreMath~1,data=MATHSSPQ)

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
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which[,-1]
names <- names(MATHSSPQ)[-1]
names[variables[1,]]
names[variables[2,]]
names[variables[3,]]
names[variables[4,]]
names[variables[5,]]


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

CVnull <- CVlm(lmnull,MATHSSPQ)
CVsub <- c(mean(CVnull),CVsub)
sesub <- c((1/sqrt(10))*sd(CVnull),sesub)
AICsub <- c(AIC(lmnull),AICsub)
BICsub <- c(AIC(lmnull,k=log(nrow(MATHSSPQ))),BICsub)
ggplot()+geom_point(aes(0:5,AICsub,col='AIC'))+geom_line(aes(0:5,AICsub,col='AIC'))+geom_point(aes(0:5,BICsub,col='BIC'))+geom_line(aes(0:5,BICsub,col='BIC'))+guides(colour=guide_legend(title=''))+ylab('Value')+xlab('Number of predictors')+scale_x_continuous(breaks = 0:5)

ggplot()+geom_point(aes(0:5,CVsub)) + geom_errorbar(aes(x=0:5, ymin=CVsub-sesub, ymax=CVsub+sesub),width=0.25) + geom_hline(yintercept=CVsub[which.min(CVsub)]+sesub[which.min(CVsub)], linetype='dashed')+scale_x_continuous(breaks=0:5)+xlab('Number of predictors')+ylab('Cross validation score')
#The addition of one predictor makes a clear improvement to the null model but no more than one seems to be needed

dat5 <- MATHSSPQ[,c('scoreMath',names[variables[5,]])]
lm5 <- lm(scoreMath~.,data=dat)
summary(lm5)
confint(lm5)
#Q82 has the clearest effect


x <- as.matrix(MATHSSPcat[,-1])
y <- MATHSSPcat$scoreMath
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which[,-1]
names <- names(MATHSSPcat)[-1]
names[variables[1,]]
names[variables[2,]]
names[variables[3,]]
names[variables[4,]]
names[variables[5,]]


CVsubcat <- rep(NA,nrow(variables))
sesubcat <- rep(NA,nrow(variables))
AICsubcat <- rep(NA,nrow(variables))
BICsubcat <- rep(NA,nrow(variables))
for(i in 1:nrow(variables)){
  dat <- MATHSSPcat[,c('scoreMath',names[variables[i,]])]
  lm <- lm(scoreMath~.,data=dat)
  CV <- CVlm(lm,dat)
  CVsubcat[i] <- mean(CV)
  sesubcat[i] <- (1/sqrt(10))*sd(CV)
  AICsubcat[i] <- AIC(lm)
  BICsubcat[i] <- AIC(lm,k=log(nrow(MATHSSPcat)))
}

CVsubcat <- c(mean(CVnull),CVsubcat)
sesubcat <- c((1/sqrt(10))*sd(CVnull),sesubcat)
AICsubcat <- c(AIC(lmnull),AICsubcat)
BICsubcat <- c(AIC(lmnull,k=log(nrow(MATHSSPcat))),BICsubcat)
ggplot()+geom_point(aes(0:5,AICsubcat,col='AIC'))+geom_line(aes(0:5,AICsubcat,col='AIC'))+geom_point(aes(0:5,BICsubcat,col='BIC'))+geom_line(aes(0:5,BICsubcat,col='BIC'))+guides(colour=guide_legend(title=''))+ylab('Value')+xlab('Number of predictors')+scale_x_continuous(breaks = 0:5)

ggplot()+geom_point(aes(0:5,CVsubcat)) + geom_errorbar(aes(x=0:5, ymin=CVsubcat-sesubcat, ymax=CVsubcat+sesubcat),width=0.25) + geom_hline(yintercept=CVsubcat[which.min(CVsubcat)]+sesubcat[which.min(CVsubcat)], linetype='dashed')+scale_x_continuous(breaks=0:5)+xlab('Number of predictors')+ylab('Cross validation score')
#no clear improvements to the null model but choosing the one with one predictor. This is the same as lasso

dat5 <- MATHSSPcat[,c('scoreMath',names[variables[5,]])]
lm5 <- lm(scoreMath~.,data=dat)
summary(lm5)
confint(lm5)

##############Cart regression tree################
treeQ <- rpart(scoreMath~.,data = MATHSSPQ)
rpart.plot(treeQ)
plotcp(treeQ)
#Should choose cp=0.17
treeQ <- rpart(scoreMath~.,data = MATHSSPQ,cp=0.17)
rpart.plot(treeQ)
#only Q82
CVtree <- rep(NA,10)
idx <- sample(1:10,nrow(MATHSSPQ),replace = TRUE)
for (i in 1:10){
  train <- MATHSSPQ[idx!=i,]
  test <- MATHSSPQ[idx==i,]
  tree <- rpart(scoreMath~., data = train,cp=0.17)
  pred <- predict(tree,test)
  CVtree[i] <- rmse(pred,test$scoreMath)
}
CVtree

#############Comparison with cross validation############
CVlassoQ <- CVlm(lmQ.post.lasso,MATHSSPQ)
CVlassoCat <- CVlm(lmcat.post.lasso,MATHSSPcat)

Model <- c('PostLassoQ','PostLassoCat','lmQ1','lmnull','treeQ')
CVscore <- c(mean(CVlassoQ),mean(CVlassoCat),CVsub[2],mean(CVnull),mean(CVtree))
k <- 1/sqrt(10)
se <- c(k*sd(CVlassoQ),k*sd(CVlassoCat),sesub[2],k*sd(CVnull),k*sd(CVtree))
ggplot()+geom_point(aes(Model,CVscore)) + geom_errorbar(aes(x=Model, ymin=CVscore-se, ymax=CVscore+se),width=0.25) + geom_hline(yintercept=CVscore[which.min(CVscore)]+se[which.min(CVscore)], linetype='dashed')
#Coose either lmQ1, PostLassoQ or treeQ

res <- residuals(lmQ.post.lasso)
plot(res)
hist(res)
qqnorm(res)
qqline(res)
plot(res~lmQ.post.lasso$fitted.values)

coef <- summary(lmQ.post.lasso)$coefficients
conf <- confint(lmQ.post.lasso)
estimates <- cbind(coef,conf)
xtable(estimates)
###############trying to predict passed failed###############
####With cart###########
MATHSSPQpf <- math[,c(2:112,127:130)]
MATHSSPcatpf <- math[,c(113:130)]

treepfQ <- rpart(PassFail~.,data = MATHSSPQpf)
rpart.plot(treepfQ)
plotcp(treepfQ)
#the empty tree is just as good

#####With linear models#########
predlmpf <- function(lm,data){
  predscore <- predict(lm,data)
  passfail <- predscore<12.5
  pred <- rep(NA,nrow(data))
  pred <- ifelse(passfail,'fail','pass')
  return(pred)
}
pred <- predlmpf(lmQ.post.lasso,MATHSSPQ)
obs <- math$PassFail[!is.na(math$scoreMath)&!is.na(math$GPROgrade)]
table(pred,obs)

obs <- factor(MATHSSPQpf$PassFail[!is.na(math$scoreMath)&!is.na(math$GPROgrade)],levels = c('pass','fail'))
CVlmpred <- rep(NA,10)
FP <- rep(NA,10)
FN <- rep(NA,10)
idx <- sample(1:10,nrow(MATHSSPQ),replace = TRUE)
for (i in 1:10){
  train <- MATHSSPQ[idx!=i,]
  test <- MATHSSPQ[idx==i,]
  obstest <- obs[idx==i]
  lm <- lm(formula(lmQ.post.lasso),data=train)
  pred <- predlmpf(lm,test)
  CVlmpred[i] <- mean(pred==obstest,na.rm=TRUE)
  tab <- table(factor(pred, levels = c('pass','fail')),obstest)
  if(sum(tab[,1])==0){FP[i] <- 0}
  else{FP[i] <- tab[2,1]/sum(tab[,1])}
  if(sum(tab[,2])==0){FN[i] <- 0}
  else{FN[i] <- tab[1,2]/sum(tab[,2])}
}
(accuracy <- mean(CVlmpred))
sda <- sd(CVlmpred)
(FPlmpred <- mean(FP))
sdFPlmpred <- sd(FP)
(FNlmpred <- mean(FN))
sdFNlmpred <- sd(FN)
#That may be okay
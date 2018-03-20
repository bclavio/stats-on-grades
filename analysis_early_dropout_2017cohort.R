library(tidyr)
library(glmnet)
library(rpart)
library(rpart.plot)
library(ggplot2)

############################################
#########Loading and preparing data ########
############################################
#Loading data from fileshare
dropoutFeb18 <- read.csv("Y:/analysis_data/dropOut/data/dropout-Feb2018-Q999.csv", encoding="UTF-8",stringsAsFactors=FALSE)
names(dropoutFeb18)[1] <- 'Name'

questions<-read.csv("Y:/analysis_data/SSP/QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
SSPAAL<-read.csv("Y:/analysis_data/SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPCPH <- read.csv("Y:/analysis_data/SSP/SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPAAL$Campus <- 'AAL'
SSPCPH$Campus <- 'CPH'
SSPAAL <- SSPAAL[-nrow(SSPAAL),]
SSPCPH <- SSPCPH[-nrow(SSPCPH),]
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
SSPAAL[,c('Q. 93 /0.09','Q. 95 /0.09','Q. 96 /0.09')] <- SSPManualAAL[,c('Response 93','Response 95','Response 96')]

SSPManualCPH <-read.csv("Y:/analysis_data/SSP/SSPanswersTestCPH 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")
SSPManualCPH <- unite_(SSPManualCPH,'Name', c("First name","Surname"), sep = ' ')
#Check that all observation coinside and insert the values
all.equal(SSPCPH$Name,SSPManualCPH$Name)
SSPCPH[,c('Q. 93 /0.09','Q. 95 /0.09','Q. 96 /0.09')] <- SSPManualCPH[,c('Response 93','Response 95','Response 96')]
#There is one student who has reported 1015 in Q96. That must be a mistake and since
#the answers to Q93 and Q95 are 15 this value is used instead (Possibly meant 10-15)
SSPCPH$`Q. 96 /0.09`[SSPCPH$`Q. 96 /0.09`==1015] <- 15

SSP <- rbind(SSPAAL,SSPCPH)
names(SSP)[4] <- 'Email'
#Renaming questions
Q <- rep('Q',111)
num <- 1:111
dat <- data.frame(Q,num)
dat <- unite_(dat,'names', c("Q","num"), sep = '')
names(SSP)[9] <- 'Grade'
names(SSP)[10:120] <- dat[,1]

SSP$Name[which(!SSP$Name%in%dropoutFeb18$Name)]
#There seems to be one person who have taken SSP but are not in dropout data
dropoutFeb18$Name[which(!dropoutFeb18$Name%in%SSP$Name)]
# There seems to be three people in dropout data who have not taken SSP

#Merge dropout and SSP
SSPDropout <- merge(dropoutFeb18,SSP, by='Name', all.x = TRUE)
SSPDropout[SSPDropout==''] <- NA
SSPDropout[SSPDropout=='-'] <- NA
SSPDropout$Email.x[is.na(SSPDropout$Email.x)] <-SSPDropout$Email.y[is.na(SSPDropout$Email.x)]
SSPDropout <- SSPDropout[,-(6:8)]
SSPDropout$Campus.x[is.na(SSPDropout$Campus.x)] <- SSPDropout$Campus.y[is.na(SSPDropout$Campus.x)]
SSPDropout <- SSPDropout[,-122]
names(SSPDropout)[4] <- 'Campus'
table(SSPDropout$Status,SSPDropout$Campus)

#In the analysis Q999 is treated as dropout
SSPDropout$Status[SSPDropout$Status=='Q999'] <- 'dropout'
#Excluding unnecesarry columns and formatting data
SSPDropout$Status <- factor(SSPDropout$Status)
SSPDropout <- SSPDropout[,-c(1,2,5,7:9)]
SSPDropout[,4:115] <- apply(SSPDropout[,4:115],MARGIN = 2,as.numeric)

#Making another dataset with questions sorted into categories
SSP <- SSPDropout[!is.na(SSPDropout$State),]
Qnolab <- questions[,c('Category','QuestionLabel','QuestionNo')]
Qnolab <- Qnolab[!duplicated(Qnolab),]
lab <- Qnolab$QuestionLabel[Qnolab$QuestionNo]
names(SSP)[5:115] <- lab
cat <- factor(questions$Category)

for (i in 1:length(levels(cat))){
  category <- levels(cat)[i]
  col <- names(SSP) %in% Qnolab$QuestionLabel[Qnolab$Category==category]
  SSP[,115+i] <- rowMeans(SSP[,col], na.rm = TRUE)
}
names(SSP)[116:129] <- levels(cat)

SSPQuestions <- SSP[,c(1,5:115)]
names(SSPQuestions)[2:112] <- names(SSPDropout)[5:115]
SSPQuestions <- SSPQuestions[,-ncol(SSPQuestions)]
#Remove last question (comments) because it is always the same
SSPCategory <- SSP[,c(1,116:129)]
which(is.na(SSPQuestions))
###Replace 10 missing values in SSPQuestions with mean of column
for (i in 2:ncol(SSPQuestions)){
  SSPQuestions[is.na(SSPQuestions[,i]),i] <- mean(SSPQuestions[,i],na.rm=TRUE)
}

######################################
#########Analysis#####################
######################################

#Checking whether campus has any influence on dropout
logCampus <- glm(Status~Campus, family = binomial(link='logit'), data = SSPDropout)
summary(logCampus)
drop1(logCampus, test = 'Chisq')
#Does not seem to have any influence

####Shifting focus to SSP questions

#Making some different logistic regression models
loggrade <- glm(Status~Grade, family = binomial(link = 'logit'), data = SSP)
summary(loggrade)
#Overall grade does not seem to be significanat
boxplot(Grade~Status, data=SSP)

#Looking at categories average
cat <- names(SSPCategory[2:15])
for (i in 1:length(cat)){
  boxplot(SSPCategory[,cat[i]]~SSPCategory$Status, main=cat[i])
}
#There are not any predictors who clearly stand out as promising

logfullcat <- glm(Status~., data = SSPCategory, family=binomial(link = 'logit'))
summary(logfullcat)
drop <- drop1(logfullcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
as.data.frame(drop)[maxp,]
logcat <- update(logfullcat,~.-`High School Behaviour`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-Grit)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Growth Mindset`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Belonging Uncertainty`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Personal Trait Comparison`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`High School Trust`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`View on Medialogy`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-Demographics)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Reasons for Going to University`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Self-control`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Attitude Towards Education` )

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Education Choice Factors`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Studying and Working Hours`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]

summary(logcat)
#Indicate that perceived academic abilities is the only signifikant one

#Try to chose the model with AIC and BIC
catAIC <- step(logfullcat, direction = 'both')
summary(catAIC)
catBIC <- step(logfullcat, k=log(nrow(SSPCategory)), direction = 'both')
summary(catBIC)
#AIC indicate that working hours could be interesting as well but it does not seem
#to be significant

#####Looking into single questions
q <- names(SSPQuestions[2:111])
for (i in 1:length(q)){
  boxplot(SSPQuestions[,q[i]]~SSPQuestions$Status, main=q[i])
}
#There are very little dispersion for some questions. Maybe they should be treated as factors instead of numeric

#Adding single term one by one and check whether they are significant
fitQ <- glm(Status~1,data = SSPQuestions,family = binomial(link='logit'))
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]
#Q7,Q9,Q10,Q25,Q88,Q92,Q96,Q98,Q106,Q107,Q108 are significant with level 0.05
#The most significant one is added and the test is repeated for the new model
fitQ <- update(fitQ,~.+Q10)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]
#Q92,Q96,Q98,Q107,Q108 are still significant after correcting for Q10
#ading the one with smallest p and repeating
fitQ <- update(fitQ,~.+Q96)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q98)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q92)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q108)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q73)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q88)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q65)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q4)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q14)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

fitQ <- update(fitQ,~.+Q32)
add <- add1(fitQ, scope = names(SSPQuestions)[2:111],test = 'Chisq')
add[which(add$`Pr(>Chi)`<0.05),]

summary(fitQ)
drop1(fitQ, test='Chisq')
#Q10 no longer significant
fitQ <- update(fitQ,~.-Q10)
###

###Using lasso to choose
x <- as.matrix(SSPQuestions[,-1])
y <- SSPQuestions[,1]

lassofit <- glmnet(x,y,family = 'binomial')
plot(lassofit, xvar='lambda')
CV.lambda <- cv.glmnet(x,y,family='binomial')
plot(CV.lambda)
CV.lambda$lambda.min
CV.lambda$lambda.1se

coef(CV.lambda, s='lambda.1se')
#All variables are disregarded
coef(CV.lambda, s='lambda.min')
#Q108,Q107,Q98,Q96,Q92,Q88,Q10.

#Fit logistic regression model with none predictors and one with Q108,Q107,Q98,Q96,Q92,Q88,Q10
lognull <- glm(Status~1,data=SSPQuestions,family = binomial)
logpostlasso <- glm(Status~Q10+Q88+Q92+Q96+Q98+Q107+Q108, data = SSPQuestions, family = binomial)


###Trying decision trees
tree <- rpart(Status~., data = SSPQuestions)
rpart.plot(tree)
tree$variable.importance
#Indicate Q108,Q88,Q16,Q65 are important
plotcp(tree)
#suggest not to use anything

tree <- rpart(Status~., data = SSPCategory)
rpart.plot(tree)
tree$variable.importance
#Indicate perceived academic abilities and styduing and working hours are important
plotcp(tree)
#Also indicate not to use any predictors

######Croos validation for all candidate models

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
    CV[i] <- mean(pred==test$Status)
    tab <- table(factor(pred, levels = c('active','dropout')),test$Status)
    FP[i] <- tab[2,1]/sum(tab[,1])
    if(sum(tab[,2])==0){FN[i] <- 0}
    else{FN[i] <- tab[1,2]/sum(tab[,2])}
  }
  sda <- sd(CV)
  accuracy <- mean(CV)
  sdFP <- sd(FP)
  FP <- mean(FP)
  sdFN <- sd(FN)
  FN <- mean(FN)
  return(list('accuracy'=accuracy,'sda'=sda,'FP'=FP,'sdFP'=sdFP,'FN'=FN,'sdFN'=sdFN))
}

CVlognull <- logCV(lognull,data = SSPQuestions)
CVlogpostlass <- logCV(logpostlasso, data = SSPQuestions)
CVlogfitQ <- logCV(fitQ, data = SSPQuestions)
CVlogcat <- logCV(logcat, data=SSPCategory)
CVlogcatAIC <- logCV(catAIC, data = SSPCategory)

CVtreeQ <- rep(0,10)
FP <- rep(0,10)
FN <- rep(0,10)
idx <- sample(1:10,nrow(SSPQuestions),replace = TRUE)
for (i in 1:10){
  train <- SSPQuestions[idx!=i,]
  test <- SSPQuestions[idx==i,]
  tree <- rpart(Status~., data = train)
  pred <- predict(tree,test,type = 'class')
  CVtreeQ[i] <- mean(pred==test$Status)
  tab <- table(factor(pred, levels = c('active','dropout')),test$Status)
  FP[i] <- tab[2,1]/sum(tab[,1])
  if(sum(tab[,2])==0){FN[i] <- 0}
  else{FN[i] <- tab[1,2]/sum(tab[,2])}
}
accuracytreeQ <- mean(CVtreeQ)
sdatreeQ <- sd(CVtreeQ)
FPtreeQ <- mean(FP)
sdFPtreeQ <- sd(FP)
FNtreeQ <- mean(FN)
sdFNtreeQ <- sd(FN)

CVtreeCat <- rep(0,10)
FP <- rep(0,10)
FN <- rep(0,10)
idx <- sample(1:10,nrow(SSPCategory),replace = TRUE)
for (i in 1:10){
  train <- SSPCategory[idx!=i,]
  test <- SSPCategory[idx==i,]
  tree <- rpart(Status~., data = train)
  pred <- predict(tree,test,type = 'class')
  CVtreeCat[i] <- mean(pred==test$Status)
  tab <- table(factor(pred, levels = c('active','dropout')),test$Status)
  FP[i] <- tab[2,1]/sum(tab[,1])
  if(sum(tab[,2])==0){FN[i] <- 0}
  else{FN[i] <- tab[1,2]/sum(tab[,2])}
}
accuracytreeCat <- mean(CVtreeCat)
sdatreeCat <- sd(CVtreeCat)
FPtreeCat <- mean(FP)
sdFPtreeCat <- sd(FP)
FNtreeCat <- mean(FN)
sdFNtreeCat <- sd(FN)

CVlasso <- rep(0,10)
FP <- rep(0,10)
FN <- rep(0,10)
idx <- sample(1:10,nrow(SSPQuestions), replace=T)
for (i in 1:10){
  xtrain <- as.matrix(SSPQuestions[idx!=i,-1])
  ytrain <- SSPQuestions[idx!=i,1]
  xtest <- as.matrix(SSPQuestions[idx==i,-1])
  ytest <- SSPQuestions[idx==i,1]
  fit <- glmnet(xtrain, ytrain, family = 'binomial', lambda = 0.05942334)
  pred <- predict(fit,xtest, type='class')
  CVlasso[i] <- mean(pred==ytest)
  tab <- table(factor(pred, levels = c('active','dropout')),ytest)
  FP[i] <- tab[2,1]/sum(tab[,1])
  if(sum(tab[,2])==0){FN[i] <- 0}
  else{FN[i] <- tab[1,2]/sum(tab[,2])}
}
accuracylasso <- mean(CVlasso)
sdalasso <- sd(CVlasso)
FPlasso <- mean(FP)
sdFPlasso <- sd(FP)
FNlasso <- mean(FN)
sdFNlasso <- sd(FN)


Model <- c('lognull', 'logpostlasso','lasso','fitQ','logcat','catAIC','treeQ','treeCat')
ac <- c(CVlognull$accuracy,CVlogpostlass$accuracy,accuracylasso,CVlogfitQ$accuracy,CVlogcat$accuracy,CVlogcatAIC$accuracy,accuracytreeQ,accuracytreeCat)
sdac <- c(CVlognull$sda,CVlogpostlass$sda,sdalasso,CVlogfitQ$sda,CVlogcat$sda,CVlogcatAIC$sda,sdatreeQ,sdatreeCat)
FP <- c(CVlognull$FP,CVlogpostlass$FP,FPlasso,CVlogfitQ$FP,CVlogcat$FP,CVlogcatAIC$FP,FPtreeQ,FPtreeCat)
sdFP <- c(CVlognull$sdFP,CVlogpostlass$sdFP,sdFPlasso,CVlogfitQ$sdFP,CVlogcat$sdFP,CVlogcatAIC$sdFP,sdFPtreeQ,sdFPtreeCat)
FN <- c(CVlognull$FN,CVlogpostlass$FN,FNlasso,CVlogfitQ$FN,CVlogcat$FN,CVlogcatAIC$FN,FNtreeQ,FNtreeCat)
sdFN <- c(CVlognull$sdFN,CVlogpostlass$sdFN,sdFNlasso,CVlogfitQ$sdFN,CVlogcat$sdFN,CVlogcatAIC$sdFN,sdFNtreeQ,sdFNtreeCat)
Parametre <- c(1,7,7,10,1,2,5,3)
(ggplot()+geom_point(aes(Model,ac, size=Parametre,col='accuracy')) + geom_errorbar(aes(x=Model, ymin=ac-sdac, ymax=ac+sdac),width=0.25) 
 +geom_point(aes(Model,FP, size=Parametre,col='FP')) + geom_errorbar(aes(x=Model, ymin=FP-sdFP, ymax=FP+sdFP),width=0.25)
+geom_point(aes(Model,FN, size=Parametre,col='FN')) + geom_errorbar(aes(x=Model, ymin=FN-sdFN, ymax=FN+sdFN),width=0.25))

#fitQ and logpostlasso seems okay.
# Created by Ninna Vihrs 2018
# Modified by Bianca Clavio Christensen 2018

library(tidyr)
library(glmnet)
library(leaps)
library(hydroGOF)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)
library(gridExtra) # plotting cost function
library(pROC)
library(SDMTools)
library(gridExtra)
library(grid)
library(lattice)
library(pROC)

#library(NMF)
#library(bigmemory)
#library(caret)


############################################
#######preparing and loading data###########
############################################
setwd('Z:/BNC/PBL development project/data/2018_SLERD_Data_Analysis_Journal/Paper/') #('Y:/2018_SLERD_Paper_Analysis/Paper')

##SSP grades AAL
SSPAAL<-read.csv("SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
SSPAAL <- SSPAAL[-nrow(SSPAAL),]
names(SSPAAL)[1] <- 'Surname'
SSPAAL <- unite_(SSPAAL,'Name', c("First name","Surname"), sep = ' ')
SSPManualAAL <-read.csv("SSPanswersTestAAL 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="ANSI")

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
questions<-read.csv("QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
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

##GPRO final and MT grades
dfAALMidGrades<-read.csv("15-02-2018_GPRO-overview-MT-exam.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE,encoding = 'UTF-8')
dfCPHMidGrades<-dfAALMidGrades[(dfAALMidGrades$"education"=="CPH"), ]
dfAALMidGrades<-dfAALMidGrades[(dfAALMidGrades$"education"!="CPH"), ]
colnames(dfAALMidGrades)[1] <- "ID number"
colnames(dfCPHMidGrades)[1] <- "ID number"

# Load SA grades
dfSAGrades<-read.csv("15-02-2018_GPRO-gradebook.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE,encoding='UTF-8')
dfSAGrades[,7:21] <- apply(dfSAGrades[,7:21],MARGIN = 2,as.numeric)
saTest<-dfSAGrades[,c(7,8,9,10,11,12,13,15,16,17,18,19)]
saRowAvg<-rowMeans(saTest, na.rm = TRUE, dims = 1)
saTest[!is.na(saTest)] <- 1
saTest[is.na(saTest)] <- 0
saNumComp<-rowSums(saTest)
dfSAGrades<-dfSAGrades[,c(6,7,8,9,10,11,12,13,15,16,17,18,19)]
dfSAGrades<-cbind(dfSAGrades,saRowAvg,saNumComp)
dfSAGrades<-dfSAGrades[!grepl("tmpuser",dfSAGrades$'Email address'),]
dfSAGrades<-dfSAGrades[,c(1,14,15)]
#Students with no average set to zero
dfSAGrades$saRowAvg[is.na(dfSAGrades$saRowAvg)] <- 0

# Load Peer Grade grades
dfPeerGrade<-read.csv("15-02-2018_Peer_Grade_course_overall_stats_data.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE,encoding = 'UTF-8')
AALpeerGrade<-dfPeerGrade[,c('student_email','Submission score','feedback score','combined score')]
# change column name to match others
colnames(AALpeerGrade)[1] <- c("Email address")

# Khan
dfKhan<-read.csv("15-02-2018_Khan_data.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding = 'UTF-8')
dfKhanID<-read.csv("all_merge.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE,encoding = 'UTF-8')
s <- strsplit(dfKhan$"Not Started", split = "\n")
x<-data.frame(V1 = rep(dfKhan$"Exercise", sapply(s, length)), V2 = unlist(s))
x$"Not Started" <- rep(0,nrow(x))
colnames(x) <- c("Exercise", "Student Name","Completed")
# Started column
s <- strsplit(dfKhan$"Started", split = "\n")
y<-data.frame(V1 = rep(dfKhan$"Exercise", sapply(s, length)), V2 = unlist(s))
y$"Started" <- rep(1,nrow(y))
colnames(y) <- c("Exercise", "Student Name","Completed")
# Completed column
s <- strsplit(dfKhan$"Completed", split = "\n")
z<-data.frame(V1 = rep(dfKhan$"Exercise", sapply(s, length)), V2 = unlist(s))
z$"Completed" <- rep(2,nrow(z))
colnames(z) <- c("Exercise", "Student Name", "Completed")
# Join columns and reshape to wide format
joinedKhan<-rbind(x,y,z)
joinedKhan<-reshape(joinedKhan, idvar = "Student Name", timevar = "Exercise", direction = "wide")
# Remove duplicates
# Not sure how a student user name could occur more than once but it did in Fall 2017
joinedKhan<-joinedKhan[!duplicated(joinedKhan[,1]),]
# Remove Teachers
joinedKhan<-joinedKhan[!grepl("hendrik",joinedKhan$'Student Name'),]
# Count number of completed Khan assignments for each student and store in "KhanCount" column
# Like SA, Khan Academny assignments were nto mandatory so we consider number of attempts instead of assigning zeros to non-attempts.
joinedKhan$"KhanCount" <- apply(joinedKhan, 1, function(x) sum(x==2))

# Merge all data sources created above

# SA and Midterm
saTest<-merge(dfKhanID,dfSAGrades,by="Email address")
dfComplete<-merge(saTest,dfAALMidGrades,by="ID number")

# Midterm and Khan
dfComplete<-merge(dfComplete,joinedKhan,by="Student Name",all.x=TRUE)
# Shorten
dfComplete<-dfComplete[,-(14:120)]
#Not in khan data get coutns 0
dfComplete[is.na(dfComplete)]<-0
# Peer Grade 
dfComplete<-merge(dfComplete,AALpeerGrade,by='Email address',all.x=TRUE)
# remove students retaking the course. Peer Grade was not required for
# these students and none of them did it
dfComplete<-dfComplete[complete.cases(dfComplete), ]
# SSP
dfComplete<-merge(dfComplete,SSP,by='Email address',all.x=TRUE)

########Making additional dataset with dropout status#########
dropoutFeb18 <- read.csv("dropout-Feb2018-Q999.csv", encoding="UTF-8",stringsAsFactors=FALSE)
names(dropoutFeb18)[1] <- 'Name'
names(dropoutFeb18)[5] <- 'ID number'
names(dropoutFeb18)[2] <- 'Email address'
dropoutFeb18 <- dropoutFeb18[,c(2,3,5)]
dropoutFeb18[dropoutFeb18==''] <- NA
test <- merge(dfComplete,dropoutFeb18,by=c('ID number'))
test <- test[,-c(1:3,18,144)]
test2 <- merge(dfComplete,dropoutFeb18,by='Email address')
test2 <- test2[,-c(1:3,18,145)]
dfdropoutgrade <-  rbind(test2,test)
####################################################
# Remove all incomplete entries leaves only 77 students
# This removes all PDP students
dfComplete<-dfComplete[complete.cases(dfComplete), ]
#Remove students who didn't take the exam
dfComplete$Exam <- as.numeric(dfComplete$Exam)
dfComplete$'mid-term' <- as.numeric(dfComplete$'mid-term')
dfComplete <- dfComplete[!is.na(dfComplete$Exam),]
#Add pass/fail variable fail=1
dfComplete$grade <- as.numeric(dfComplete$grade)
#dfComplete$PassFail <- factor(as.numeric(dfComplete$grade<2))
dfComplete$PassFail <- factor(as.numeric(dfComplete$Exam >= 54)) # where 1 is passed and 0 is failed

#Deleting unnecesarry colums
dfComplete <- dfComplete[,-c(1,2,3,6,8,10,18)]
names(dfComplete)[c(3,9:12)] <- c('midterm','PeerSubmissionScore','PeerFeedbackScore','PeerCombinedScore','SSPGrade')
names(dfComplete)[123:136] <- c('AttitudeTowardsEducation', "BelongingUncertainty","Demographics", "EducationChoiceFactors", "Grit" ,"GrowthMindset","HighSchoolBehaviour","HighSchoolTrust","PerceivedAcademicAbilities","PersonalTraitComparison","ReasonsforGoingtoUniversity","Selfcontrol","StudyingandWorkingHours","ViewonMedialogy")

# normalizes avg grades
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfComplete$Total <- as.numeric(dfComplete$Total)
dfComplete$Tutoring <- as.numeric(dfComplete$Tutoring)
dfComplete$PassFail <- as.numeric(dfComplete$PassFail)
dfComplete <- as.data.frame(lapply(dfComplete, normalize))


#######################################################
############ Training and test samples ################
#######################################################

## 75% of the sample size (72)
# dfCompleteOLD <- dfComplete
# smp_size <- floor(0.75 * nrow(dfComplete))
# 
# ## set the seed to make your partition reproducible
# set.seed(123)
# train_ind <- sample(seq_len(nrow(dfComplete)), size = smp_size)
# 
# dfComplete <- dfCompleteOLD[train_ind, ] # 54
# dfCompleteTest <- dfCompleteOLD[-train_ind, ] # 18

####################################
############Analysis################
####################################
mean(dfComplete$PassFail==1)
#34.7 percent failed the course
# plot(dfComplete$Exam~dfComplete$saNumComp)
# plot(dfComplete$Exam~dfComplete$saRowAvg)
# plot(dfComplete$Exam~dfComplete$midterm)
# #There seems to be a tendency
# plot(dfComplete$Exam~dfComplete$KhanCount)
# #There could be a tendency
# plot(dfComplete$Exam~dfComplete$PeerSubmissionScore)
# plot(dfComplete$Exam~dfComplete$PeerFeedbackScore)
# plot(dfComplete$Exam~dfComplete$PeerCombinedScore)
# plot(dfComplete$Exam~dfComplete$SSPGrade)
# plot(dfComplete$Exam~factor(dfComplete$Q33))
# plot(dfComplete$Exam~factor(dfComplete$Q78))
# 
# #If only interested in passed failed
# plot(dfComplete$saNumComp~factor(dfComplete$PassFail))
# plot(dfComplete$saRowAvg~factor(dfComplete$PassFail))
# plot(dfComplete$midterm~factor(dfComplete$PassFail))
# plot(dfComplete$KhanCount~factor(dfComplete$PassFail))
# plot(dfComplete$PeerSubmissionScore~factor(dfComplete$PassFail))
# plot(dfComplete$PeerFeedbackScore~factor(dfComplete$PassFail))
# plot(dfComplete$PeerCombinedScore~factor(dfComplete$PassFail))
# plot(dfComplete$SSPGrade~factor(dfComplete$PassFail))
# plot(dfComplete$Q33~factor(dfComplete$PassFail))
# plot(dfComplete$Q78~factor(dfComplete$PassFail))

##########################################
########Analysis with linear models#######
##########################################
#Choosing with lasso (single questions and other predictors)
x <- as.matrix(dfComplete[,c(1:3,8:11,13:122)])
y <- dfComplete$Exam
lasso <- glmnet(x,y,family = 'gaussian')
plot(lasso,label = T)
CV.lasso <- cv.glmnet(x,y,family='gaussian')
plot(CV.lasso)
# Changes each time due to crossvalidation randomness: CV.lasso$lambda.min, CV.lasso$lambda.1se
coef(CV.lasso,s=CV.lasso$lambda.min)
#OLD with s=3.858913: Q82,Q22,KhanCount,midterm # NOW: Q107, Q82, Q46, PeerCombinedScore, midterm, (Intercept)= -4.36009747
#NEW with s=2.302452: Q107, Q102, Q99, Q82, Q61, Q56, Q46, Q22, Q6, PeerCombinedScore, KhanCount, midterm (Intercept)= -16.76775275
coef(CV.lasso, s=CV.lasso$lambda.1se)
#OLD with s=6.74356: Only midterm #NOW: Q82, midterm, (Intercept)=18.0135222
#NEW with s=5.837555: Q82, midterm (Intercept) = 12.8336376

post.lasso.min <- lm(Exam~midterm+PeerCombinedScore+Q107+Q82+Q46, data=dfComplete)
post.lasso.1se <- lm(Exam~midterm+Q82, data=dfComplete)
anova(post.lasso.min,post.lasso.1se)
#Even though lasso does not choose them some seems to be significant anyway

#Choosing with lasso (categories and other predictors)
x <- as.matrix(dfComplete[,c(1:3,8:11,123:136)])
y <- dfComplete$Exam
lasso.cat <- glmnet(x,y,family = 'gaussian')
plot(lasso.cat,xvar = 'lambda',label = T)
CV.lasso.cat <- cv.glmnet(x,y,family='gaussian')
plot(CV.lasso.cat)
CV.lasso.cat$lambda.min 
#0.7935902
# NEW: 2.412085
CV.lasso.cat$lambda.1se
#3.203738
# NEW: 6.71177
coef(CV.lasso.cat,s=CV.lasso.cat$lambda.min)
#Self-control,reasons for going to university, persona trai comparison, perceived academic abilities
#high school trust,grit, education choice factors, demographics,PeerCombinedScore,Peersubmissionscore
#khancount, midterm, SaNumComp
##That is simply too many to be relevant
coef(CV.lasso.cat, s=CV.lasso.cat$lambda.1se)
#Only midterm





######Using best subset selection
#single questions
x <- as.matrix(dfComplete[,c(1:3,8:11,13:122)])
y <- dfComplete$Exam
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names(dfComplete[,c(1:3,8:11,13:122)])[variables[1,-1]]
names(dfComplete[,c(1:3,8:11,13:122)])[variables[2,-1]]
names(dfComplete[,c(1:3,8:11,13:122)])[variables[3,-1]]
names(dfComplete[,c(1:3,8:11,13:122)])[variables[4,-1]]
names(dfComplete[,c(1:3,8:11,13:122)])[variables[5,-1]]
# lmfit1 <- lm(Exam~midterm,data=dfComplete)
# lmfit2 <- lm(Exam~midterm+Q82,data=dfComplete)
# lmfit3 <- lm(Exam~midterm+KhanCount+Q82,data=dfComplete)
# lmfit4 <- lm(Exam~midterm+Q56+Q82+Q107,data=dfComplete)
# lmfit5 <- lm(Exam~midterm+Q22+Q56+Q82+Q107,data=dfComplete)

lmfit1 <- lm(Exam~midterm,data=dfComplete)
lmfit2 <- lm(Exam~midterm+Q82,data=dfComplete)
lmfit3 <- lm(Exam~midterm+Q46+Q82,data=dfComplete)
lmfit4 <- lm(Exam~midterm+Q46+Q61+Q82,data=dfComplete)
lmfit5 <- lm(Exam~midterm+Q46+Q61+Q82+Q107,data=dfComplete)



#Categories
x <- as.matrix(dfComplete[,c(1:3,8:11,123:136)])
y <- dfComplete$Exam
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names(dfComplete[,c(1:3,8:11,123:136)])[variables[1,-1]]
names(dfComplete[,c(1:3,8:11,123:136)])[variables[2,-1]]
names(dfComplete[,c(1:3,8:11,123:136)])[variables[3,-1]]
names(dfComplete[,c(1:3,8:11,123:136)])[variables[4,-1]]
names(dfComplete[,c(1:3,8:11,123:136)])[variables[5,-1]]

# lmcatfit1 <- lm(Exam~midterm, data=dfComplete)
# lmcatfit2 <- lm(Exam~midterm+KhanCount, data=dfComplete)
# lmcatfit3 <- lm(Exam~midterm+KhanCount+HighSchoolTrust, data=dfComplete)
# lmcatfit4 <- lm(Exam~midterm+KhanCount+HighSchoolTrust+Selfcontrol, data=dfComplete)
# lmcatfit5 <- lm(Exam~midterm+KhanCount+HighSchoolTrust+PersonalTraitComparison+Selfcontrol, data=dfComplete)

lmcatfit1 <- lm(Exam~midterm, data=dfComplete)
lmcatfit2 <- lm(Exam~midterm+PeerCombinedScore, data=dfComplete)
lmcatfit3 <- lm(Exam~midterm+PeerCombinedScore+HighSchoolTrust, data=dfComplete)
lmcatfit4 <- lm(Exam~midterm+PeerCombinedScore+HighSchoolTrust+Selfcontrol, data=dfComplete)
lmcatfit5 <- lm(Exam~midterm+PeerCombinedScore+HighSchoolTrust+PersonalTraitComparison+Selfcontrol, data=dfComplete)

#########Forwards and backwards selection
##Questions
#Which are even significant
xdata <- dfComplete[,c(1:4,8:11,13:122)]
scope <- terms(Exam~.,data = xdata)
lmnull <- lm(Exam~1,data=xdata)
add <- add1(lmnull,scope = scope,test = 'Chisq')
#Significane level 0.05
add[which(add$`Pr(>Chi)`<0.05),]
#Significance level 0.01
add[which(add$`Pr(>Chi)`<0.01),]

#Forwards and backwards selection
#Sig.level 0.05
lmfit05 <- step(lmnull,scope=scope,direction='both',k=CV.lasso$lambda.min)
#OLD: midterm,Q82,KhanCount,Q60,Q33,Q107,Q50,Q20,Q7,Q66,Q22,Q34,Q92,Q9,Q86,Q54,Q4,Q80,Q99,Q17, Q108, Q76, Q31
#NEW: midterm, Q82, Q46, Q61, Q107, Q22, Q94, Q56, Q75, Q79, Q102, Q36, Q4, Q100, Q5, Q104, Q91, Q39, Q85, Q69, Q99, Q67, saNumComp, Q71, Q57, Q54, Q23, Q103, Q6, Q89, Q24, Q43, Q93, Q19, Q26, Q8, Q16, Q34, Q40, Q83, Q64, Q7, Q18, Q25, Q17, Q33, Q27, Q12, Q14, Q28, Q108, Q68, saRowAvg
#Sig.level 0.01
lmfit01 <- step(lmnull,scope=scope,direction='both',k=CV.lasso$lambda.1se)
#OLD: midterm, Q82, KhanCount, Q60 
#NEW: midterm, Q82, Q46, Q61, Q107, Q22, Q94, Q56, Q75, Q79, Q102

##Categories
xdata <- dfComplete[,c(1:4,8:11,123:136)]
scope <- terms(Exam~.,data = xdata)
lmnull <- lm(Exam~1,data=xdata)
add <- add1(lmnull,scope = scope,test = 'Chisq')
#Significane level 0.05
add[which(add$`Pr(>Chi)`<0.05),]
#Significance level 0.01
add[which(add$`Pr(>Chi)`<0.01),]

#Forwards and backwards selection
#Sig.level 0.05
lmfitcat05 <- step(lmnull,scope=scope,direction='both',k=CV.lasso.cat$lambda.min)
#OLD: midterm,KhanCount,HighSchoolTrust,Selfcontrol,PersonalTraitComparison,ReasonsforGoingtoUniversity,Demographics
#NEW: midterm, PeerCombinedScore, HighSchoolTrust, Selfcontrol, PersonalTraitComparison, ReasonsforGoingtoUniversity, KhanCount
#Sig.level 0.01
lmfitcat01 <- step(lmnull,scope=scope,direction='both',k=CV.lasso.cat$lambda.1se)
#OLD: midterm, KhanCount
#NEW: midterm
#Doing crossvalidation for all models
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
#post.lasso.1selmfit1 and lmcatfit1 are the same model so only one

CVpostlassomin <- CVlm(post.lasso.min,dfComplete)
CVpostlasso1se <- CVlm(post.lasso.1se,dfComplete)
CVsub2 <- CVlm(lmfit2,dfComplete)
CVsub3 <- CVlm(lmfit3,dfComplete)
CVsub4 <- CVlm(lmfit4,dfComplete)
CVsub5 <- CVlm(lmfit5,dfComplete)
CVsubcat2 <- CVlm(lmcatfit2,dfComplete)
CVsubcat3 <- CVlm(lmcatfit3,dfComplete)
CVsubcat4 <- CVlm(lmcatfit4,dfComplete)
CVsubcat5 <- CVlm(lmcatfit5,dfComplete)
CVstep05 <- CVlm(lmfit05,dfComplete)
CVstep01 <- CVlm(lmfit01,dfComplete)
CVcatstep05 <- CVlm(lmfitcat05,dfComplete)
CVcatstep01 <- CVlm(lmfitcat01,dfComplete)

null.model <- lm(formula = Exam ~ 1, data = dfComplete)
CVnull <- CVlm(null.model,dfComplete)

Model <- c('nullmodel','lasso','lmQ2','lmQ3','lmQ4','lmQ5','lmcat2','lmcat3','lmcat4','lmcat5')
CVmean <- c(mean(CVnull),mean(CVpostlasso1se),mean(CVsub2),mean(CVsub3),mean(CVsub4),mean(CVsub5),mean(CVsubcat2),mean(CVsubcat3),mean(CVsubcat4),mean(CVsubcat5))
se <- (1/sqrt(10))*c(sd(CVnull),sd(CVpostlasso1se),sd(CVsub2),sd(CVsub3),sd(CVsub4),sd(CVsub5),sd(CVsubcat2),sd(CVsubcat3),sd(CVsubcat4),sd(CVsubcat5))
Parametre <- c(1,2,3,4,5,6,3,4,5,6)
(ggplot()+ylab('Cross validation score')+geom_point(aes(Model,CVmean, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=CVmean-se, ymax=CVmean+se),width=0.25) 
  +scale_colour_discrete(name="Measure")+scale_size_continuous(name="Parameters",breaks = seq(1,8,1)))
#lmQ5 is very stable in the bottom

xtable(AIC(null.model,post.lasso.1se,lmfit2,lmfit3,lmfit4,lmfit5,lmcatfit2,lmcatfit3,lmcatfit4,lmcatfit5))
#It also has the lowest AIC
xtable(AIC(null.model,post.lasso.1se,lmfit2,lmfit3,lmfit4,lmfit5,lmcatfit2,lmcatfit3,lmcatfit4,lmcatfit5,k=log(nrow(dfComplete))))
#And BIC
xtable(summary(lmfit5)$coefficients)
#Everything is significant
xtable(confint(lmfit5))

###############################################################
##Trying to predict passed failed with classification tree
# Qdatapf <- dfComplete[,c(1:3,8:11,13:122,137)]
# treeQ <- rpart(PassFail~.,data = Qdatapf)
# rpart.plot(treeQ)
# summary(dfComplete$grade[dfComplete$midterm<52])
# #All students with midterm score less than 52 failed
# summary(dfComplete$grade[dfComplete$midterm>=74])
# dfComplete$grade[dfComplete$midterm>=74]
# #Only six percent of students with more than 72 in midterm failed
# treeQ$variable.importance
# summary(treeQ)
# plotcp(treeQ)
# #cp=0.037
# #Cross validation
# CVtree <- rep(NA,10)
# FP <- rep(NA,10)
# FN <- rep(NA,10)
# idx <- sample(1:10,nrow(Qdatapf),replace = TRUE)
# for (i in 1:10){
#   train <- Qdatapf[idx!=i,]
#   test <- Qdatapf[idx==i,]
#   tree <- rpart(PassFail~., data = train)
#   pred <- predict(tree,test,type = 'class')
#   CVtree[i] <- mean(pred==test$PassFail)
#   tab <- table(factor(pred, levels = c(0,1)),test$PassFail)
#   FP[i] <- tab[2,1]/sum(tab[,1])
#   if(sum(tab[,2])==0){FN[i] <- 0}
#   else{FN[i] <- tab[1,2]/sum(tab[,2])}
# }
# accuracy <- mean(CVtree)
# sda <- sd(CVtree)
# FPtree <- mean(FP)
# sdFPtree <- sd(FP)
# FNtree <- mean(FN)
# sdFNtree <- sd(FN)
###############################################################


####Trying to predict total score excluding midterm as predictor since it is part of the total score.
#Choosing with lasso (single questions and other predictors)
dfComplete$Exam <- as.numeric(dfComplete$Exam)
x <- as.matrix(dfComplete[,c(1:2,8:11,13:122)])
y <- dfComplete$Exam
lassoTotal <- glmnet(x,y,family = 'gaussian')
plot(lasso,label = T)
CV.lasso <- cv.glmnet(x,y,family='gaussian')
plot(CV.lasso)
CV.lasso$lambda.min
#4.414971 (Changes each time due to crossvalidation randomness)
# NEW: 3.44983
CV.lasso$lambda.1se
#8.467519
#NEW: 7.607327
coef(CV.lasso,s=CV.lasso$lambda.min)
#OLD: PeerSubmissionScore, Q6, Q82, Q98 
# NEW: Q99, Q82, Q57, Q56, Q46, Q43, Q36, Q25, Q6, Q2, PeerCombinedScore, PeerSubmissionScore, KhanCount, saRowAvg
coef(CV.lasso, s=CV.lasso$lambda.1se)
#OLD: Q82, PeerSubmissionScore
# NEW: Q82

post.lasso.min.total <- lm(Exam~Q99+Q82+Q57+Q56+Q46+Q43+Q36+Q25+Q6+Q2+PeerCombinedScore+PeerSubmissionScore+KhanCount+saRowAvg, data=dfComplete)
post.lasso.1se.total <- lm(Exam~Q82, data=dfComplete)
anova(post.lasso.min.total,post.lasso.1se.total) 
#Even though lasso does not choose them some seems to be significant anyway

#Choosing with lasso (categories and other predictors)
x <- as.matrix(dfComplete[,c(1:2,8:11,123:136)])
y <- dfComplete$Exam
lasso.cat <- glmnet(x,y,family = 'gaussian')
plot(lasso.cat,xvar = 'lambda',label = T)
CV.lasso.cat <- cv.glmnet(x,y,family='gaussian')
plot(CV.lasso.cat)
CV.lasso.cat$lambda.min
#2.396323
#NEW: 4.86215
CV.lasso.cat$lambda.1se
#8.814586
#NEW: 7.741917
coef(CV.lasso.cat,s=CV.lasso.cat$lambda.min)
#OLD: Self-control,reasons for going to university, high school trust,grit, education choice factors,PeerSubmissionScore, khancount, saRowAvg
#NEW: PeerCombinedScore, PeerSubmissionScore, saNumComp, saRowAvg
coef(CV.lasso.cat, s=CV.lasso.cat$lambda.1se)
#OLD & NEW: No predictors at all

post.lasso.min.cat.total <- lm(Exam~PeerCombinedScore+PeerSubmissionScore+saNumComp+saRowAvg,data = dfComplete)
null.model <- lm(Exam~1,data = dfComplete)

######Using best subset selection
#single questions
x <- as.matrix(dfComplete[,c(1:2,8:11,13:122)])
y <- dfComplete$Exam
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names(dfComplete[,c(1:2,8:11,13:122)])[variables[1,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[2,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[3,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[4,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[5,-1]]

# lmfittotal1 <- lm(Exam~Q82,data=dfComplete)
# lmfittotal2 <- lm(Exam~Q82+Q98,data=dfComplete)
# lmfittotal3 <- lm(Exam~Q6+Q82+Q98,data=dfComplete)
# lmfittotal4 <- lm(Exam~PeerSubmissionScore+Q46+Q56+Q82,data=dfComplete)
# lmfittotal5 <- lm(Exam~PeerSubmissionScore+Q6+Q56+Q82+Q98,data=dfComplete)

lmfittotal1 <- lm(Exam~Q82,data=dfComplete)
lmfittotal2 <- lm(Exam~Q56+Q82,data=dfComplete)
lmfittotal3 <- lm(Exam~PeerSubmissionScore+Q56+Q82,data=dfComplete)
lmfittotal4 <- lm(Exam~Q46+Q56+Q82+Q99,data=dfComplete)
lmfittotal5 <- lm(Exam~PeerSubmissionScore+Q46+Q56+Q82+Q99,data=dfComplete)
AIC(lmfittotal1,lmfittotal2,lmfittotal3,lmfittotal4,lmfittotal5)

#Categories
x <- as.matrix(dfComplete[,c(1:2,8:11,123:136)])
y <- dfComplete$Exam
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names(dfComplete[,c(1:2,8:11,123:136)])[variables[1,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[2,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[3,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[4,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[5,-1]]

# lmcatfittotal1 <- lm(Exam~PeerSubmissionScore, data=dfComplete)
# lmcatfittotal2 <- lm(Exam~PeerSubmissionScore+Selfcontrol, data=dfComplete)
# lmcatfittotal3 <- lm(Exam~PeerSubmissionScore+HighSchoolTrust+Selfcontrol, data=dfComplete)
# lmcatfittotal4 <- lm(Exam~saRowAvg+PeerSubmissionScore+HighSchoolTrust+Selfcontrol, data=dfComplete)
# lmcatfittotal5 <- lm(Exam~KhanCount+PeerSubmissionScore+HighSchoolTrust+PersonalTraitComparison+Selfcontrol, data=dfComplete)

lmcatfittotal1 <- lm(Exam~PeerSubmissionScore, data=dfComplete)
lmcatfittotal2 <- lm(Exam~saRowAvg+PeerSubmissionScore, data=dfComplete)
lmcatfittotal3 <- lm(Exam~saRowAvg+PeerSubmissionScore+Selfcontrol, data=dfComplete)
lmcatfittotal4 <- lm(Exam~saRowAvg+PeerSubmissionScore+HighSchoolTrust+Selfcontrol, data=dfComplete)
lmcatfittotal5 <- lm(Exam~saRowAvg+PeerSubmissionScore+HighSchoolTrust+PerceivedAcademicAbilities+Selfcontrol, data=dfComplete)

AIC(lmcatfittotal1,lmcatfittotal2,lmcatfittotal3,lmcatfittotal4,lmcatfittotal5)

#Crossvalidation
CVlassoTotalmin <- CVlm(post.lasso.min.total,dfComplete)
CVlassoTotal1se <- CVlm(post.lasso.1se.total,dfComplete)
CVlassoTotalcatmin <- CVlm(post.lasso.min.cat.total,dfComplete)
CVnull <- CVlm(lmnull,dfComplete)
CVQ1 <- CVlm(lmfittotal1,dfComplete)
CVQ2 <- CVlm(lmfittotal2,dfComplete)
CVQ3 <- CVlm(lmfittotal3,dfComplete)
CVQ4 <- CVlm(lmfittotal4,dfComplete)
CVQ5 <- CVlm(lmfittotal5,dfComplete)
CVcat1 <- CVlm(lmcatfittotal1,dfComplete)
CVcat2 <- CVlm(lmcatfittotal2,dfComplete)
CVcat3 <- CVlm(lmcatfittotal3,dfComplete)
CVcat4 <- CVlm(lmcatfittotal4,dfComplete)
CVcat5 <- CVlm(lmcatfittotal5,dfComplete)

Model <- c('lasso.total','nullmodel','lmQ1total','lmQ2total','lmQ3total','lmQ4total','lmQ5total','lmcat1total','lmcat2total','lmcat3total','lmcat4total','lmcat5total')
CVmean <- c(mean(CVlassoTotal1se),mean(CVnull),mean(CVQ1),mean(CVQ2),mean(CVQ3),mean(CVQ4),mean(CVQ5),mean(CVcat1),mean(CVcat2),mean(CVcat3),mean(CVcat4),mean(CVcat5))
se <-(1/sqrt(10))*c(sd(CVlassoTotal1se),sd(CVnull),sd(CVQ1),sd(CVQ2),sd(CVQ3),sd(CVQ4),sd(CVQ5),sd(CVcat1),sd(CVcat2),sd(CVcat3),sd(CVcat4),sd(CVcat5))
Parametre <- c(3,1,2,3,4,5,6,2,3,4,5,6)
(ggplot()+geom_point(aes(Model,CVmean, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=CVmean-se, ymax=CVmean+se),width=0.25) 
  +scale_colour_discrete(name="Measure")+scale_size_continuous(name="Parameters",breaks = seq(1,8,1)))

#none seems to be a clear improvement to the null model
xtable(AIC(null.model,post.lasso.1se.total,lmfittotal1,lmfittotal2,lmfittotal3,lmfittotal4,lmfittotal5,lmcatfittotal1,lmcatfittotal2,lmcatfittotal3,lmcatfittotal4,lmcatfittotal5))
#lmfittotal5 lavest AIC
AIC(null.model,post.lasso.1se.total,lmfittotal1,lmfittotal2,lmfittotal3,lmfittotal4,lmfittotal5,lmcatfittotal1,lmcatfittotal2,lmcatfittotal3,lmcatfittotal4,lmcatfittotal5,k=log(nrow(dfComplete)))
#and BIC
xtable(summary(lmfittotal5)$coefficients)
#Everything significant
xtable(confint(lmfittotal5))
# res <- residuals(lmfittotal5)
# hist(res)
# qqnorm(res)
# qqline(res)
# plot(res)
# plot(res~lmfittotal5$fitted.values)
# plot(res~dfComplete$PeerSubmissionScore)
# plot(res~dfComplete$Q6)
# plot(res~dfComplete$Q56)
# plot(res~dfComplete$Q82)
# plot(res~dfComplete$Q98)


###Q82 shows up pretty often so making some plots with this
# plot(Total~Q82, data = dfComplete)
# abline(lmfittotal1)
# plot(Total~factor(Q82),data = dfComplete)
# #It seems to make sense
# lmfitQ82asfactor <- lm(Total~factor(Q82),data = dfComplete)
# summary(lmfitQ82asfactor)

# plot(Exam~Q82, data = dfComplete)
# abline(lmfittotal1)
# plot(Exam~factor(Q82),data = dfComplete)
# #It seems to make sense
# lmfitQ82asfactor <- lm(Exam~factor(Q82),data = dfComplete)
# summary(lmfitQ82asfactor)

##Checking residuals for one model
# res <- residuals(lmfit5)
# plot(res)
# plot(res~lmfit5$fitted.values)
# plot(res~dfComplete$midterm)
# plot(res~factor(dfComplete$Q22))
# plot(res~factor(dfComplete$Q56))
# plot(res~factor(dfComplete$Q82))
# plot(res~factor(dfComplete$Q107))
# hist(res)
# qqnorm(res)
# qqline(res)

###############################################################
#######Trying to predict dropout based on exam results#########
###############################################################
table(dfdropoutgrade$Status)
#Not many who have taken the exam has dropped out yet

MedData <- read.csv('MedDataBSc.csv',encoding="UTF-8")
MedData$dropout <- factor(as.numeric(MedData$statussn=='afbrudt'))
logdropout <- glm(dropout~X1_T_mGPA, data = MedData, family = binomial)
summary(logdropout)
data2017 <- dfdropoutgrade[,c('grade','Status')]
names(data2017)[1] <- 'X1_T_mGPA'
data2017$X1_T_mGPA <- as.numeric(data2017$X1_T_mGPA)
data2017$X1_T_mGPA <- ifelse(is.na(data2017$X1_T_mGPA), -3, data2017$X1_T_mGPA) # Bianca added

predprop <- predict(logdropout,newdata = data2017, type = 'response')
predclass <- as.numeric(predprop>0.5)
# confusion matrix
xtable(table(predclass,data2017$Status))
#The model actually identifies most of the dropout students. 25 active student are also predicted to
#dropout and it is yet unknown how acurate this is.

# IGNORE: confusion matrix with Q999 considered as dropout 
obs <- ifelse(data2017$Status == "active", 1,0)
confusion.matrix(obs,predprop,threshold=0.5)
#TP:1(DO), FP:25, TN:40, FN:11 # this doesn't seem to be fit with the above

#######################################################################
####### Extra analysis: ROC, confusion matrix, (cost function)#########
#######################################################################

# 1) create ROC on each prediction models (in one graph)

# for the most interesting prediction model from ROC:
# 2) create confusion matrix and 
# 3) create a Sensitivity and specificity versus criterion value 
# (ala Ninna's graph Accuracy, FP and FN rates from 10-fold crossvalidation in updated_analysis_2017_new.R) 

# test with dropout data (one predictor) - worked!
DOstatus <- ifelse(data2017$Status == "active", 1, 0)
predictions <- data.frame(DOstatus, predprop)

# predicting dropouts
# calculate ROC and cost function
calculate_roc <- function(data2017, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(data2017, threshold) {
    sum(predprop >= threshold & DOstatus == 0) / sum(DOstatus == 0)
  }
  
  fpr <- function(data2017, threshold) {
    sum(predprop >= threshold & DOstatus == 1) / sum(DOstatus == 1)
  }
  
  cost <- function(data2017, threshold, cost_of_fp, cost_of_fn) {
    sum(predprop >= threshold & DOstatus <= 1) * cost_of_fp + 
      sum(predprop < threshold & DOstatus >= 0) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(data2017, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(data2017, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(data2017, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}
roc <- calculate_roc(predictions, 1, 2, n = 100)

# plot ROC and cost function
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}
plot_roc(roc, 0.7, 1, 2)

# area under the curve
auc(predictions$DOstatus, predictions$predprop)
# 0.8730769



#############
# same method but with GPRO data

# predprop <- predict(logdropout,newdata = data2017, type = 'response')
#dfComplete$pass <- ifelse(dfComplete$grade >= 2, 1,0)
#dfCompleteTest$pass <- ifelse(dfCompleteTest$grade >= 2, 1,0)

# TOGGLE: changed from grade to the fianl exam score:
#dfComplete$pass <- ifelse(dfComplete$Exam >= 52, 1,0)
#dfCompleteTest$pass <- ifelse(dfCompleteTest$Exam >= 52, 1,0)

# for model lmfittotal1
# passStudents <- data.frame(dfCompleteTest$Q82, dfCompleteTest$pass)
# names(passStudents) <- c("Q82","passed")
# lmfittotal5Pred <- predict(lmfittotal1, newdata=passStudents, type="response") # not sure this is correct.. need a number btw. 0 and 1?
# lmfittotal5Pred <- lmfittotal5Pred/100
# passed <- passStudents$passed
# lmfittotal5Predictions <- data.frame(dfCompleteTest$pass, lmfittotal5Pred)
# names(lmfittotal5Predictions) <- c("passed", "pred")

# for model lmfittotal5
passStudents <- data.frame(dfCompleteTest$PeerSubmissionScore, dfCompleteTest$Q46, dfCompleteTest$Q56, dfCompleteTest$Q82, dfCompleteTest$Q99,
                           dfCompleteTest$PassFail)
names(passStudents) <- c("PeerSubmissionScore", "Q46", "Q56", "Q82","Q99", "passed")

#linkinv <- family(lmfittotal5)$linkinv
lmfittotal5Pred <- predict(lmfittotal5, newdata=passStudents, type="response") # not sure this is correct.. need a number btw. 0 and 1?
#linkinv(lmfittotal5Pred)
#lmfittotal5Pred <- lmfittotal5Pred/100
passed <- passStudents$passed
lmfittotal5Predictions <- data.frame(dfCompleteTest$PassFail, lmfittotal5Pred)
names(lmfittotal5Predictions) <- c("passed", "pred")

dfComplete$pred <- lmfittotal5Pred
# Distribution of the predictions
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(lmfittotal5Pred >= threshold & dfCompleteTest$PassFail == 1, "TP", v)
  v <- ifelse(lmfittotal5Pred >= threshold & dfCompleteTest$PassFail == 0, "FP", v)
  v <- ifelse(lmfittotal5Pred < threshold & dfCompleteTest$PassFail == 1, "FN", v)
  v <- ifelse(lmfittotal5Pred < threshold & dfCompleteTest$PassFail == 0, "TN", v)
  
  dfComplete$pred_type <- v
  
  ggplot(data=dfComplete, aes(x=pass, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}
#threshold <- 59.6725 # using the absolute value of the intercept as threshold
plot_pred_type_distribution(lmfittotal5Predictions, 0.7) 

dfCompleteTest$pred <- lmfittotal5Pred
# confusion matrix for predicting GPRO pass/fail
confusion.matrix(dfCompleteTest$PassFail,dfCompleteTest$pred,threshold=0.7)
# 00=4, 11=4, 01=4, 10=6 , where 1=passed

# predicting passed GPRO
# calculate ROC and cost function
calculate_roc <- function(passStudents, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(passStudents, threshold) {
    sum(lmfittotal5Pred >= threshold & passed == 1) / sum(passed == 1)
  }
  
  fpr <- function(passStudents, threshold) {
    sum(lmfittotal5Pred >= threshold & passed == 0) / sum(passed == 0)
  }
  
  cost <- function(passStudents, threshold, cost_of_fp, cost_of_fn) {
    sum(lmfittotal5Pred >= threshold & passed <= 0) * cost_of_fp + 
      sum(lmfittotal5Pred < threshold & passed >= 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(passStudents, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(passStudents, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(passStudents, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}
roc <- calculate_roc(lmfittotal5Predictions, 1, 2, n = 100)

# plot ROC and cost function
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}
plot_roc(roc, 0.70, 1, 2)

# area under the curve
auc(lmfittotal5Predictions$passed, lmfittotal5Predictions$pred)
# 0.55






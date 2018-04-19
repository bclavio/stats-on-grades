library(tidyr)
library(glmnet)
library(leaps)
library(hydroGOF)
library(ggplot2)
library(rpart)
library(rpart.plot)
############################################
#######preparing and loading data###########
############################################
setwd('C:/Users/Helle/Downloads/2018_SLERD_Paper_Analysis/Paper')

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
# Remove all incomplete entries leaves only 77 students
# This removes all PDP students
dfComplete<-dfComplete[complete.cases(dfComplete), ]
#Remove students who didn't take the exam
dfComplete$Exam <- as.numeric(dfComplete$Exam)
dfComplete$'mid-term' <- as.numeric(dfComplete$'mid-term')
dfComplete <- dfComplete[!is.na(dfComplete$Exam),]
#Add pass/fail variable fail=1
dfComplete$grade <- as.numeric(dfComplete$grade)
dfComplete$PassFail <- factor(as.numeric(dfComplete$grade<2))
#Deleting unnecesarry colums
Email <- dfComplete$`Email address`
dfComplete <- dfComplete[,-c(1,2,3,6,8,10,18)]
names(dfComplete)[c(3,9:12)] <- c('midterm','PeerSubmissionScore','PeerFeedbackScore','PeerCombinedScore','SSPGrade')
names(dfComplete)[123:136] <- c('AttitudeTowardsEducation', "BelongingUncertainty","Demographics", "EducationChoiceFactors", "Grit" ,"GrowthMindset","HighSchoolBehaviour","HighSchoolTrust","PerceivedAcademicAbilities","PersonalTraitComparison","ReasonsforGoingtoUniversity","Selfcontrol","StudyingandWorkingHours","ViewonMedialogy")
####################################
############Analysis################
####################################
mean(dfComplete$PassFail==1)
#34.7 percent failed the course
plot(dfComplete$Exam~dfComplete$saNumComp)
plot(dfComplete$Exam~dfComplete$saRowAvg)
plot(dfComplete$Exam~dfComplete$midterm)
#There seems to be a tendency
plot(dfComplete$Exam~dfComplete$KhanCount)
#There could be a tendency
plot(dfComplete$Exam~dfComplete$PeerSubmissionScore)
plot(dfComplete$Exam~dfComplete$PeerFeedbackScore)
plot(dfComplete$Exam~dfComplete$PeerCombinedScore)
plot(dfComplete$Exam~dfComplete$SSPGrade)
plot(dfComplete$Exam~factor(dfComplete$Q33))
plot(dfComplete$Exam~factor(dfComplete$Q78))

#If only interested in passed failed
plot(dfComplete$saNumComp~factor(dfComplete$PassFail))
plot(dfComplete$saRowAvg~factor(dfComplete$PassFail))
plot(dfComplete$midterm~factor(dfComplete$PassFail))
plot(dfComplete$KhanCount~factor(dfComplete$PassFail))
plot(dfComplete$PeerSubmissionScore~factor(dfComplete$PassFail))
plot(dfComplete$PeerFeedbackScore~factor(dfComplete$PassFail))
plot(dfComplete$PeerCombinedScore~factor(dfComplete$PassFail))
plot(dfComplete$SSPGrade~factor(dfComplete$PassFail))
plot(dfComplete$Q33~factor(dfComplete$PassFail))
plot(dfComplete$Q78~factor(dfComplete$PassFail))

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
CV.lasso$lambda.min
#3.858913 (Changes each time due to crossvalidation randomness)
CV.lasso$lambda.1se
#6.74356
coef(CV.lasso,s=3.858913)
#Q82,Q22,KhanCount,midterm
coef(CV.lasso, s=6.74356)
#Only midterm

post.lasso.min <- lm(Exam~midterm+KhanCount+Q22+Q82, data=dfComplete)
post.lasso.1se <- lm(Exam~midterm, data=dfComplete)
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
CV.lasso.cat$lambda.1se
#3.203738
coef(CV.lasso.cat,s=0.7935902)
#Self-control,reasons for going to university, persona trai comparison, perceived academic abilities
#high school trust,grit, education choice factors, demographics,PeerCombinedScore,Peersubmissionscore
#khancount, midterm, SaNumComp
##That is simply too many to be relevant
coef(CV.lasso.cat, s=6.74356)
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
lmfit1 <- lm(Exam~midterm,data=dfComplete)
lmfit2 <- lm(Exam~midterm+Q82,data=dfComplete)
lmfit3 <- lm(Exam~midterm+KhanCount+Q82,data=dfComplete)
lmfit4 <- lm(Exam~midterm+Q56+Q82+Q107,data=dfComplete)
lmfit5 <- lm(Exam~midterm+Q22+Q56+Q82+Q107,data=dfComplete)


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

lmcatfit1 <- lm(Exam~midterm, data=dfComplete)
lmcatfit2 <- lm(Exam~midterm+KhanCount, data=dfComplete)
lmcatfit3 <- lm(Exam~midterm+KhanCount+HighSchoolTrust, data=dfComplete)
lmcatfit4 <- lm(Exam~midterm+KhanCount+HighSchoolTrust+Selfcontrol, data=dfComplete)
lmcatfit5 <- lm(Exam~midterm+KhanCount+HighSchoolTrust+Selfcontrol+PersonalTraitComparison, data=dfComplete)

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
lmfit05 <- step(lmnull,scope=scope,direction='both',k=3.84)
#midterm,Q82,KhanCount,Q60,Q33,Q107,Q50,Q20,Q7,Q66,Q22,Q34,Q92,Q9,Q86,Q54, 
#Q4,Q80,Q99,Q17, Q108, Q76, Q31
#Sig.level 0.01
lmfit01 <- step(lmnull,scope=scope,direction='both',k=6.63)
#midterm, Q82, KhanCount, Q60

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
lmfitcat05 <- step(lmnull,scope=scope,direction='both',k=3.84)
#midterm,KhanCount,HighSchoolTrust,Selfcontrol,PersonalTraitComparison,ReasonsforGoingtoUniversity,Demographics
#Sig.level 0.01
lmfitcat01 <- step(lmnull,scope=scope,direction='both',k=6.63)
#midterm, KhanCount

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



Model <- c('post.lasso.Q.min','post.lasso.1se','lmQ2','lmQ3','lmQ4','lmQ5','lmcat2','lmcat3','lmcat4','lmcat5','lmQp01','lmcatp05','lmcatp01')
CVmean <- c(mean(CVpostlassomin),mean(CVpostlasso1se),mean(CVsub2),mean(CVsub3),mean(CVsub4),mean(CVsub5),mean(CVsubcat2),mean(CVsubcat3),mean(CVsubcat4),mean(CVsubcat5),mean(CVstep01),mean(CVcatstep05),mean(CVcatstep01))
se <- (1/sqrt(10))*c(sd(CVpostlassomin),sd(CVpostlasso1se),sd(CVsub2),sd(CVsub3),sd(CVsub4),sd(CVsub5),sd(CVsubcat2),sd(CVsubcat3),sd(CVsubcat4),sd(CVsubcat5),sd(CVstep01),sd(CVcatstep05),sd(CVcatstep01))
Parametre <- c(5,2,3,4,5,6,3,4,5,6,5,8,3)
(ggplot()+geom_point(aes(Model,CVmean, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=CVmean-se, ymax=CVmean+se),width=0.25) 
  +scale_colour_discrete(name="Measure")+scale_size_continuous(name="Parameters",breaks = seq(1,8,1)))

##Trying to predict passed failed with classification tree
Qdatapf <- dfComplete[,c(1:3,8:11,13:122,137)]
treeQ <- rpart(PassFail~.,data = Qdatapf)
rpart.plot(treeQ)
summary(dfComplete$grade[dfComplete$midterm<54])
#All students with midterm score less than 54 failed
summary(dfComplete$grade[dfComplete$midterm>=72])
dfComplete$grade[dfComplete$midterm>=72]
#Only six percent of students with more than 72 in midterm failed
treeQ$variable.importance
summary(treeQ)
plotcp(treeQ)
#cp=0.037
#Cross validation
CVtree <- rep(NA,10)
FP <- rep(NA,10)
FN <- rep(NA,10)
idx <- sample(1:10,nrow(Qdatapf),replace = TRUE)
for (i in 1:10){
  train <- Qdatapf[idx!=i,]
  test <- Qdatapf[idx==i,]
  tree <- rpart(PassFail~., data = train)
  pred <- predict(tree,test,type = 'class')
  CVtree[i] <- mean(pred==test$PassFail)
  tab <- table(factor(pred, levels = c(0,1)),test$PassFail)
  FP[i] <- tab[2,1]/sum(tab[,1])
  if(sum(tab[,2])==0){FN[i] <- 0}
  else{FN[i] <- tab[1,2]/sum(tab[,2])}
}
accuracy <- mean(CVtree)
sda <- sd(CVtree)
FPtree <- mean(FP)
sdFPtree <- sd(FP)
FNtree <- mean(FN)
sdFNtree <- sd(FN)

####Trying to predict total score excluding midterm as predictor since it is part of the total score.
#Choosing with lasso (single questions and other predictors)
dfComplete$Total <- as.numeric(dfComplete$Total)
x <- as.matrix(dfComplete[,c(1:2,8:11,13:122)])
y <- dfComplete$Total
lassoTotal <- glmnet(x,y,family = 'gaussian')
plot(lasso,label = T)
CV.lasso <- cv.glmnet(x,y,family='gaussian')
plot(CV.lasso)
CV.lasso$lambda.min
#4.414971 (Changes each time due to crossvalidation randomness)
CV.lasso$lambda.1se
#8.467519
coef(CV.lasso,s=4.414971)
#PeerSubmissionScore, Q6, Q82, Q98
coef(CV.lasso, s=8.467519)
#Q82, PeerSubmissionScore

post.lasso.min.total <- lm(Total~PeerSubmissionScore+Q6+Q82+Q98, data=dfComplete)
post.lasso.1se.total <- lm(Total~Q82+PeerSubmissionScore, data=dfComplete)
anova(post.lasso.min,post.lasso.1se)
#Even though lasso does not choose them some seems to be significant anyway

#Choosing with lasso (categories and other predictors)
x <- as.matrix(dfComplete[,c(1:2,8:11,123:136)])
y <- dfComplete$Total
lasso.cat <- glmnet(x,y,family = 'gaussian')
plot(lasso.cat,xvar = 'lambda',label = T)
CV.lasso.cat <- cv.glmnet(x,y,family='gaussian')
plot(CV.lasso.cat)
CV.lasso.cat$lambda.min
#2.396323
CV.lasso.cat$lambda.1se
#8.814586
coef(CV.lasso.cat,s=2.396323)
#Self-control,reasons for going to university, 
#high school trust,grit, education choice factors,PeerSubmissionScore
#khancount, saRowAvg
coef(CV.lasso.cat, s=8.814586)
#No predictors at all

post.lasso.min.cat.total <- lm(Total~Selfcontrol+ReasonsforGoingtoUniversity+HighSchoolTrust+Grit+EducationChoiceFactors+PeerSubmissionScore+saRowAvg,data = dfComplete)
null.model <- lm(Total~1,data = dfComplete)

######Using best subset selection
#single questions
x <- as.matrix(dfComplete[,c(1:2,8:11,13:122)])
y <- dfComplete$Total
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names(dfComplete[,c(1:2,8:11,13:122)])[variables[1,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[2,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[3,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[4,-1]]
names(dfComplete[,c(1:2,8:11,13:122)])[variables[5,-1]]
lmfittotal1 <- lm(Total~Q82,data=dfComplete)
lmfittotal2 <- lm(Total~PeerSubmissionScore+Q82,data=dfComplete)
lmfittotal3 <- lm(Total~PeerSubmissionScore+Q56+Q82,data=dfComplete)
lmfittotal4 <- lm(Total~PeerSubmissionScore+Q6+Q82+Q98,data=dfComplete)
lmfittotal5 <- lm(Total~PeerSubmissionScore+Q6+Q56+Q82+Q98,data=dfComplete)

AIC(lmfittotal1,lmfittotal2,lmfittotal3,lmfittotal4,lmfittotal5)
#Categories
x <- as.matrix(dfComplete[,c(1:2,8:11,123:136)])
y <- dfComplete$Total
subsets <- regsubsets(x,y,nvmax = 5,method = 'exhaustive', really.big = T)
variables <- summary(subsets)$which
names(dfComplete[,c(1:2,8:11,123:136)])[variables[1,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[2,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[3,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[4,-1]]
names(dfComplete[,c(1:2,8:11,123:136)])[variables[5,-1]]

lmcatfittotal1 <- lm(Total~PeerSubmissionScore, data=dfComplete)
lmcatfittotal2 <- lm(Total~PeerSubmissionScore+Selfcontrol, data=dfComplete)
lmcatfittotal3 <- lm(Total~saRowAvg+PeerSubmissionScore+Selfcontrol, data=dfComplete)
lmcatfittotal4 <- lm(Total~saRowAvg+PeerSubmissionScore+HighSchoolTrust+Selfcontrol, data=dfComplete)
lmcatfittotal5 <- lm(Total~saRowAvg+PeerSubmissionScore+HighSchoolTrust+ReasonsforGoingtoUniversity+Selfcontrol, data=dfComplete)

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

Model <- c('lasso.Q.min.total','lasso.Q.1se.total','lasso.cat.min.total','nullmodel','lmQ1total','lmQ2total','lmQ3total','lmQ4total','lmQ5total','lmcat1total','lmcat2total','lmcat3total','lmcat4total','lmcat5total')
CVmean <- c(mean(CVlassoTotalmin),mean(CVlassoTotal1se),mean(CVlassoTotalcatmin),mean(CVnull),mean(CVQ1),mean(CVQ2),mean(CVQ3),mean(CVQ4),mean(CVQ5),mean(CVcat1),mean(CVcat2),mean(CVcat3),mean(CVcat4),mean(CVcat5))
se <- c(sd(CVlassoTotalmin),sd(CVlassoTotal1se),sd(CVlassoTotalcatmin),sd(CVnull),sd(CVQ1),sd(CVQ2),sd(CVQ3),sd(CVQ4),sd(CVQ5),sd(CVcat1),sd(CVcat2),sd(CVcat3),sd(CVcat4),sd(CVcat5))
Parametre <- c(5,3,8,1,2,3,4,5,6,2,3,4,5,6)
(ggplot()+geom_point(aes(Model,CVmean, size=Parametre)) + geom_errorbar(aes(x=Model, ymin=CVmean-se, ymax=CVmean+se),width=0.25) 
  +scale_colour_discrete(name="Measure")+scale_size_continuous(name="Parameters",breaks = seq(1,8,1)))


###Q82 shows up pretty often so making some plots with this
plot(Total~Q82, data = dfComplete)
abline(lmfittotal1)
plot(Total~factor(Q82),data = dfComplete)
#It seems to make sense
lmfitQ82asfactor <- lm(Total~factor(Q82),data = dfComplete)
summary(lmfitQ82asfactor)

###############################################################
#######Trying to predict dropout based on exam results#########
###############################################################
dropoutFeb18 <- read.csv("Y:/analysis_data/dropOut/data/dropout-Feb2018-Q999.csv", encoding="UTF-8",stringsAsFactors=FALSE)
names(dropoutFeb18)[1] <- 'Name'
dropoutFeb18 <- dropoutFeb18[,2:3]
dfComplete$Email <- Email
test <- merge(dfComplete,dropoutFeb18,by='Email',all.x = T)
which(!dfComplete$Email%in%dropoutFeb18$Email)
Email[c(19,42)]

library(tidyr)
library(glmnet)
library(rpart)
library(rpart.plot)

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
SSPDropout$Status <- factor(SSPDropout$Status, levels = c('dropout','active'))
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
###NEED TO REPLACE MISSING VALUES

######################################
#########Analysis#####################
######################################

#Checking whether campus has any influence on dropout

logCampus <- glm(drop~Campus, family = binomial(link='logit'), data = SSPDropout)
summary(logCampus)
drop1(logCampus, test = 'Chisq')
#Does not seem to have any influence

####Shifting focus to SSP questions

#Making some different logistic regression models
loggrade <- glm(SSPQuestions$Status~SSP$SSPGrade, family = binomial(link = 'logit'))
summary(loggrade)
#Overall grade does not seem to be significanat
boxplot(SSPGrade~Status, data=SSP)

#Looking at categories average
cat <- names(SSPCategoryAVG[2:15])
for (i in 1:length(cat)){
  boxplot(SSPCategoryAVG[,cat[i]]~SSPCategoryAVG$Status, main=cat[i])
}
#Could indicate that the following are interesting predictors
#high school behavior, perceived academic abilities, studying and working hours, view on mediology 


logfullcat <- glm(Status~., data = SSPCategoryAVG, family=binomial(link = 'logit'))
summary(logfullcat)
drop <- drop1(logfullcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
as.data.frame(drop)[maxp,]
logcat <- update(logfullcat,~.-`Attitude Towards Education`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Education Choice Factors`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-Demographics)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Self-control`)

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
logcat <- update(logcat,~.-`View on Medialogy`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Reasons for Going to University`)

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
logcat <- update(logcat,~.-`Perceived Academic Abilities`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Studying and Working Hours`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcat <- update(logcat,~.-`Belonging Uncertainty`)

drop <- drop1(logcat, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]

summary(logcat)
#Does not really indicate that any categories are significant

#Try to chose the model with AIC and BIC
catAIC <- step(logfullcat)
catBIC <- step(logfullcat, k=log(nrow(SSPCategoryAVG)))
#The BIC is just the null model

#####Looking into single questions
q <- names(SSPQuestions[2:112])
for (i in 1:length(q)){
  boxplot(SSPQuestions[,q[i]]~SSPQuestions$Status, main=q[i])
}
#There are very little dispersion for some questions. Maybe they should be treated as factors instead of numeric

fit <- glm(Status~1,data = SSPQ,family = binomial(link='logit'))
add <- add1(fit, scope = names(SSPQ)[2:108],test = 'Chisq')
add[which.min(add$`Pr(>Chi)`),]
fit <- update(fit,~.+Q108)
add <- add1(fit, scope = names(SSPQ)[2:108],test = 'Chisq')
add[which.min(add$`Pr(>Chi)`),]
###Giver bare samme som lasso senere

###Brug lasso
#Replace missing values with mean of column (Only 5 in total)
for (i in 2:ncol(SSPQ)){
  SSPQ[is.na(SSPQ[,i]),i] <- mean(SSPQ[,i],na.rm=TRUE)
}
x <- as.matrix(SSPQ[,-1])
y <- factor(SSPQ[,1])

lassofit <- glmnet(x,y,family = 'binomial')
plot(lassofit, xvar='lambda')
CV.lambda <- cv.glmnet(x,y,family='binomial')
plot(CV.lambda)
CV.lambda$lambda.min
CV.lambda$lambda.1se

coef(CV.lambda, s='lambda.1se')
#All variables are disregarded
coef(CV.lambda, s=0.08)
#This is not the best choice by crossvalidation but now Q108, Q107 and Q98 is there

#Q108 seems to be significant
fit <- glm(Status~Q108, data = SSPQ, family = binomial)
summary(fit)
drop1(fit, test='Chisq')
boxplot(Q108~Status,data = SSPQ)

###Trying decision trees
tree <- rpart(Status~., data = SSPQ)
rpart.plot(tree)
tree$variable.importance
plotcp(tree)
#Also suggest not to use anything

tree <- rpart(Status~., data = SSPCategoryAVG)
rpart.plot(tree)
tree$variable.importance
plotcp(tree)
#Also indicate not to use any predictors


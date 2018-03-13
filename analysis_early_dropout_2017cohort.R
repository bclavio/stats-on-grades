library(tidyr)
library(glmnet)
library(rpart)
library(rpart.plot)
dropoutFeb18 <- read.csv("Y:/analysis_data/dropOut/data/dropout-Feb2018-Q999.csv", encoding="UTF-8",stringsAsFactors=FALSE)
names(dropoutFeb18)[1] <- 'Name'

questions<-read.csv("C:/Users/Helle/Downloads/2018_SLERD_Paper_Analysis/Paper/QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
SSP<-read.csv("Y:/analysis_data/SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE, encoding="UTF-8")
names(SSP)[1] <- 'Surname'
names(SSP)[5] <- 'Email'
SSPName <- unite_(SSP,'Name', c("First name","Surname"), sep = ' ')
SSPName <- SSPName[-92,]

SSPDropout <- merge(dropoutFeb18,SSPName, by='Name', all.x = TRUE, all.y = TRUE)
na <- (SSPDropout$Email.x==''|is.na(SSPDropout$Email.x))
SSPDropout$Email.x[na] <-SSPDropout$Email.y[na]
SSPDropout <- SSPDropout[,-(6:8)]
SSPDropout$Campus[SSPDropout$Campus==''] <- NA
#SSP is only taken i AAL so students with answers must be from AAL
SSPDropout$Campus[!is.na(SSPDropout$State)&is.na(SSPDropout$Campus)] <- 'AAL'
#Assuming the others with missing campus are from CPH
SSPDropout$Campus[is.na(SSPDropout$Campus)] <- 'CPH'
table(SSPDropout$Status,SSPDropout$Campus)

#Checking whether campus has any influence on dropout
#OBS! in the following Q999 is treated as dropout
drop <- factor(SSPDropout$Status)
drop[drop=='Q999'] <- 'dropout'
drop <- factor(drop)
logCampus <- glm(drop~Campus, family = binomial(link='logit'), data = SSPDropout)
summary(logCampus)
drop1(logCampus, test = 'Chisq')
#Does not seem to have any influence

####Shifting focus to SSP questions
Qnolab <- questions[,c('Category','QuestionLabel','QuestionNo')]
Qnolab <- Qnolab[!duplicated(Qnolab),]
lab <- Qnolab$QuestionLabel[Qnolab$QuestionNo]
#Making the names easier to interpret in the analysis
names(SSPDropout)[11:121] <- lab
SSP <- SSPDropout[SSPDropout$State=='Finished'&!is.na(SSPDropout$State),]
SSP[SSP=='-'] <- NA
SSP[,11:121] <- apply(SSP[,11:121],MARGIN = 2,as.numeric)
#Sorting into categories
cat <- factor(questions$Category)

for (i in 1:length(levels(cat))){
  category <- levels(cat)[i]
  col <- names(SSP) %in% Qnolab$QuestionLabel[Qnolab$Category==category]
  SSP[,121+i] <- rowMeans(SSP[,col], na.rm = TRUE)
}
names(SSP)[122:135] <- levels(cat)


for (i in 1:length(levels(cat))){
  category <- levels(cat)[i]
  col <- names(SSP) %in% Qnolab$QuestionLabel[Qnolab$Category==category]
  SSP[,135+i] <- rowSums(SSP[,col], na.rm = TRUE)
}
SSP$`Grade/10.00` <- as.numeric(SSP$`Grade/10.00`)
names(SSP)[10] <- 'SSPGrade'

SSPQuestions <- SSP[,c(3,11:121)]
SSPQuestions$Status[SSPQuestions$Status=='Q999'] <-'dropout' 
SSPQuestions$Status <- factor(SSPQuestions$Status, levels = c('dropout','active'))
SSPCategoryAVG <- SSP[,c(3,122:135)]
SSPCategoryAVG$Status[SSPCategoryAVG$Status=='Q999'] <-'dropout' 
SSPCategoryAVG$Status <- factor(SSPCategoryAVG$Status, levels = c('dropout','active'))
SSPCategorySum <- SSP[,c(3,136:149)]
names(SSPCategorySum)[2:15] <- levels(cat)
SSPCategorySum$Status[SSPCategorySum$Status=='Q999'] <-'dropout' 
SSPCategorySum$Status <- factor(SSPCategorySum$Status, levels = c('dropout','active'))


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

###Tries to repeat it with sum instead of average

logfullcatsum <- glm(Status~., data = SSPCategorySum, family=binomial(link = 'logit'))
summary(logfullcatsum)
drop <- drop1(logfullcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
as.data.frame(drop)[maxp,]
logcatsum <- update(logfullcatsum,~.-`Education Choice Factors`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Attitude Towards Education`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-Demographics)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Self-control`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-Grit)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Growth Mindset`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`View on Medialogy`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Reasons for Going to University`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Personal Trait Comparison`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`High School Trust`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Perceived Academic Abilities`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Studying and Working Hours`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]
logcatsum <- update(logcatsum,~.-`Belonging Uncertainty`)

drop <- drop1(logcatsum, test = 'Chisq')
maxp <- which.max(drop$`Pr(>Chi)`)
drop[maxp,]

summary(logcatsum)
#Same conclusion

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

SSPQ <- SSPQuestions
Q <- rep('Q',111)
num <- 1:111
dat <- data.frame(Q,num)
dat <- unite_(dat,'names', c("Q","num"), sep = '')
names(SSPQ)[2:112] <-dat[,1] 
#Removing all columns with only one value
col <- rep(0,ncol(SSPQ)-1)
for (i in 2:ncol(SSPQ)){
  fac <- factor(SSPQ[,i])
  if (length(levels(fac))==1){
    col[i] <- i
  }
}
SSPQ <- SSPQ[,-col]
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


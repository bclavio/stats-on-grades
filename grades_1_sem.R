library(ggplot2)

setwd("Y:/analysis_data/dropOut/data")
MedData <- read.csv('MedDataBSc.csv',encoding="UTF-8")
MedData$dropout <- factor(as.numeric(MedData$statussn=='afbrudt'))
table(MedData$startaar)
MedData2012 <- MedData[MedData$startaar==2012,]
MedData2013 <- MedData[MedData$startaar==2013,]
MedData2014 <- MedData[MedData$startaar==2014,]

#All grades on first semester seems to be important
plot(MedData2012$X1_T_mGPA~MedData2012$dropout)
plot(MedData2012$X1_NT_mGPA~MedData2012$dropout)
plot(MedData2012$X1_Pr_mGPA~MedData2012$dropout)

log2012T <- glm(dropout~X1_T_mGPA, data = MedData2012, family = binomial)
summary(log2012T)
exp(-0.18959)-1

log2012NT <- glm(dropout~X1_NT_mGPA, data = MedData2012, family = binomial)
summary(log2012NT)
exp(-0.19509)-1

log2012Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2012, family = binomial)
summary(log2012Pr)
exp(-0.10785)-1
#Thet are all significant. T reduces probability most, but almost the same as NT.

table(MedData2012$X1_T_mGPA,MedData2012$X1_NT_mGPA)
table(MedData2012$X1_T_mGPA,MedData2012$X1_Pr_mGPA)
table(MedData2012$X1_NT_mGPA,MedData2012$X1_Pr_mGPA)
#They seems to be correlated

log2012all <- glm(dropout~X1_T_mGPA+X1_NT_mGPA+X1_Pr_mGPA, data = MedData2012, family = binomial)
summary(log2012all)
drop1(log2012all,test = 'Chisq')
log2012update <- update(log2012all,~.-X1_Pr_mGPA)
drop1(log2012update,test = 'Chisq')
#Both significant
summary(log2012update)
#T reduces probability most

#Try again but given they have started on the third semester
MedData2012.3 <- MedData2012[MedData2012$EndSemester>=3,]

#All grades on first semester seems to be important
plot(MedData2012.3$X1_T_mGPA~MedData2012.3$dropout)
plot(MedData2012.3$X1_NT_mGPA~MedData2012.3$dropout)
plot(MedData2012.3$X1_Pr_mGPA~MedData2012.3$dropout)

log2012T <- glm(dropout~X1_T_mGPA, data = MedData2012.3, family = binomial)
summary(log2012T)
exp(-0.19638)-1

log2012NT <- glm(dropout~X1_NT_mGPA, data = MedData2012.3, family = binomial)
summary(log2012NT)
exp(-0.17675)-1

log2012Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2012.3, family = binomial)
summary(log2012Pr)
exp(-0.07475)-1
#The importanve of NT and Pr is less now.

#Given they have started on fourth semester
MedData2012.4 <- MedData2012[MedData2012$EndSemester>=4,]

#All grades on first semester seems to be important
plot(MedData2012.4$X1_T_mGPA~MedData2012.4$dropout)
plot(MedData2012.4$X1_NT_mGPA~MedData2012.4$dropout)
plot(MedData2012.4$X1_Pr_mGPA~MedData2012.4$dropout)

log2012T <- glm(dropout~X1_T_mGPA, data = MedData2012.4, family = binomial)
summary(log2012T)
exp(-0.1935)-1

log2012NT <- glm(dropout~X1_NT_mGPA, data = MedData2012.4, family = binomial)
summary(log2012NT)
exp(-0.1775)-1

log2012Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2012.4, family = binomial)
summary(log2012Pr)
exp(-0.063)-1
#The importanve of Pr is less now.

#Given they have started on fifth semester
MedData2012.5 <- MedData2012[MedData2012$EndSemester>=5,]

#All grades on first semester seems to be important
plot(MedData2012.5$X1_T_mGPA~MedData2012.5$dropout)
plot(MedData2012.5$X1_NT_mGPA~MedData2012.5$dropout)
plot(MedData2012.5$X1_Pr_mGPA~MedData2012.5$dropout)

log2012T <- glm(dropout~X1_T_mGPA, data = MedData2012.5, family = binomial)
summary(log2012T)
exp(-0.21462)-1

log2012NT <- glm(dropout~X1_NT_mGPA, data = MedData2012.5, family = binomial)
summary(log2012NT)
exp(-0.22271)-1

log2012Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2012.5, family = binomial)
summary(log2012Pr)
exp(-0.07309)-1

#Given they have started on sixth semester
MedData2012.6 <- MedData2012[MedData2012$EndSemester>=6,]

#All grades on first semester seems to be important
plot(MedData2012.6$X1_T_mGPA~MedData2012.6$dropout)
plot(MedData2012.6$X1_NT_mGPA~MedData2012.6$dropout)
plot(MedData2012.6$X1_Pr_mGPA~MedData2012.6$dropout)

log2012T <- glm(dropout~X1_T_mGPA, data = MedData2012.6, family = binomial)
summary(log2012T)
exp(-0.17068)-1

log2012NT <- glm(dropout~X1_NT_mGPA, data = MedData2012.6, family = binomial)
summary(log2012NT)
exp(-0.26322)-1

log2012Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2012.6, family = binomial)
summary(log2012Pr)
exp(-0.014)-1
#The importanve Pr is less now.
##For 2012 the grades on first semester seems to be important for the rest of the time


##Repeating with 2013
#All grades on first semester seems to be important
plot(MedData2013$X1_T_mGPA~MedData2013$dropout)
plot(MedData2013$X1_Pr_mGPA~MedData2013$dropout)

log2013T <- glm(dropout~X1_T_mGPA, data = MedData2013, family = binomial)
summary(log2013T)
exp(-0.2807)-1

log2013Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2013, family = binomial)
summary(log2013Pr)
exp(-0.18072)-1
#They are both significant. T reduces probability most.

table(MedData2013$X1_T_mGPA,MedData2013$X1_Pr_mGPA)
#They seems to be correlated

log2013all <- glm(dropout~X1_T_mGPA+X1_Pr_mGPA, data = MedData2013, family = binomial)
summary(log2013all)
drop1(log2013all,test = 'Chisq')
#Both significant
#T reduces probability most
#Try again but given they have started on the third semester
MedData2013.3 <- MedData2013[MedData2013$EndSemester>=3,]

plot(MedData2013.3$X1_T_mGPA~MedData2013.3$dropout)
plot(MedData2013.3$X1_Pr_mGPA~MedData2013.3$dropout)

log2013T <- glm(dropout~X1_T_mGPA, data = MedData2013.3, family = binomial)
summary(log2013T)
exp(-0.29887)-1

log2013Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2013.3, family = binomial)
summary(log2013Pr)
exp(-0.17344)-1

#Given they have started on fourth semester
MedData2013.4 <- MedData2013[MedData2013$EndSemester>=4,]

plot(MedData2013.4$X1_T_mGPA~MedData2013.4$dropout)
plot(MedData2013.4$X1_Pr_mGPA~MedData2013.4$dropout)

log2013T <- glm(dropout~X1_T_mGPA, data = MedData2013.4, family = binomial)
summary(log2013T)
exp(-0.29701)-1

log2013Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2013.4, family = binomial)
summary(log2013Pr)
exp(-0.17201)-1

#Given they have started on fifth semester
MedData2013.5 <- MedData2013[MedData2013$EndSemester>=5,]

plot(MedData2013.5$X1_T_mGPA~MedData2013.5$dropout)
plot(MedData2013.5$X1_Pr_mGPA~MedData2013.5$dropout)

log2013T <- glm(dropout~X1_T_mGPA, data = MedData2013.5, family = binomial)
summary(log2013T)
exp(-0.28027)-1

log2013Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2013.5, family = binomial)
summary(log2013Pr)
exp(-0.28578)-1

#Given they have started on sixth semester
MedData2013.6 <- MedData2013[MedData2013$EndSemester>=6,]

plot(MedData2013.6$X1_T_mGPA~MedData2013.6$dropout)
plot(MedData2013.6$X1_Pr_mGPA~MedData2013.6$dropout)

log2013T <- glm(dropout~X1_T_mGPA, data = MedData2013.6, family = binomial)
summary(log2013T)
exp(-0.26217)-1


log2013Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2013.6, family = binomial)
summary(log2013Pr)
exp(-0.17157)-1
##For 2013 the grades on first semester seems to be important for the rest of the time


##Repeating for 2014
plot(MedData2014$X1_T_mGPA~MedData2014$dropout)
plot(MedData2014$X1_NT_mGPA~MedData2014$dropout)
plot(MedData2014$X1_Pr_mGPA~MedData2014$dropout)
#Project does not seem important

log2014T <- glm(dropout~X1_T_mGPA, data = MedData2014, family = binomial)
summary(log2014T)
exp(-0.2289)-1

log2014NT <- glm(dropout~X1_NT_mGPA, data = MedData2014, family = binomial)
summary(log2014NT)
exp(-0.16763)-1

log2014Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2014, family = binomial)
summary(log2014Pr)
exp(-0.1592)-1
#They are all significant. T reduces probability most, but almost the same as NT.
#OBS! Maybe -5 should be coded as NA if interested in effect of grades and not of not showing up.

table(MedData2014$X1_T_mGPA,MedData2014$X1_NT_mGPA)
table(MedData2014$X1_T_mGPA,MedData2014$X1_Pr_mGPA)
table(MedData2014$X1_NT_mGPA,MedData2014$X1_Pr_mGPA)
#They seems to be correlated

log2014all <- glm(dropout~X1_T_mGPA+X1_NT_mGPA+X1_Pr_mGPA, data = MedData2014, family = binomial)
summary(log2014all)
drop1(log2014all,test = 'Chisq')
#All significant
#T reduces probability most

#Try again but given they have started on the third semester
MedData2014.3 <- MedData2014[MedData2014$EndSemester>=3,]

plot(MedData2014.3$X1_T_mGPA~MedData2014.3$dropout)
plot(MedData2014.3$X1_NT_mGPA~MedData2014.3$dropout)
plot(MedData2014.3$X1_Pr_mGPA~MedData2014.3$dropout)

log2014T <- glm(dropout~X1_T_mGPA, data = MedData2014.3, family = binomial)
summary(log2014T)
exp(-0.26592)-1

log2014NT <- glm(dropout~X1_NT_mGPA, data = MedData2014.3, family = binomial)
summary(log2014NT)
exp(-0.27565)-1

log2014Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2014.3, family = binomial)
summary(log2014Pr)
exp(-0.22788)-1

#Given they have started on fourth semester
MedData2014.4 <- MedData2014[MedData2014$EndSemester>=4,]

plot(MedData2014.4$X1_T_mGPA~MedData2014.4$dropout)
plot(MedData2014.4$X1_NT_mGPA~MedData2014.4$dropout)
plot(MedData2014.4$X1_Pr_mGPA~MedData2014.4$dropout)

log2014T <- glm(dropout~X1_T_mGPA, data = MedData2014.4, family = binomial)
summary(log2014T)
exp(-0.24019)-1

log2014NT <- glm(dropout~X1_NT_mGPA, data = MedData2014.4, family = binomial)
summary(log2014NT)
exp(-0.25689)-1

log2014Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2014.4, family = binomial)
summary(log2014Pr)
exp(-0.26173)-1

#Given they have started on fifth semester
MedData2014.5 <- MedData2014[MedData2014$EndSemester>=5,]

plot(MedData2014.5$X1_T_mGPA~MedData2014.5$dropout)
plot(MedData2014.5$X1_NT_mGPA~MedData2014.5$dropout)
plot(MedData2014.5$X1_Pr_mGPA~MedData2014.5$dropout)

log2014T <- glm(dropout~X1_T_mGPA, data = MedData2014.5, family = binomial)
summary(log2014T)
exp(-0.17185)-1

log2014NT <- glm(dropout~X1_NT_mGPA, data = MedData2014.5, family = binomial)
summary(log2014NT)
exp(-0.28681)-1

log2014Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2014.5, family = binomial)
summary(log2014Pr)
exp(-0.2973)-1

#Given they have started on sixth semester
MedData2014.6 <- MedData2014[MedData2014$EndSemester>=6,]

plot(MedData2014.6$X1_T_mGPA~MedData2014.6$dropout)
plot(MedData2014.6$X1_NT_mGPA~MedData2014.6$dropout)
plot(MedData2014.6$X1_Pr_mGPA~MedData2014.6$dropout)

log2014T <- glm(dropout~X1_T_mGPA, data = MedData2014.6, family = binomial)
summary(log2014T)
#Now it isn't significant

log2014NT <- glm(dropout~X1_NT_mGPA, data = MedData2014.6, family = binomial)
summary(log2014NT)
#Again not significant

log2014Pr <- glm(dropout~X1_Pr_mGPA, data = MedData2014.6, family = binomial)
summary(log2014Pr)
#Again not significant

MedData$EndSemester[is.na(MedData$EndSemester)] <- 7
ggplot(aes(factor(X1_T_mGPA),EndSemester, fill=dropout),data = MedData[MedData$EndSemester>=2,])+geom_boxplot()



##Crossvalidation
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
    pred <-ifelse(pred>thres, 1,0)
    pred <- factor(pred)
    CV[i] <- mean(pred==test$dropout)
    tab <- table(factor(pred, levels = c(0,1)),test$dropout)
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
MedData2014nna <- MedData2014[!is.na(MedData2014$X1_T_mGPA)&!is.na(MedData2014$X1_NT_mGPA)&!is.na(MedData2014$X1_Pr_mGPA),]
logCV(log2014all,10,MedData2014nna)



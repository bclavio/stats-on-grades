library(survival)
library(survminer)
library(ggplot2)
encoding <- file('dfEnrolStatusBsc.csv', encoding='UTF8')
write.csv(dfEnrolStatusBsc, file = encoding)

enrolldata <- dfEnrolStatusBsc
#dfEnrolstatusBsc from importDataAndgetInShape.R
enrolldata$slutdatosn[is.na(enrolldata$slutdatosn)] <- '2017-08-03'
enrolldata$Days <- enrolldata$slutdatosn-enrolldata$fradatosn
enrolldata$delta <- as.numeric(enrolldata$statussn=='afbrudt')
enrolldata$deltaInst <- as.numeric(enrolldata$udmeldsn=="Afbrudt af institutionen")
enrolldata$deltaSelv <- as.numeric(enrolldata$udmeldsn=="Afbrudt af den studerende")

enrollYear <- enrolldata[enrolldata$startaar %in% c(2011,2013,2014),]
sur <- survfit(Surv(Days,delta)~startaar, data = enrollYear)
ggsurvplot(sur, conf.int = T)
sur <- survfit(Surv(Days,deltaSelv)~startaar, data = enrollYear)
ggsurvplot(sur, conf.int = T)
sur <- survfit(Surv(Days,deltaInst)~startaar, data = enrollYear)
ggsurvplot(sur, conf.int = T)
#It looks like the changes are for students unrolled by the institution. I may indicate
#that the changes are due to new rules and not the study plan

sem <- 1:10

enroll2011 <- enrolldata[enrolldata$startaar==2011,]
drop2011 <- c()
dropInst2011 <- c()
dropSelv2011 <- c()
for (day in c(152,333,517,698,882,1063,1247,1429,1613,1794)){
  if (day<max(enroll2011$Days)){
  drop2011 <- c(drop2011,mean(enroll2011$statussn=='afbrudt' & enroll2011$Days<day)) 
  dropInst2011 <- c(dropInst2011,mean(enroll2011$udmeldsn=='Afbrudt af institutionen' & enroll2011$Days<day)) 
  dropSelv2011 <- c(dropSelv2011,mean(enroll2011$udmeldsn=='Afbrudt af den studerende' & enroll2011$Days<day)) 
  
  }
}

enroll2012 <- enrolldata[enrolldata$startaar==2012,]
drop2012 <- c()
dropInst2012 <- c()
dropSelv2012 <- c()
for (day in c(152,333,517,698,882,1063,1247,1429,1613,1794)){
  if (day<max(enroll2012$Days)){
    drop2012 <- c(drop2012,mean(enroll2012$statussn=='afbrudt' & enroll2012$Days<day)) 
    dropInst2012 <- c(dropInst2012,mean(enroll2012$udmeldsn=='Afbrudt af institutionen' & enroll2012$Days<day)) 
    dropSelv2012 <- c(dropSelv2012,mean(enroll2012$udmeldsn=='Afbrudt af den studerende' & enroll2012$Days<day)) 
    
  }
}

enroll2013 <- enrolldata[enrolldata$startaar==2013,]
drop2013 <- c()
dropInst2013 <- c()
dropSelv2013 <- c()
for (day in c(152,333,517,698,882,1063,1247,1429,1613,1794)){
  if (day<max(enroll2013$Days)){
    drop2013 <- c(drop2013,mean(enroll2013$statussn=='afbrudt' & enroll2013$Days<day)) 
    dropInst2013 <- c(dropInst2013,mean(enroll2013$udmeldsn=='Afbrudt af institutionen' & enroll2013$Days<day)) 
    dropSelv2013 <- c(dropSelv2013,mean(enroll2013$udmeldsn=='Afbrudt af den studerende' & enroll2013$Days<day)) 
    
  }
}

enroll2014 <- enrolldata[enrolldata$startaar==2014,]
drop2014 <- c()
dropInst2014 <- c()
dropSelv2014 <- c()
for (day in c(152,333,517,698,882,1063,1247,1429,1613,1794)){
  if (day<max(enroll2014$Days)){
    drop2014 <- c(drop2014,mean(enroll2014$statussn=='afbrudt' & enroll2014$Days<day)) 
    dropInst2014 <- c(dropInst2014,mean(enroll2014$udmeldsn=='Afbrudt af institutionen' & enroll2014$Days<day)) 
    dropSelv2014 <- c(dropSelv2014,mean(enroll2014$udmeldsn=='Afbrudt af den studerende' & enroll2014$Days<day)) 
    
  }
}

library(ggplot2)
navne <- c('2011'='red','2012'='green','2013'='purple','2014'='blue')
(ggplot()+ylim(0,1)+ylab('Dropout')+xlab('Semester') + geom_point(aes(factor(sem),drop2011, col='2011')) + geom_line(aes(sem,drop2011, col='2011'))
  + geom_point(aes(factor(sem),drop2012, col='2012')) + geom_line(aes(sem,drop2012, col='2012'))
  + geom_point(aes(factor(1:8),drop2013, col='2013')) + geom_line(aes(1:8,drop2013, col='2013'))
  + geom_point(aes(factor(1:6),drop2014, col='2014')) + geom_line(aes(1:6,drop2014, col='2014'))
  )            

(ggplot()+ylim(0,1)+ylab('Dropout by institution')+xlab('Semester')
  + geom_point(aes(factor(sem),dropInst2011, col='2011')) + geom_line(aes(sem,dropInst2011, col='2011'))
  + geom_point(aes(factor(sem),dropInst2012, col='2012')) + geom_line(aes(sem,dropInst2012, col='2012'))
  + geom_point(aes(factor(1:8),dropInst2013, col='2013')) + geom_line(aes(1:8,dropInst2013, col='2013'))
  + geom_point(aes(factor(1:6),dropInst2014, col='2014')) + geom_line(aes(1:6,dropInst2014, col='2014'))
) 

(ggplot()+ylim(0,1)+ylab('Dropout by student')+xlab('Semester')
  + geom_point(aes(factor(sem),dropSelv2011, col='2011')) + geom_line(aes(sem,dropSelv2011, col='2011'))
  + geom_point(aes(factor(sem),dropSelv2012, col='2012')) + geom_line(aes(sem,dropSelv2012, col='2012'))
  + geom_point(aes(factor(1:8),dropSelv2013, col='2013')) + geom_line(aes(1:8,dropSelv2013, col='2013'))
  + geom_point(aes(factor(1:6),dropSelv2014, col='2014')) + geom_line(aes(1:6,dropSelv2014, col='2014'))
) 

#It does seem to reveal the same pattern. After 2014 most students who are forced to drop
#out drops out on 3. semester and the previous years it was on 5. semester.

##Looking into how many exam trys they have used
enrollMaxExamTry <- merge(enrolldata, dfRetrier, all.x = T)
enrollMaxExamTry <- enrollMaxExamTry[enrollMaxExamTry$startaar %in% c(2011,2012,2013,2014),]

dat <- data.frame(table(enrollMaxExamTry$startaar,enrollMaxExamTry$MaxTry))
count2011 <- sum(dat$Freq[dat$Var1==2011])
count2012 <- sum(dat$Freq[dat$Var1==2012])
count2013 <- sum(dat$Freq[dat$Var1==2013])
count2014 <- sum(dat$Freq[dat$Var1==2014])
dat$Freq[dat$Var1==2011] <- dat$Freq[dat$Var1==2011]/count2011
dat$Freq[dat$Var1==2012] <- dat$Freq[dat$Var1==2012]/count2012
dat$Freq[dat$Var1==2013] <- dat$Freq[dat$Var1==2013]/count2013
dat$Freq[dat$Var1==2014] <- dat$Freq[dat$Var1==2014]/count2014

ggplot(dat, aes(factor(Var2), Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge")


MaxTryInst <- enrollMaxExamTry[enrollMaxExamTry$udmeldsn=='Afbrudt af institutionen',]

datInst <- data.frame(table(MaxTryInst$startaar,MaxTryInst$MaxTry))
count2011 <- sum(datInst$Freq[datInst$Var1==2011])
count2012 <- sum(datInst$Freq[datInst$Var1==2012])
count2013 <- sum(datInst$Freq[datInst$Var1==2013])
count2014 <- sum(datInst$Freq[datInst$Var1==2014])
datInst$Freq[datInst$Var1==2011] <- datInst$Freq[datInst$Var1==2011]/count2011
datInst$Freq[datInst$Var1==2012] <- datInst$Freq[datInst$Var1==2012]/count2012
datInst$Freq[datInst$Var1==2013] <- datInst$Freq[datInst$Var1==2013]/count2013
datInst$Freq[datInst$Var1==2014] <- datInst$Freq[datInst$Var1==2014]/count2014

ggplot(datInst, aes(factor(Var2), Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge")

MaxTrySelv <- enrollMaxExamTry[enrollMaxExamTry$udmeldsn=='Afbrudt af den studerende',]

datSelv <- data.frame(table(MaxTrySelv$startaar,MaxTrySelv$MaxTry))
count2011 <- sum(datSelv$Freq[datSelv$Var1==2011])
count2012 <- sum(datSelv$Freq[datSelv$Var1==2012])
count2013 <- sum(datSelv$Freq[datSelv$Var1==2013])
count2014 <- sum(datSelv$Freq[datSelv$Var1==2014])
datSelv$Freq[datSelv$Var1==2011] <- datSelv$Freq[datSelv$Var1==2011]/count2011
datSelv$Freq[datSelv$Var1==2012] <- datSelv$Freq[datSelv$Var1==2012]/count2012
datSelv$Freq[datSelv$Var1==2013] <- datSelv$Freq[datSelv$Var1==2013]/count2013
datSelv$Freq[datSelv$Var1==2014] <- datSelv$Freq[datSelv$Var1==2014]/count2014

ggplot(datSelv, aes(factor(Var2), Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge")

MaxTryFinished <- enrollMaxExamTry[enrollMaxExamTry$udmeldsn=='Afsluttet',]

datFinished <- data.frame(table(MaxTryFinished$startaar,MaxTryFinished$MaxTry))
count2011 <- sum(datFinished$Freq[datFinished$Var1==2011])
count2012 <- sum(datFinished$Freq[datFinished$Var1==2012])
count2013 <- sum(datFinished$Freq[datFinished$Var1==2013])
count2014 <- sum(datFinished$Freq[datFinished$Var1==2014])
datFinished$Freq[datFinished$Var1==2011] <- datFinished$Freq[datFinished$Var1==2011]/count2011
datFinished$Freq[datFinished$Var1==2012] <- datFinished$Freq[datFinished$Var1==2012]/count2012
datFinished$Freq[datFinished$Var1==2013] <- datFinished$Freq[datFinished$Var1==2013]/count2013
datFinished$Freq[datFinished$Var1==2014] <- datFinished$Freq[datFinished$Var1==2014]/count2014

ggplot(datFinished, aes(factor(Var2), Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge")

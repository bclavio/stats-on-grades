library(survival)
library(survminer)

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





myWD<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/stats-on-grades'} else {"~/git/AAU/DropOutProject/analysis/"}
setwd(myWD)
#when downloading from Qlikview remember to remove last three lines and upload download as cvs from google docs
source('importDataAndgetInShape.R')

### Comment: I get a warning that SD is 0 and corrplot prints no numbers:
M <- cor(dfMed2AalX)
corrplot(M, method="circle")

moddfMed2Aal.form<- "isDropOut~ MATGrade + jobHoursPerWeek +
ParentsEduMax +
ParentsEduAvg +
MedHappyWith +
MedBelongHere +
WantMScDeg"

dropOutModeldfMed2AalGLM<- glm(moddfMed2Aal.form ,dfMed2AalX,family=binomial())
summary(step(dropOutModeldfMed2AalGLM))
summary(dropOutModeldfMed2AalGLM)
step(dropOutModeldfMed2AalGLM)
summary(dropOutModeldfMed2AalGLM)

mean(dfMed2Aal$hoursWorkedPerWeek,na.rm = TRUE)
mean(dfMed2Aal[dfMed2Aal$DropOutQ999Combo==1,]$hoursWorkedPerWeek,na.rm = TRUE)
numCols <- sapply(dfMed2Aal, is.numeric)

### R suggests using `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
med2DOOverview<-dfMed2Aal[,numCols] %>% group_by(DropOutQ999Combo) %>% summarise_each(funs(mean(.,na.rm=T)))
#predict cohort 2016 (in May 2017 with data up to Feb/Mar)
#glm()

#XXXXXXXXXXXXXXXXXXXX
# remove?? ----------------------------------------------------------------


#### OLD 

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/dropOut/data'} else {"~/SVN/01Projects/dropOut/data/"}
setwd(SVNData)

dfUD1 <-read.csv("RawDataOnlyUD1Engl.csv", header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")
dfUD1$status<-dfUD1$status2015
dfUD1$statusYear<-2015
df2016<-read.csv("RawDataOnlyUD1Engl2016.csv", header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8")
df2016$status<-df2016$status2016
df2016$statusYear<-2016
df2016<-plyr::rename(df2016,c("cprnr"="cprnr", "optag_aar"="yearOfEnrolment", "efter_adgeksamen"="delayAfterGraduationFromGymnEtc", "ADGGRU"="ADGGRU", "kvotient"="waitTimeAdjustedGradeInclBonus", "geobag"="residenceBeforeEnrolment", "Aldop"="ageAtEnrolment", "NAVN"="FullName", "postnr"="zip", "geoinst"="GraduationSchoolArea", "institution"="graduationSchool", "campus"="campus", "kon"="gender", "type_optag"="degreeEnrolledFor", "ramme_retning_optag"="studyDirectionAtUniEnrolment", "ramme_2016"="studyDirectionInYear", "orlov2016"="studyLeaveInYear", "studienaevn2016"="studyboardResponsibleInYear", "MAT"="mathGrade", "Niveau_MAT"="mathLevel", "DAN"="DanishGrade", "Niveau_DAN"="DanishLevel", "ENG"="EnglishGrade", "NIveau_ENG"="EnglishLevel", "staa"="staa"
))

# can go? -----------------------------------------------------------------


dfUD1<-rbind.fill(dfUD1,df2016)
dfUD1$campus<-factor(dfUD1$campus,levels=c("Aalborg","Kbh.","Esbjerg"))
#dfUD1$isDropOut<-ifelse(dfUD1$status2015="Afbrudt",1,0)
lookupDropOutsVector=c(Afsluttet= 0, Afbrudt=1, Indskrevet=0, 'Afbrudt (School skift)'=1,'Afbrudt(Fak skift)'=1,'Afbrudt (SN skift)'=1)
lookupDropOutsiUniVector=c(Afsluttet= 0, Afbrudt=1, Indskrevet=0, 'Afbrudt (School skift)'=0,'Afbrudt(Fak skift)'=0,'Afbrudt (SN skift)'=0)
dfUD1$isDropOutButInUni<-lookupDropOutsiUniVector[as.character(dfUD1$status)]
dfUD1$isDropOut<-lookupDropOutsVector[as.character(dfUD1$status)]

dfUD1$isInternationalStudent<-ifelse(dfUD1$GraduationSchoolArea=="Ikke Danmark",1,0)
#dfUD1$yearsFromEnrolment<-2015-dfUD1$yearOfEnrolment
dfUD1$mathGradeBinned<-cut(dfUD1$mathGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18))
dfUD1$mathGradeBinHighGran<-cut(dfUD1$mathGrade,breaks=c(-6,-1,1.5,3,4.5,5.5,6.5,7.5,8.5,11,18))
dfUD1<-dfUD1[!is.na(dfUD1$mathGrade),]
dfUD1$mathLevelABC<-dfUD1$mathLevel
dfUD1$mathLevel<-ifelse(dfUD1$mathLevel %in% c("B","C"),"B","A" )
#super slow dfUD1 <- read.xlsx("RawDataOnly.xlsx", sheetName="UDDATA-1")
#dfUD2 <- read.xlsx("RawDataOnly.xlsx", sheetName="UDDATA-2")
#dfCourseGrades <- read.xlsx("RawDataOnly.xlsx", sheetName="UDDATA-3")
dfCG <-read.csv("RawDataOnlyUD3-googleDocs.csv", header = TRUE, fill=TRUE, sep = ",",fileEncoding = "UTF-8",check.names = FALSE)
dfCG$Kvotient<-NULL


#XXXXXXXXXXXXXXXXXXXX

#XXXXXXXXXXXXXXXXXXX
# keep figure prduction --------------------------------------------------------------------

### Comment: "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?"
dropOutByMathGradeByCampusBy<-sqldf("select mathGradeBinned,  mathLevel, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where mathLevel<>''  group by   mathLevel,mathGradeBinned")
ggplot(dropOutByMathGradeByCampusBy,aes(mathGradeBinned,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% dropped out by 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)#+facet_grid(. ~ campus)

#p<-predict(dropOutModel,newdata=test,type="response")


# dropModel only for complete years 2011/12/13 ---------------------------------------------------------------
### Comment: dfM3y not found
myvars<-names(dfM3y) %in% c("isDropOut","MAT_Niveau", "MATGrade")
dataForModel<-dfM3y[myvars]
dfM3y<-dfM[dfM$startaar %in% c(2011,2012,2013) & !is.na(dfM$isDropOut) ,]
dfM3y$MAT_Niveau<-as.factor(dfM3y$MAT_Niveau)
dfM3y$ENG_Niveau<-as.factor(dfM3y$ENG_Niveau)
dfM3y$DAN_Niveau<-as.factor(dfM3y$DAN_Niveau)
dfM3y$DANGradeX<-ifelse(is.na(dfM3y$DANGrade),dfM3y$ENGGrade,dfM3y$DANGrade)
#dfM3y<-dfM3y[!is.na(dfM3y$isDropOut),]

#find out when mandatory enrolment to exams happened
#dfTemp<-

# lm model with all sorts of vars -----------------------------------------


dropOutModel<- glm(isDropOut ~ #
                     #mathGrade + 
                     MAT_Niveau
                   * MATGrade
                   + ENG_Niveau* ENGGrade
                   + DANGradeX
                   #+ GPRO_PassedBy1stAttempt
                   #+ GPRO_PassedBy2ndAttempt
                   #+ MMA_PassedBy1stAttempt
                   #+ MMA_PassedBy2ndAttempt
                   #+ PFI_PassedBy1stAttempt
                   #+ PFI_PassedBy2ndAttempt
                   #mathLevel 
                   #+ EnglishGrade
                   #+ EnglishLevel
                   #+ DanishGrade
                   #+ yearsFromEnrolment
                   #+ADGGRU
                   +campus 
                   #+ gender 
                   +isIntl
                   ,dfM3y,family=binomial())
#,dfM[dfM$yearOfEnrolment== & dfM$campus=="Aalborg" ,])
#,dfM[dfM$startaar %in% c(2011,2012,2013) ,])
summary(dropOutModel)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           1.044795   0.086623  12.061  < 2e-16 ***
#   MAT_NiveauB           0.092581   0.081301   1.139 0.254811    
# MATGrade             -0.150067   0.011331 -13.244  < 2e-16 ***
#   ENG_NiveauB           0.189702   0.046480   4.081 4.48e-05 ***
#   ENGGrade             -0.029819   0.009331  -3.196 0.001396 ** 
#   DANGrade             -0.147841   0.011416 -12.950  < 2e-16 ***
#   campusKbh.           -0.365480   0.046271  -7.899 2.82e-15 ***
#   campusEsbjerg         0.164903   0.076888   2.145 0.031975 *  
#   isIntl               -0.386010   0.114862  -3.361 0.000778 ***
#   MAT_NiveauB:MATGrade  0.129486   0.014788   8.756  < 2e-16 ***

# proper model testing GLM based ----------------------------------------------------
mod.form<-"isDropOut ~MAT_Niveau + MATGrade + DANGradeX + ENGGrade + campus + MAT_Niveau:MATGrade"
dropOutModelGLM<- glm(mod.form ,dfM3y,family=binomial())
summary(dropOutModelGLM)
step(glm(isDropOut~1 ,data=dfM3y,family=binomial()),scope="~MAT_Niveau + MATGrade + DANGradeX + ENGGrade + campus + MAT_Niveau:MATGrade",direction = "forward")
#campus has does not sign. predict  dropout

indx <- apply(dfM3y, 2, function(x) any(is.na(x)))
colnames[indx]
apply(dfM3y, 2, function(x) any(is.na(x)))

mod.formNonDan<-"isDropOut ~(MAT_Niveau*MATGrade)"
dropOutModelGLMNonDan<- glm(mod.formNonDan ,dfM3y[is.na(dfM3y$DANGrade),],family=binomial())
summary(dropOutModelGLMNonDan)
#for non-Danes (no Dan grades) the matgrade and MATH A B have no predictive power on dropout (motivated?)

dropOutNullModelGLM<-glm(isDropOut~1, dfM3y,family=binomial())
# MATGrade -0.15 , DanGrade -.13 and MATB:MATGrade 0.13 are sign. pred. of drop-out
#before taking exams at Medialogy

#following Andy Field book here page 332
modelCHI<-dropOutModelGLM$null.deviance-dropOutModelGLM$deviance
chidf<-dropOutModelGLM$df.null-dropOutModelGLM$df.residual
chisq.prob<-1-pchisq(modelCHI,chidf)
chisq.prob

dfM3y$predicted.prob<-fitted(dropOutModelGLM)
#predict drop out semester
dfM3ypid<-dfM[dfM$startaar %in% c(2012,2013) & !is.na(dfM$isDropOut) ,]

mod.form2<-"isDropOut ~ MMA_1+GPRO_2+PID_2"
mod.form2<-"MMA_1~MATGrade*MAT_Niveau"
mod.form2<-"GPRO_1~MATGrade*MAT_Niveau+DANGrade"

dropOutModelGLMpid<- glm(mod.form2 ,dfM,family=binomial())
#mod.form2<-"isDropOut ~ (MAT_Niveau*MATGrade)+MMA_1+GPRO_1+PID_1"

summary(dropOutModelGLMpid)

sqldf("select aktivitetshort, takenInYear, avg(isPassed) from dfAAUGrades where `Forsoeg.nr.`=1 group by aktivitetShort, takenInYear")
dfAAUGrades$
  dfM3y$`A+2`<-NULL

mod.PF1<-"isDropOut ~ MAT_Niveau  * MATGrade"
mod.GPRO1<-
  mod.MMA1

#,dfM[dfM$yearOfEnrolment== & dfM$campus=="Aalborg" ,])
#,dfM[dfM$startaar %in% c(2011,2012,2013) ,])



anova(dropOutModelGLM,test="Chisq")

# ROC of Model ------------------------------------------------------------
dfM3y$predictedDO<-predict(dropOutModelGLM,type = "response")
pr<-prediction(dfM3y$predictedDO,dfM3y$isDropOut)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0,1)

#run all models for dropout predicition
hurdleList<-c("MMA_1","MMA_2","GPRO_1","GPRO_2","PFI_1","PFI_2")
#average passing grade of re-exam (2nd) to check Martin's assumption hypothesis: grade rather high (maybe correlate with entry grades mathA/B)
#check average drop-out semester after 2nd semester, higher vs smaller.


modpfi.form<-"isDropOut ~GPRO_1+PFI_1+MMA_1"
dropOutModelGLMpfi<- glm(modpfi.form ,dfPFI,family=binomial())
summary(dropOutModelGLMpfi)

# GPRO_2       -1.8110     0.7840  -2.310 0.020897 *  
# PFI_2        -3.0485     0.8023  -3.800 0.000145 ***
# MMA_2        -1.1446     0.6067  -1.887 0.059225 .  


#correlation between the courses
pairs(~GPRO_1+GPRO_2+MMA_1+MMA_2+PFI_1+PFI_2,data=dfPFI, 
      main="Simple Scatterplot Matrix")

pairs(~GPRO_1+GPRO_2+MMA_1+MMA_2+PFI_1+PFI_2,data=dfPFI, 
      main="Simple Scatterplot Matrix")

M<-cor(dfPFI[,c("GPRO_1","GPRO_2","PFI_1","PFI_2","MMA_1","MMA_2")],)
corrplot(M,method="ellipse")


# further plotting --------------------------------------------------------


dropOutByMathGradeAll<-sqldf("select mathGradeBinned, mathLevel, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where mathLevel<>''  and startaar in (2011,2012,2013)  group by  mathLevel,mathGradeBinned")
ggplot(dropOutByMathGradeAll,aes(mathGradeBinned,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% dropped out by 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)
ggsave("dropOutEngGradesANdLevels'11-'13cohorts.png",width=10,height = 7.3)
ggplot(dropOutByMathGradeAll,aes(mathGradeBinned,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% dropped out by 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)

dropOutByMathGradeAllHighGran<-sqldf("select mathGradeBinHighGran, mathLevel, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where mathLevel<>''  and startaar in (2011,2012,2013)  group by  mathLevel,mathGradeBinHighGran")
ggplot(dropOutByMathGradeAllHighGran,aes(mathGradeBinHighGran,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% dropped out by Mar 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)

dropOutByEngGradeAllHighGran<-sqldf("select ENGGradeBinned, ENG_Niveau, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where ENG_Niveau<>''  and startaar in (2011,2012,2013)  group by  ENG_Niveau, ENGGradeBinned")
ggplot(dropOutByEngGradeAllHighGran,aes(ENGGradeBinned,dropOutPct*100,colour=ENG_Niveau))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% dropped out by Mar 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)
dropOutByEngGrade<-sqldf("select ENGGradeBinned,  avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where ENG_Niveau<>''  and startaar in (2011,2012,2013)  group by   ENGGradeBinned")
ggplot(dropOutByEngGrade,aes(ENGGradeBinned,dropOutPct*100))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% dropped out by Mar 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)
ggsave("dropOutEngGradesANdLevels.png")

#MATH grades vs. MMA grades
dropOutByMathGradeAllHighGran<-sqldf("select mathGradeBinHighGran, mathLevel, avg(MMA_FinExamGrade) as avgFinalMathExamGrade, count(campus) as CountOfStudents from dfM where mathLevel<>''  and startaar in (2011,2012,2013)  group by  mathLevel,mathGradeBinHighGran")
ggplot(dropOutByMathGradeAllHighGran,aes(mathGradeBinHighGran,avgFinalMathExamGrade,colour=mathLevel))+theme_bw()+geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("MMA final exam grade")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)

#correlation of math grades with MMA grades
dfMGr<-dfM[!duplicated(dfM$studienr) & dfM$startaar %in% c(2011,2012,2013),]
cor.test(dfMGr[dfMGr$MAT_Niveau=="A",]$MATGrade,dfMGr[dfMGr$MAT_Niveau=="A",]$MMA_Lg,use="complete")
cor.test(dfMGr[dfMGr$MAT_Niveau=="B",]$MATGrade,dfMGr[dfMGr$MAT_Niveau=="B",]$MMA_Lg,use="complete")

sum(dropOutByMathGradeAllHighGran$CountOfStudents)
sum(dropOutByMathGradeAll$CountOfStudents)
dropOutByMathGradeByCampusBy<-sqldf("select mathGradeBinned, campus, mathLevel, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where mathLevel<>''  group by  campus, mathLevel,mathGradeBinned")
ggplot(dropOutByMathGradeByCampusBy,aes(mathGradeBinned,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% of cohort dropped out by 2017")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)+facet_grid(. ~ campus)

dropOutByMathGrade<-sqldf("select mathGradeBinned, mathLevel, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfMGr where mathLevel<>'' and yearOfEnrolment=2012 group by  mathLevel,mathGradeBinned")
ggplot(dropOutByMathGrade,aes(mathGradeBinned,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10)) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% of cohort dropped out by 2015")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)


# data again --------------------------------------------------------------


### Comment: Do we want the figure in dropbox or in git?
#myWD2<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/stats-on-grades/output'} else {"~/git/AAU/DropOutProject/analysis/output"}
#setwd(myWD2)
myWD3 <- ifelse(grepl("BiancaClavio", getwd()), 'C:/Users/BiancaClavio/Dropbox/Apps/ShareLatex/MedialogyBSc re-design/figures', '~/Dropbox/Apps/ShareLatex/MedialogyBSc re-design/figures')
setwd(myWD3)



#dfCG$campus<-factor(dfCG$campus,levels=c("Aalborg","Kbh.","Esbjerg"))
# dfCG$isDropOut<-ifelse(dfCG$status2015=="Afbrudt",1,0)
dfCG<-plyr::rename(dfCG, c(isLastEligibleExamAttempt="isLastAttemptAtExam",ExamGradeNum="FirstExamGradeNum"))
dfCG$isNoShow<-ifelse(dfCG$ExamGradeText=="U",1,0)
dfCG$isNumericGrade<-!is.na(as.numeric(levels(dfCG$ExamGradeText))[dfCG$ExamGradeText])
dfCG<-dfCG[!is.na(dfCG$examAttempt),]
dfCG$examGradeNumeric<-ifelse(dfCG$isNumericGrade,as.numeric(levels(dfCG$ExamGradeText))[dfCG$ExamGradeText],NA)
dfCG$passed<-ifelse(dfCG$examGradeNumeric<=0,0,1)
dfCG$Kvotient<-NULL

#dfCG$isDropOut<-ifelse(dfCG)
gradesPassedLUVec<-c('02'=1,'4'=1,'7'=1,'10'=1,'12'=1,'00'=0,'-3'=0,'B'=1,'EB'=-1,'U'=-1,'I'=-1)
monthsLookupVector <-c('< 0 md'=0,'6 md'=6,'12 md'=12,'18 md'=18,'24 md'=24, '30 md'=30, '36 md'=36, '42 md'=42)
dfCG$monthsIntoStudy<-monthsLookupVector[as.character(dfCG$takenInWhichSemesterInMonths)]
latestCommittment<-sqldf("select cprnr, max(monthsIntoStudy) as LatestExam from dfCG where passed in (0,1) group by cprnr ")
latestCommittment$LatestExamInSem<-ifelse(is.na(latestCommittment$LatestExam),0,latestCommittment$LatestExam)/6
finalFails<-sqldf("select cprnr, max(monthsIntoStudy) as FinalFailmonthsIntoStudy,1 as failedLast from dfCG where passed=0 and isLastAttemptAtExam='Ja' group by cprnr, failedLast") 
latestCommittment<-merge(latestCommittment,finalFails,by="cprnr",all.x=TRUE)


#OLD dfM<-dfUD1[dfUD1$studyDirectionAtUniEnrolment=="Medialogi",]
minYear=2011
#min(dfM$startaar,na.rm = TRUE)
maxYear=max(dfM$startaar,na.rm = TRUE)
dropOutByCampusByYear<-dfM %>% group_by(campus,startaar)%>%summarise(mean=mean(isDropOut))
dropOutByCampusByYearSQLDF<-sqldf("select campus, startaar, avg(isDropOut) as mean from dfM group by campus, startaar")
dropOutByCampusByYear<-sqldf("select startaar, campus, mathLevel, avg(isDropOut) as dropOutPct, count(campus) as CountOfStudents from dfM where mathLevel<>'' group by startaar, campus, mathLevel")
ggplot(dropOutByCampusByYear,aes(startaar,dropOutPct*100,colour=mathLevel))+theme_bw()+scale_y_continuous(limits=c(20,80),breaks=seq(0,100,10))+scale_x_continuous(limits=c(minYear,maxYear),breaks = minYear:maxYear) +geom_point(aes(size=CountOfStudents,alpha=.5))+geom_line()+ylab("% of cohort dropped out by 2015")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)+facet_grid(. ~ campus)
ggsave("DropOutByCampusByYear.png",width=9.71,height=8)


dfUD1<-merge(dfUD1,latestCommittment,by="cprnr")


semesterScaffold<-data.frame(semesterNum= 0:8)
dfMBlowUp<-sqldf("select * from dfM, semesterScaffold")
dfMBlowUp$isOddSemester<-is.odd(dfMBlowUp$semesterNum)
dfMBlowUp$YearsToAdd<-ceiling(dfMBlowUp$semesterNum/2)
dfMBlowUp$SemCutOffDate<-as.Date(ifelse(dfMBlowUp$isOddSemester,  paste(as.character(dfMBlowUp$startaar+dfMBlowUp$YearsToAdd),"/2/1", sep = ""),
                                        paste(as.character(dfMBlowUp$startaar+dfMBlowUp$YearsToAdd),"/9/1", sep = "")), format="%Y/%m/%d")
dfMBlowUp$IsDropOutInSem <-ifelse(is.na(dfMBlowUp$slutdatosn),0,ifelse(dfMBlowUp$slutdatosn < dfMBlowUp$SemCutOffDate,dfMBlowUp$isDropOut,0))
dfMBlowUp<-dfMBlowUp[dfMBlowUp$SemCutOffDate<=as.Date("2017/3/1",format="%Y/%m/%d"),]

#dfMBlowUp<-dfMBlowUp[dfMBlowUp$semesterNum<= (2017-dfMBlowUp$startaar)*2,]


# more plotting  ----------------------------------------------------------


#grade correlations
plot(jitter(dfPFI[dfPFI$campus=="Aalborg"]$GPRO_1g,1),jitter(dfPFI[dfPFI$campus=="Aalborg"]$PFI_Lg,1))
plot(jitter(dfPFI[dfPFI$campus=="Aalborg",]$GPRO_1g,1),jitter(dfPFI[dfPFI$campus=="Aalborg",]$PFI_Lg,1))
plot(jitter(dfPFI[dfPFI$campus=="Kbh.",]$GPRO_1g,1),jitter(dfPFI[dfPFI$campus=="Kbh.",]$PFI_Lg,1))
plot(jitter(dfPFI[dfPFI$campus=="Aalborg",]$GPRO_1g,1),jitter(dfPFI[dfPFI$campus=="Aalborg",]$PFI_Lg,1))
plot(jitter(dfPFI[dfPFI$campus=="Kbh.",]$GPRO_1g,1),jitter(dfPFI[dfPFI$campus=="Kbh.",]$PFI_Lg,1))
plot(jitter(dfM[dfM$campus=="Kbh.",]$GPRO_1g,1),jitter(dfM[dfM$campus=="Kbh.",]$MMA_Lg,1))
plot(jitter(dfM[dfM$campus=="Aalborg",]$GPRO_1g,1),jitter(dfM[dfM$campus=="Aalborg",]$MMA_Lg,1))
plot(jitter(dfM[dfM$campus=="Aalborg",]$GPRO_1g,1),jitter(dfM[dfM$campus=="Aalborg",]$GPRO_Lg,1))
plot(jitter(dfM[dfM$campus=="Kbh.",]$GPRO_1g,1),jitter(dfM[dfM$campus=="Kbh.",]$GPRO_Lg,1))

plot(jitter(dfPFI$GPRO_Lg,1),jitter(dfPFI$PFI_Lg,1))

z1<-lm(PFI_Lg~GPRO_Lg,data = dfPFI)
abline(z1)
z1

cor(dfPFI$PFI_Lg,dfPFI$GPRO_Lg, use = "complete.obs")
z<-lm(PFI_Lg~GPRO_Lg,data = dfPFI)
abline(z)


dfMBlowUp$cohort<-as.factor(dfMBlowUp$startaar)
dfDbSem<-sqldf("select cohort,campus ,semesternum, mathlevel, avg(IsDropOutInSem) as dropOutPct, count(studienr) as numOfStudents from dfMBlowUp where isDropOutInSem in (0,1) and cohort in (2011,2012,2013,2014,2015,2016) group by  cohort, campus, semesterNum, mathlevel")
ggplot(dfDbSem[!dfDbSem$mathLevel=="C" & !dfDbSem$campus=="Esbjerg",],aes(semesterNum,dropOutPct*100,colour=cohort))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+scale_x_continuous(limits=c(0,8)) +geom_point()+geom_line()+ylab("% of cohort dropped out")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)+facet_grid(. ~ campus*mathLevel)
ggsave("DropOutByCampusBySemesterByCohortByMathAB.png",width=9.71,height=5)

dfDbSemCamp<-sqldf("select cohort,campus ,semesternum,  avg(IsDropOutInSem) as dropOutPct, count(studienr) as numOfStudents from dfMBlowUp where isDropOutInSem in (0,1) and cohort in (2011,2012,2013,2014,2015,2016) group by  cohort, campus, semesterNum")
ggplot(dfDbSemCamp[ !dfDbSemCamp$campus=="Esbjerg",],aes(semesterNum,dropOutPct*100,colour=cohort))+theme_bw()+scale_y_continuous(limits=c(0,70),breaks=seq(0,100,10))+scale_x_continuous(limits=c(0,8)) +geom_point()+geom_line()+ylab("% of cohort dropped out")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE)+facet_grid(. ~ campus)
ggsave("DropOutByCampusBySemesterByCohort.png",width=9.71,height=5)

#now only math levels 
dfDbSem<-sqldf("select  semesternum, mathlevel, mathGradeBinned, avg(IsDropOutInSem) as dropOutPct, count(studienr) as numOfStudents from dfMBlowUp where isDropOutInSem in (0,1) and startaar in (2011,2012,2013) group by  mathGradeBinned, semesterNum, mathlevel")

ggplot(dfDbSem[!dfDbSem$mathLevel=="C" ,],aes(semesterNum,dropOutPct*100,colour=mathGradeBinned))+theme_bw()+scale_y_continuous(limits=c(0,60),breaks=seq(0,60,10))+scale_x_continuous(limits=c(0,7)) +geom_point()+geom_line()+ylab("% of cohort dropped out")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"),plot.margin=unit(c(0,1,0,0),"lines")  )+guides(alpha=FALSE) +facet_grid(. ~ mathLevel) 
ggsave("DropOutBySemesterByMathLevel.png",width=9.71,height=5)

inDistributionByMathLevels<-sqldf("select mathLevel, count(studienr) from dfM where mathlevel<>'' group by mathLevel")

MathEnrolmentByCampusByYear<-dfM[dfM$mathLevel!='',] %>% group_by(campus,startaar,mathLevel) %>% summarise (n = n()) %>% mutate(freq = n / sum(n))
ggplot(MathEnrolmentByCampusByYear,aes(startaar,freq*100,colour=mathLevel))+theme_bw()+theme(panel.spacing = unit(2, "lines"))+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+scale_x_continuous(limits=c(minYear,maxYear),breaks = minYear:maxYear) +geom_point()+geom_line()+ylab("percentage of enrolling students")+facet_grid(. ~ campus)
ggsave("MathEnrolmentByCampusByYear.png",width=6.71,height=2.5)

intStudEnrolmentByCampusByYear<-dfM[dfM$startaar>=2011 & !is.na(dfM$isIntl),] %>% group_by(campus,startaar,isIntl) %>%   summarise (n = n()) %>% mutate(freq = n / sum(n))
intStudEnrolmentByYear<-dfM[dfM$startaar>=2011 & !is.na(dfM$isIntl),] %>% group_by(startaar,isIntl) %>%   summarise (n = n()) %>% mutate(freq = n / sum(n))
ggplot(intStudEnrolmentByCampusByYear,aes(startaar,freq*100,colour=factor(isIntl)))+theme_bw()+theme(panel.spacing = unit(2, "lines"))+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+scale_x_continuous(limits=c(minYear,maxYear),breaks = minYear:maxYear) +geom_point()+geom_line()+ylab("percentage of enrolling students")+facet_grid(. ~ campus)+
  theme(panel.spacing = unit(2, "lines"),strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),plot.margin = unit( c(0,0,0,0) , units = "lines" ),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16) )
ggplot(intStudEnrolmentByYear,aes(startaar,freq*100,colour=factor(isIntl)))+theme_bw()+theme(panel.spacing = unit(2, "lines"))+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+scale_x_continuous(limits=c(minYear,maxYear),breaks = minYear:maxYear) +geom_point()+geom_line()+ylab("percentage of enrolling students")+
  theme(panel.spacing = unit(2, "lines"),strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),plot.margin = unit( c(0,0,0,0) , units = "lines" ),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16) )
ggsave("intStudEnrolmentByYear.png",width=8.71,height=3.5)

AvgMathGradeByCampusByYear<-sqldf("select campus, startaar, MAT_Niveau, avg(MATGRade) as avgMATGRade from dfM  where MAT_Niveau<>'' and MAT_Niveau<>'C' group by campus, startaar, MAT_Niveau ")
ggplot(AvgMathGradeByCampusByYear[AvgMathGradeByCampusByYear$MAT_Niveau!='',],aes(startaar, avgMATGRade ,colour=factor(MAT_Niveau)))+theme_bw()+scale_x_continuous(limits=c(minYear,maxYear),breaks = minYear:maxYear) +geom_point()+geom_line()+ylab("math grade avg")+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),plot.margin = unit( c(0,0,0,0) , units = "lines" ),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16) , panel.spacing = unit(2, "lines") )+facet_grid(. ~ campus)
ggsave("MathGradeByCampusByYear.png",width=6.71,height=3.5)

intStudEnrolmentByCampusByYear<-dfM %>% group_by(campus,startaar,isInternationalStudent) %>%   summarise (n = n()) %>% mutate(freq = n / sum(n))

genderStudEnrolmentByCampusByYear<-dfM %>%   group_by(campus,startaar,gender) %>% summarise (n = n()) %>% mutate(freq = n / sum(n))
ggplot(genderStudEnrolmentByCampusByYear,aes(startaar,freq*100,colour=gender))+theme_bw()+scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+scale_x_continuous(limits=c(minYear,maxYear),breaks = minYear:maxYear) +geom_point()+geom_line()+ylab("percentage of enrolling students")+facet_grid(. ~ campus)
ggsave("genderStudEnrolmentByCampusByYear.png")


dfCG<-merge(dfUD1,dfCG,by="cprnr")

#sqldf("select GraduationSchoolarea,count(zip) from dfM group by GraduationSchoolarea ")
dfCGM<-dfCG[dfCG$studyDirectionAtUniEnrolment=="Medialogi",]

dfGPRO<-dfCGM[dfCGM$activityName=="Grundlæggende programmering",]
dfMMA<-dfCGM[dfCGM$activityName=="Matematik til multimedie-applikationer",]
#dfPFI<-dfCGM[dfCGM$activityName=="Grundlæggende programmering",]


dfCGlast<-dfCG[dfCG$isLastAttemptAtExam=="Ja",-(15:22)]
#use also for no-show
dfCGfirst<-dfCG[dfCG$examAttempt==1,-(15:22)]


#columns rød and år are not reading... need to change manually in CSV file then re-import after export from gdocs spreadsheet
df<-dfUD1 
#merge(dfUD1,dfUD2,by="cprnr") #only import ECTS column

#for prior grade analysis remove all NAs from prior grades no show/

#dfCG$mathGradeBinned<-cut(dfCG$mathGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18))

#dfGPRO$mathGradeBinned<-cut(dfGPRO$mathGrade,breaks=c(-6,-1,1.5,3,5.5,8.5,11,18))

#replace(dfGPRO$mathGradeBinnedText, dfGPRO$mathGradeBinnedText== c("(-6,1.5]", "(1.5,3]", "(3,5.5]","(5.5,8.5]","(8.5,11]", "(11,18]"), c(0,2,4,7,10,12))
lookUpGradesVector=c('(-6,-1]'="-2", '(-1,1.5]'="0", '(1.5,3]'="2", '(3,5.5]'="4",'(5.5,8.5]'="7",'(8.5,11]'="10", '(11,18]'="12")
dfGPRO$mathGradeBinnedNum<-as.numeric(lookUpGradesVector[dfGPRO$mathGradeBinned])
dfGPRO<-dfGPRO[!is.na(dfGPRO$mathGrade),]

#dflastExamAttempts<- sqldf("select cprnr, activityName, examattempt,examGradeNumeric from dfCGM where isLastAttemptAtExam='Ja'")
#dfFirstExamAttempts<-sqldf("select cprnr, activityName, examattempt,examGradeNumeric from dfCGM where examattempt=1 and ")

dfNoShowRisk<-sqldf("select avg(isNoShow), count(mathgrade) as numOfStudents, mathGrade,mathlevel from dfGPRO group by mathgrade, mathlevel");dfNoShowRisk
ggplot(dfGPRO[dfGPRO$isNoShow==1,], aes(x=mathGrade,colour=mathLevel)) + geom_density()
mathGPROpass<-sqldf("select mathLevel,mathGradeBinnedNum,avg(passed) as probabilityPassingGPROmed1,count(mathgrade) as numOfStudents  from dfGPRO where isLastAttemptAtExam='Ja' group by mathGradeBinnedNum, mathLevel order by mathlevel, mathGradeBinnedNum")
#mathPFIpass<-sqldf("select mathLevel,mathGradeBinnedNum,avg(passed) as probabilityPassingGPROmed1,count(mathgrade) as numOfStudents  from dfAAUGrades where isLastAttemptAtExam='Ja' group by mathGradeBinnedNum, mathLevel order by mathlevel, mathGradeBinnedNum")

mathGPROnoShow<-sqldf("select mathLevel,mathGradeBinnedNum,avg(isNoShow) as probabilityNoShowGPROmed1,count(mathgrade) as numOfStudents from dfGPRO group by mathGradeBinnedNum, mathLevel order by mathlevel, mathGradeBinnedNum")


ggplot(mathGPROpass,aes(mathGradeBinnedNum,probabilityPassingGPROmed1,colour=mathLevel))+theme_bw()+geom_point()+geom_line()+ylim(0,1)+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),plot.margin = unit( c(0,0,0,0) , units = "lines" ),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16),panel.spacing = unit(2, "lines"))
ggsave("mathGPROpass.png",width=3.5,height=10.5)

ggplot(mathGPROnoShow,aes(mathGradeBinnedNum,probabilityNoShowGPROmed1,colour=mathLevel))+theme_bw()+geom_point(aes(size=numOfStudents))+geom_line()+ylim(0,0.5)+theme(strip.text.x = element_text(size = 18),legend.position = "bottom",panel.background=element_rect(fill = "white",color = "white"),plot.margin = unit( c(0,0,0,0) , units = "lines" ),  axis.text.x  = element_text(size=16),axis.text.y  = element_text(size=16), panel.spacing = unit(2, "lines"))
ggsave("mathGPROnoShow.png",width=3.5,height=6.5)

#hist_cut + geom_bar(position="dodge")

sqldf("select activityName, examAttempt, avg(isNoShow) from dfCGM where activityCode='NJA110006L' group by activityName, examAttempt")
sqldf("select distinct activityName from dfCGM group by activityName")

ggplot(dfGPRO,aes(mathGrade,FirstExamGradeNum))+geom_point(aes(colour=mathLevel,alpha=0.05))

df$Mat7<-ifelse(df$mathGrade<7,0,1)
dfM$Mat7<-ifelse(dfM$mathGrade<7,0,1)

dropOutModel<- lm(isDropOut ~ #
                    #mathGrade + 
                    MAT_Niveau
                  * MATGrade
                  #+ ENG_Niveau
                  #+ ENGGrade
                  #+ DAN_Niveau
                  #+ DANGrade
                  #+GPRO_PassedBy1stAttempt
                  #+GPRO_PassedBy2ndAttempt
                  #+MMA_PassedBy1stAttempt
                  #+MMA_PassedBy2ndAttempt
                  # +PFI_PassedBy1stAttempt
                  # +PFI_PassedBy2ndAttempt
                  #mathLevel 
                  #+ EnglishGrade
                  #+ EnglishLevel
                  #+ DanishGrade
                  #+ yearsFromEnrolment
                  #+ADGGRU
                  #+campus 
                  #+ gender 
                  # isInternationalStudent
                  #,dfM[dfM$yearOfEnrolment== & dfM$campus=="Aalborg" ,])
                  ,dfM[dfM$startaar %in% c(2011,2012,2013) ,])
summary(dropOutModel)



dropOutModel<- lm(isDropOut ~ #
                    Mat7 
                  #+ 
                  #+ EnglishGrade
                  #+ EnglishLevel
                  #+ DanishGrade
                  #+ yearsFromEnrolment
                  #+ADGGRU
                  + campus 
                  #+ gender 
                  # isInternationalStudent
                  ,dfM[dfM$startaar<2015 && dfM$mathLevel=="B",])
summary(dropOutModel)

GPROMathMod <- lm(FirstExamGradeNum~ mathGrade
                  +mathLevel,
                  dfGPRO[dfGPRO$startaar==2014,])
summary(GPROMathMod)

#MMAMathMod 



#P4IMathMod 

GPROMathModb <- lm(FirstExamGradeNum~ mathGrade,
                   dfGPROmB)
summary(GPROMathModb)

#create cdf by factor mathlevel 


write.csv(dflastExamAttempts,"ForKasper.csv")

library(reshape)

castData<-cast(dflastExamAttempts, cprnr+examAttempt~activityName, value = "examGradeNumeric", sum)


#MedDataBSc <- read.csv("Y:/analysis_data/dropOut/data/MedDataBSc.csv", stringsAsFactors=FALSE, encoding = 'ANSI')

MedDataBSc <- read.csv("Y:/analysis_data/dropOut/data/MedDataBSc2012-2016.csv", stringsAsFactors=FALSE, encoding = 'ANSI')

MedDataBSc2014 <- MedDataBSc[MedDataBSc$startaar==2014,]
table(MedDataBSc2014$statussn,MedDataBSc2014$udmeldsn)
MedDataBSc2014 <- MedDataBSc2014[MedDataBSc2014$statussn!='orlov',]
MedDataBSc2014$dropout <- ifelse(MedDataBSc2014$statussn=='afbrudt','dropout','active')
MedDataBSc2014 <- MedDataBSc2014[,-c(1:24,32,36:39,41,44,45,47)]
MedDataBSc2014$EndSemester[is.na(MedDataBSc2014$EndSemester)] <- 6

#Looking into danish and english
fit.null <- glm(factor(dropout)~1,family = binomial, data=MedDataBSc2014)

add1(fit.null,~DANGrade, test='LRT')
fit.DAN <- glm(factor(dropout)~DANGrade+kvotient, family=binomial, data = MedDataBSc2014)
summary(fit.DAN)
drop1(fit.DAN,test = 'LRT')

add1(fit.null,~ENGGrade+ENG_Niveau, test='LRT')

#Can disregard them when including kvotient
MedDataBSc2014 <- MedDataBSc2014[,-c(4:7)]

which(is.na(MedDataBSc2014[-(15:158)]))
#Imputing 41 missing values, numeric by mean, categorical by most common
for (i in 1:14) {
  if(any(is.na(MedDataBSc2014[,i]))){
    if(class(MedDataBSc2014[,i])=='numeric'){
      MedDataBSc2014[is.na(MedDataBSc2014[,i]),i] <- mean(MedDataBSc2014[,i],na.rm = T)
    }
    else{
      tab <- table(MedDataBSc2014[,i])
      MedDataBSc2014[is.na(MedDataBSc2014[,i]),i] <- names(tab[which.max(tab)])
    }
  }
}
table(MedDataBSc2014$ADGGRU)
#Treating Matematisk student and Sproglig student as STX
MedDataBSc2014$ADGGRU[MedDataBSc2014$ADGGRU%in%c('Matematisk student','Sproglig student')] <- 'STX'
#One student with math C is excluded
MedDataBSc2014 <- MedDataBSc2014[-152,]

MedDataBSc2014.0 <- MedDataBSc2014[,-(11:154)]


#Fitting models
fit.null <- glm(factor(dropout)~1,family = binomial, data=MedDataBSc2014.0)

add1(fit.null,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment, test='LRT')

fitBIC <- step(fit.null,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment,k=log(nrow(MedDataBSc2014.0)))
summary(fitBIC)

normfun <- function(x){(x-min(x))/(max(x)-min(x))}
MATGradenom <- normfun(MedDataBSc2014.0$MATGrade)
fitBICnom <- glm(factor(dropout)~MATGradenom,family=binomial, data=MedDataBSc2014.0)
summary(fitBICnom)  

fitAIC <- step(fit.null,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment)
summary(fitAIC)

fitInt <- glm(factor(dropout)~MATGrade*MAT_Niveau+kvotient,family = binomial,data = MedDataBSc2014.0)
summary(fitInt)
drop1(fitInt,test = 'LRT')


#Doing cross-validation
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
    CV[i] <- mean(pred==test$dropout)
    tab <- table(factor(pred, levels = c('active','dropout')),test$dropout)
    FP[i] <- tab[2,1]/sum(tab[,1])
    if(sum(tab[,2])==0){FN[i] <- 0}
    else{FN[i] <- tab[1,2]/sum(tab[,2])}
  }
  accuracy <- CV
  FP <- FP
  FN <- FN
  return(list('accuracy'=accuracy,'FP'=FP,'FN'=FN))
}

CVBIC <- logCV(fitBIC,data=MedDataBSc2014.0)
mean(CVBIC$accuracy)
mean(CVBIC$FN)
mean(CVBIC$FP)

CVAIC <- logCV(fitAIC,data=MedDataBSc2014.0)
mean(CVAIC$accuracy)
mean(CVAIC$FN)
mean(CVAIC$FP)

#Predicting for 2017
MedDataBSc2017 <- read.csv("Y:/analysis_data/dropOut/data_2017cohortCPHAAL/MedDataBSc2017_1107.csv", encoding="ANSI", stringsAsFactors=FALSE)
MedDataBSc2017$dropout <- ifelse(MedDataBSc2017$udmeld_aarsag!=''|MedDataBSc2017$Q999==1,'dropout','active')
MedDataBSc2017[MedDataBSc2017==''] <- NA
MedDataBSc2017[MedDataBSc2017=='.'] <- NA

MedDataBSc2017$predBIC <- predict(fitBIC, newdata = MedDataBSc2017,type = 'response')
MedDataBSc2017$predAIC <- predict(fitAIC, newdata = MedDataBSc2017,type = 'response')

classBIC <- ifelse(MedDataBSc2017$predBIC>0.5,'dropout','active')
classAIC <- ifelse(MedDataBSc2017$predAIC>0.5,'dropout','active')

table(classBIC,MedDataBSc2017$dropout)
table(classAIC,MedDataBSc2017$dropout)

# including information from first semester
MedDataBSc2014.1 <- MedDataBSc2014[MedDataBSc2014$EndSemester>1,]
table(MedDataBSc2014.1$dropout)

MedDataBSc2014.1 <- MedDataBSc2014.1[,-c(19:114,119:154)]
MedDataBSc2014.1 <- MedDataBSc2014.1[,-c(15,16,19)]

#Setting GPA to NA if they did not attempt any ECTS
MedDataBSc2014.1$X1_T_mGPA[MedDataBSc2014.1$X1_T_atpIn==0] <- NA
MedDataBSc2014.1$X1_NT_mGPA[MedDataBSc2014.1$X1_NT_atpIn==0] <- NA
MedDataBSc2014.1$X1_Pr_mGPA[MedDataBSc2014.1$X1_Pr_atpIn==0] <- NA
which(is.na(MedDataBSc2014.1))
which(!complete.cases(MedDataBSc2014.1))
#Excluding students who did not attempt ECTS in one of the relevant categories (Merit)
MedDataBSc2014.1 <- MedDataBSc2014.1[complete.cases(MedDataBSc2014.1),]
table(MedDataBSc2014.1$dropout)

fit.null.1 <- glm(factor(dropout)~1,family=binomial, data=MedDataBSc2014.1)

add1(fit.null.1,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X1_T_atnBy+X1_NT_atnBy+X1_Pr_atnBy+X1_T_mGPA+X1_NT_mGPA+X1_Pr_mGPA, test='LRT')

fitBIC.1 <- step(fit.null.1,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X1_T_atnBy+X1_NT_atnBy+X1_Pr_atnBy+X1_T_mGPA+X1_NT_mGPA+X1_Pr_mGPA,k=log(nrow(MedDataBSc2014.1)))
summary(fitBIC.1)

X1_T_atnBynom <- normfun(MedDataBSc2014.1$X1_T_atnBy)
X1_Pr_mGPAnom <- normfun(MedDataBSc2014.1$X1_Pr_mGPA)
X1_NT_mGPAnom <- normfun(MedDataBSc2014.1$X1_NT_mGPA)

fitBIC.1nom <- glm(factor(dropout)~X1_T_atnBynom+X1_Pr_mGPAnom+X1_NT_mGPAnom,family=binomial,data=MedDataBSc2014.1)
summary(fitBIC.1nom)

fitAIC.1 <- step(fit.null.1,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X1_T_atnBy+X1_NT_atnBy+X1_Pr_atnBy+X1_T_mGPA+X1_NT_mGPA+X1_Pr_mGPA)
summary(fitAIC.1)

CV.1 <- logCV(fitBIC.1,data=MedDataBSc2014.1)
mean(CV.1$accuracy)
mean(CV.1$FN)
mean(CV.1$FP)

#predicting for 2017
MedDataBSc2017$X1_T_mGPA[MedDataBSc2017$X1_T_atpIn==0] <- NA
MedDataBSc2017$X1_NT_mGPA[MedDataBSc2017$X1_NT_atpIn==0] <- NA
MedDataBSc2017$X1_Pr_mGPA[MedDataBSc2017$X1_Pr_atpIn==0] <- NA

idx <- which(as.Date(MedDataBSc2017$udmeld_dato)<'2018-02-01')

MedDataBSc2017$predBIC.1 <- predict(fitBIC.1, newdata = MedDataBSc2017,type = 'response')
pred <- predict(fitBIC.1,MedDataBSc2017[-idx,],type='response')
classBIC.1 <- ifelse(pred>0.5,'dropout','active')
table(classBIC.1,MedDataBSc2017$dropout[-idx])

#Including information from 2. semester
MedDataBSc2014.2 <- MedDataBSc2014[MedDataBSc2014$EndSemester>2,]
table(MedDataBSc2014.2$dropout)
MedDataBSc2014.2 <- MedDataBSc2014.2[,-c(11:18,27:118,123:154)]
MedDataBSc2014.2 <- MedDataBSc2014.2[,-c(15,16,19)]

which(is.na(MedDataBSc2014.2))
which(!complete.cases(MedDataBSc2014.2))
#Excluding students with NA in some GPA
MedDataBSc2014.2 <- MedDataBSc2014.2[complete.cases(MedDataBSc2014.2),]
table(MedDataBSc2014.2$dropout)

fit.null.2 <- glm(factor(dropout)~1,family=binomial, data=MedDataBSc2014.2)

add1(fit.null.2,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X2_T_atnBy+X2_NT_atnBy+X2_Pr_atnBy+X2_T_mGPA+X2_NT_mGPA+X2_Pr_mGPA, test='LRT')

fitBIC.2 <- step(fit.null.2,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X2_T_atnBy+X2_NT_atnBy+X2_Pr_atnBy+X2_T_mGPA+X2_NT_mGPA+X2_Pr_mGPA,k=log(nrow(MedDataBSc2014.2)))
summary(fitBIC.2)

X2_T_atnBynom <- normfun(MedDataBSc2014.2$X2_T_atnBy)
X2_Pr_mGPA <- normfun(MedDataBSc2014.2$X2_Pr_mGPA)

fitBIC.2nom <- glm(factor(dropout)~X2_T_atnBynom+X2_Pr_mGPA,family = binomial, data = MedDataBSc2014.2)
summary(fitBIC.2nom)

fitAIC.2 <- step(fit.null.2,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X2_T_atnBy+X2_NT_atnBy+X2_Pr_atnBy+X2_T_mGPA+X2_NT_mGPA+X2_Pr_mGPA)
summary(fitAIC.2)

CV.2 <- logCV(fitBIC.2,data=MedDataBSc2014.2)
mean(CV.2$accuracy)
mean(CV.2$FN)
mean(CV.2$FP)

#including information from 3. semester

MedDataBSc2014.3 <- MedDataBSc2014[MedDataBSc2014$EndSemester>3,]
table(MedDataBSc2014.3$dropout)
MedDataBSc2014.3 <- MedDataBSc2014.3[,-c(11:26,35:122,127:154)]
MedDataBSc2014.3 <- MedDataBSc2014.3[,-c(15,16,19)]

fit.null.3 <- glm(factor(dropout)~1,family=binomial, data=MedDataBSc2014.3)

add1(fit.null.3,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X3_T_atnBy+X3_NT_atnBy+X3_Pr_atnBy+X3_T_mGPA+X3_NT_mGPA+X3_Pr_mGPA, test='LRT')

fitBIC.3 <- step(fit.null.3,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment+X3_T_atnBy+X3_NT_atnBy+X3_Pr_atnBy+X3_T_mGPA+X3_NT_mGPA+X3_Pr_mGPA,k=log(nrow(MedDataBSc2014.3)))
summary(fitBIC.3)

X3_T_atnBynom <- normfun(MedDataBSc2014.3$X3_T_atnBy)
X3_NT_mGPAnom <- normfun(MedDataBSc2014.3$X3_NT_mGPA)

fitBIC.3nom <- glm(factor(dropout)~X3_T_atnBynom+X3_NT_mGPAnom,family = binomial, data = MedDataBSc2014.3)
summary(fitBIC.3nom)

CV.3 <- logCV(fitBIC.3,data=MedDataBSc2014.3)
mean(CV.3$accuracy)
mean(CV.3$FN)
mean(CV.3$FP)

#After 4. semester there are too few dropouts.
MedDataBSc2014.4 <- MedDataBSc2014[MedDataBSc2014$EndSemester>4,]
table(MedDataBSc2014.4$dropout)

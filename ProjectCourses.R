library(ggplot2)

Grades <- read.csv("Y:/analysis_data/dropOut/data/dfAAUMarriedGrades.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Grades <- Grades[,c(2,4,9,12,18,24,27,32,38,39,41,42,43,44,45)]
#Only interested in first try
Grades <- Grades[Grades$Forsoeg.nr.==1,-5]
table(Grades$CourseLocation)

GradesBsc <- Grades[Grades$type=='bachelor',-4]
GradesMsc <- Grades[Grades$type=='kandidat',-4]
table(GradesBsc$CourseLocation)
table(GradesMsc$CourseLocation)

table(GradesBsc$takenInSem,GradesBsc$startaar)
#Only data for first 6 semesters for 2012-2014 so to compare across semester use only these
GradesBsc <- GradesBsc[GradesBsc$startaar>=2012 & GradesBsc$startaar<=2014,]
#Only looking at the first 6 semesters
GradesBsc <- GradesBsc[GradesBsc$takenInSem<=6,]
GradesBsc$isProj <- as.numeric(GradesBsc$bctdf=='project')

Fixed <- GradesBsc[,c(3,5,6,12)]
Fixed <- Fixed[!duplicated(Fixed),]
switch <- Fixed$enrolID[duplicated(Fixed$enrolID)]
#seven students seems to have svitched campus. They are removed because otherwise campus can not be compared
GradesBsc <- GradesBsc[!GradesBsc$enrolID%in%switch,]

ID <- levels(factor(GradesBsc$enrolID))
Data <- data.frame(ID)     
Data <- merge(Data,Fixed, by.y='enrolID', by.x='ID')
Data$courseAVG1sem <- rep(NA, nrow(Data))
Data$courseAVG2sem <- rep(NA, nrow(Data))
Data$courseAVG3sem <- rep(NA, nrow(Data))
Data$courseAVG4sem <- rep(NA, nrow(Data))
Data$courseAVG5sem <- rep(NA, nrow(Data))
Data$courseAVG6sem <- rep(NA, nrow(Data))
Data$courseAVGallsem <- rep(NA, nrow(Data))
Data$projectAVG1sem <- rep(NA, nrow(Data))
Data$projectAVG2sem <- rep(NA, nrow(Data))
Data$projectAVG3sem <- rep(NA, nrow(Data))
Data$projectAVG4sem <- rep(NA, nrow(Data))
Data$projectAVG5sem <- rep(NA, nrow(Data))
Data$projectAVG6sem <- rep(NA, nrow(Data))
Data$projectAVGallsem <- rep(NA, nrow(Data))
i <- 1
for(I in ID){
  Data$courseAVGallsem[i] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I&GradesBsc$isProj!=1])
  Data$projectAVGallsem[i] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I&GradesBsc$isProj==1])
  for(sem in 1:6){
    Data[i,sem+4] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I & GradesBsc$isProj!=1 & GradesBsc$takenInSem==sem])
    Data[i,sem+11] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I & GradesBsc$isProj==1 & GradesBsc$takenInSem==sem])
  }
  i <- i+1
}


fit <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation), data = Data)
summary(fit)
drop1(fit,test = 'F')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+geom_point(aes(col=factor(CourseLocation)))+geom_abline(intercept = (3.6),slope = 0.77,col='red')+geom_abline(intercept = (3.6+0.12),slope = (0.77),col='blue')+geom_abline(intercept = (3.6-0.19),slope = (0.77+0.22),col='green')

fit <- lm(projectAVGallsem~courseAVGallsem*factor(startaar), data = Data)
summary(fit)
drop1(fit,test = 'F')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+geom_point(aes(col=factor(startaar)))+geom_abline(intercept = (3.5),slope = 0.93,col='red')+geom_abline(intercept = (3.5+0.37),slope = (0.93-0.29),col='blue')+geom_abline(intercept = (3.5+0.33),slope = (0.93-0.12),col='green')
#different slopes

ggplot(aes(courseAVG1sem,projectAVG1sem), data = Data)+geom_point(aes(col=factor(CourseLocation)))
ggplot(aes(courseAVG2sem,projectAVG2sem), data = Data)+geom_point(aes(col=factor(CourseLocation)))
ggplot(aes(courseAVG3sem,projectAVG3sem), data = Data)+geom_point(aes(col=factor(CourseLocation)))
ggplot(aes(courseAVG4sem,projectAVG4sem), data = Data)+geom_point(aes(col=factor(CourseLocation)))
ggplot(aes(courseAVG5sem,projectAVG5sem), data = Data)+geom_point(aes(col=factor(CourseLocation)))
ggplot(aes(courseAVG6sem,projectAVG6sem), data = Data)+geom_point(aes(col=factor(CourseLocation)))



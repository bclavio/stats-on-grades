library(ggplot2)
library(reshape2)
Grades <- read.csv("Y:/analysis_data/dropOut/data/dfAAUMarriedGrades.csv", encoding="UTF-8", stringsAsFactors=FALSE)
Grades <- Grades[,c(2,4,9,12,18,24,27,32,38,39,41,42,43,44,45)]
#Only interested in first try
Grades <- Grades[Grades$Forsoeg.nr.==1,-5]

GradesBsc <- Grades[Grades$type=='bachelor',-4]
GradesMsc <- Grades[Grades$type=='kandidat',-4]

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
  Data$courseAVGallsem[i] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I&GradesBsc$isProj!=1&GradesBsc$gradeType=='scale'])
  Data$projectAVGallsem[i] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I&GradesBsc$isProj==1&GradesBsc$gradeType=='scale'])
  for(sem in 1:6){
    Data[i,sem+4] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I & GradesBsc$isProj!=1 & GradesBsc$takenInSem==sem&GradesBsc$gradeType=='scale'])
    Data[i,sem+11] <- mean(GradesBsc$gradeNum[GradesBsc$enrolID==I & GradesBsc$isProj==1 & GradesBsc$takenInSem==sem&GradesBsc$gradeType=='scale'])
  }
  i <- i+1
}
Data[is.na(Data)] <- NA

Data2012 <- Data[Data$startaar==2012,]
Data2013 <- Data[Data$startaar==2013,]
Data2014 <- Data[Data$startaar==2014,]

##############Plots################
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVGallsem))+
  geom_boxplot(aes(x='Project',y=projectAVGallsem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVGallsem))+
  geom_boxplot(aes(x='Project',y=projectAVGallsem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVGallsem))+
  geom_boxplot(aes(x='Project',y=projectAVGallsem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVGallsem))+
  geom_boxplot(aes(x='Project',y=projectAVGallsem))+
  facet_wrap(~factor(CourseLocation))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

#International

ggplot(aes(col=factor(Data$isIntl)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVGallsem))+
  geom_boxplot(aes(x='Project',y=projectAVGallsem))+
  scale_color_discrete(name=' ',labels=c('Not international','International'))+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVGallsem))+
  geom_boxplot(aes(x='Project',y=projectAVGallsem))+
  facet_wrap(~factor(isIntl),labeller = as_labeller(c('0'='Not internatinal','1'='International')))+
  xlab('Activity')+
  ylab('Average grade')

##By semester
# 1 semester
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG1sem))+
  geom_boxplot(aes(x='Project',y=projectAVG1sem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG1sem))+
  geom_boxplot(aes(x='Project',y=projectAVG1sem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG1sem))+
  geom_boxplot(aes(x='Project',y=projectAVG1sem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG1sem))+
  geom_boxplot(aes(x='Project',y=projectAVG1sem))+
  facet_wrap(~factor(CourseLocation))+
  stat_summary(aes(x='Courses',y=courseAVG1sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  stat_summary(aes(x='Project',y=projectAVG1sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

#2 semester
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG2sem))+
  geom_boxplot(aes(x='Project',y=projectAVG2sem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG2sem))+
  geom_boxplot(aes(x='Project',y=projectAVG2sem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG2sem))+
  geom_boxplot(aes(x='Project',y=projectAVG2sem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG2sem))+
  geom_boxplot(aes(x='Project',y=projectAVG2sem))+
  facet_wrap(~factor(CourseLocation))+
  stat_summary(aes(x='Courses',y=courseAVG2sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  stat_summary(aes(x='Project',y=projectAVG2sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')
#3 semester
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG3sem))+
  geom_boxplot(aes(x='Project',y=projectAVG3sem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG3sem))+
  geom_boxplot(aes(x='Project',y=projectAVG3sem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG3sem))+
  geom_boxplot(aes(x='Project',y=projectAVG3sem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG3sem))+
  geom_boxplot(aes(x='Project',y=projectAVG3sem))+
  facet_wrap(~factor(CourseLocation))+
  stat_summary(aes(x='Courses',y=courseAVG3sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  stat_summary(aes(x='Project',y=projectAVG3sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')
#4 semester
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG4sem))+
  geom_boxplot(aes(x='Project',y=projectAVG4sem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG4sem))+
  geom_boxplot(aes(x='Project',y=projectAVG4sem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG4sem))+
  geom_boxplot(aes(x='Project',y=projectAVG4sem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG4sem))+
  geom_boxplot(aes(x='Project',y=projectAVG4sem))+
  facet_wrap(~factor(CourseLocation))+
  stat_summary(aes(x='Courses',y=courseAVG4sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  stat_summary(aes(x='Project',y=projectAVG4sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')
#5 semester
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG5sem))+
  geom_boxplot(aes(x='Project',y=projectAVG5sem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG5sem))+
  geom_boxplot(aes(x='Project',y=projectAVG5sem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG5sem))+
  geom_boxplot(aes(x='Project',y=projectAVG5sem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG5sem))+
  geom_boxplot(aes(x='Project',y=projectAVG5sem))+
  facet_wrap(~factor(CourseLocation))+
  stat_summary(aes(x='Courses',y=courseAVG5sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  stat_summary(aes(x='Project',y=projectAVG5sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')
#6 semester
#year
ggplot(aes(col=factor(Data$startaar)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG6sem))+
  geom_boxplot(aes(x='Project',y=projectAVG6sem))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG6sem))+
  geom_boxplot(aes(x='Project',y=projectAVG6sem))+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='Year')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(aes(col=factor(Data$CourseLocation)),data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG6sem))+
  geom_boxplot(aes(x='Project',y=projectAVG6sem))+
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data)+
  geom_boxplot(aes(x='Courses',y=courseAVG6sem))+
  geom_boxplot(aes(x='Project',y=projectAVG6sem))+
  facet_wrap(~factor(CourseLocation))+
  stat_summary(aes(x='Courses',y=courseAVG6sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  stat_summary(aes(x='Project',y=projectAVG6sem),fun.y = mean, geom="point",colour="blue", size=2,pch='X') +
  scale_color_discrete(name='Campus')+
  xlab('Activity')+
  ylab('Average grade')

#Comparing semester wise
sub <- Data[,c(2:10,12:17)]
Data.m <- melt(sub, id.vars = c('startaar','CourseLocation','isIntl'))
Data.m$type <- rep(NA,nrow(Data.m))
Data.m$type <- c(rep('Course',6024),rep('Project',6024))
Data.m$sem <- rep(NA,nrow(Data.m))
Data.m$sem[Data.m$variable=='courseAVG1sem'| Data.m$variable=='projectAVG1sem'] <- 1
Data.m$sem[Data.m$variable=='courseAVG2sem'| Data.m$variable=='projectAVG2sem'] <- 2
Data.m$sem[Data.m$variable=='courseAVG3sem'| Data.m$variable=='projectAVG3sem'] <- 3
Data.m$sem[Data.m$variable=='courseAVG4sem'| Data.m$variable=='projectAVG4sem'] <- 4
Data.m$sem[Data.m$variable=='courseAVG5sem'| Data.m$variable=='projectAVG5sem'] <- 5
Data.m$sem[Data.m$variable=='courseAVG6sem'| Data.m$variable=='projectAVG6sem'] <- 6

ggplot(data=Data.m,aes(x=factor(sem),y=value,col=type))+
  geom_boxplot()+
  scale_color_discrete(name='')+
  xlab('Semester')+
  ylab('Average grade')

ggplot(data=Data.m,aes(x=factor(sem),y=value,col=type))+
  geom_boxplot()+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='')+
  xlab('Semester')+
  ylab('Average grade')

ggplot(data=Data.m,aes(x=factor(sem),y=value,col=type))+
  geom_boxplot()+
  facet_wrap(~CourseLocation,labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  scale_color_discrete(name='')+
  xlab('Semester')+
  ylab('Average grade')



##Looking for differences between cohorts
fit <- lm(projectAVGallsem~courseAVGallsem*factor(startaar), data = Data)
summary(fit)
drop1(fit,test = 'F')

ggplot(aes(courseAVGallsem,projectAVGallsem,col=factor(startaar)), data = Data)+
  geom_point()+geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Year'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')
#different slope for 2014

AVGCAll <- Data$courseAVGallsem[!is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AVGPAll <- Data$projectAVGallsem[!is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgCAll2012 <- Data$courseAVGallsem[Data$startaar==2012 & !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgCAll2013 <- Data$courseAVGallsem[Data$startaar==2013& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgCAll2014 <- Data$courseAVGallsem[Data$startaar==2014& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgPAll2012 <- Data$projectAVGallsem[Data$startaar==2012& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgPAll2013 <- Data$projectAVGallsem[Data$startaar==2013& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgPAll2014 <- Data$projectAVGallsem[Data$startaar==2014& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
cor(AVGCAll,AVGPAll)
cor(AvgCAll2012,AvgPAll2012)
cor(AvgCAll2013,AvgPAll2013)
cor(AvgCAll2014,AvgPAll2014)

##Looking at differences between campus
fit <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation), data = Data)
summary(fit)
drop1(fit,test = 'F')

ggplot(aes(courseAVGallsem,projectAVGallsem,col=CourseLocation), data = Data)+
  geom_point()+geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'))+
  scale_color_discrete(labels=c('Aalborg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~CourseLocation,labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')
#Seems to be different slope for Esbjerg and Copenhagen

fit <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation)*factor(startaar), data = Data)
summary(fit)
drop1(fit,test = 'F')

fit2012 <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation),data = Data2012)
summary(fit2012)
drop1(fit2012,test='F')
fit2012 <- lm(projectAVGallsem~courseAVGallsem+factor(CourseLocation),data = Data2012)
summary(fit2012)
drop1(fit2012,test='F')
#No difference on campus

fit2013 <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation),data = Data2013)
summary(fit2013)
drop1(fit2013,test='F')

fit2014 <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation),data = Data2014)
summary(fit2014)
drop1(fit2014,test='F')

ggplot(aes(courseAVGallsem,projectAVGallsem, col=CourseLocation), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T, fullrange=T)+
  guides(col=guide_legend(title='Campus'))+
  scale_color_discrete(labels=c('Aalborg','Esbjerg','Copenhagen'))+
  facet_wrap(~startaar)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~CourseLocation+startaar,labeller = labeller(CourseLocation=as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen'))))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

AvgCAllA <- Data$courseAVGallsem[Data$CourseLocation=='A' & !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgCAllE <- Data$courseAVGallsem[Data$CourseLocation=='E'& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgCAllK <- Data$courseAVGallsem[Data$CourseLocation=='K'& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgPAllA <- Data$projectAVGallsem[Data$CourseLocation=='A'& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgPAllE <- Data$projectAVGallsem[Data$CourseLocation=='E'& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
AvgPAllK <- Data$projectAVGallsem[Data$CourseLocation=='K'& !is.na(Data$courseAVGallsem) & !is.na(Data$projectAVGallsem)]
cor(AvgCAllA,AvgPAllA)
cor(AvgCAllE,AvgPAllE)
cor(AvgCAllK,AvgPAllK)

##Looking at difference by international students
fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl), data = Data)
summary(fit)
drop1(fit,test = 'F')

ggplot(aes(courseAVGallsem,projectAVGallsem,col=factor(isIntl)), data = Data)+
  geom_point()+geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title=''))+
  scale_color_discrete(labels=c('Not International','International'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(isIntl),labeller = as_labeller(c('0'='Not international','1'='International')))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl)*factor(startaar), data = Data)
summary(fit)
drop1(fit,test = 'F')


ggplot(aes(courseAVGallsem,projectAVGallsem, col=factor(isIntl)), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=T,na.rm = T)+
  guides(col=guide_legend(title=''))+
  scale_color_discrete(labels=c('Not international','international'))+
  facet_wrap(~startaar)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(isIntl)+startaar)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl)*CourseLocation, data = Data)
summary(fit)
drop1(fit,test = 'F')

##Looking at data by semester
cor(Data$courseAVG1sem[!is.na(Data$courseAVG1sem) & !is.na(Data$projectAVG1sem)],Data$projectAVG1sem[!is.na(Data$courseAVG1sem) & !is.na(Data$projectAVG1sem)])
cor(Data$courseAVG2sem[!is.na(Data$courseAVG2sem) & !is.na(Data$projectAVG2sem)],Data$projectAVG2sem[!is.na(Data$courseAVG2sem) & !is.na(Data$projectAVG2sem)])
cor(Data$courseAVG3sem[!is.na(Data$courseAVG3sem) & !is.na(Data$projectAVG3sem)],Data$projectAVG3sem[!is.na(Data$courseAVG3sem) & !is.na(Data$projectAVG3sem)])
cor(Data$courseAVG4sem[!is.na(Data$courseAVG4sem) & !is.na(Data$projectAVG4sem)],Data$projectAVG4sem[!is.na(Data$courseAVG4sem) & !is.na(Data$projectAVG4sem)])
cor(Data$courseAVG5sem[!is.na(Data$courseAVG5sem) & !is.na(Data$projectAVG5sem)],Data$projectAVG5sem[!is.na(Data$courseAVG5sem) & !is.na(Data$projectAVG5sem)])
cor(Data$courseAVG6sem[!is.na(Data$courseAVG6sem) & !is.na(Data$projectAVG6sem)],Data$projectAVG6sem[!is.na(Data$courseAVG6sem) & !is.na(Data$projectAVG6sem)])

#Year
fit <- lm(projectAVG1sem~courseAVG1sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG1sem~courseAVG1sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
#No difference between years

fit2012 <- lm(projectAVG1sem~courseAVG1sem,data = Data2012)
summary(fit2012)
fit2013 <- lm(projectAVG1sem~courseAVG1sem,data = Data2013)
summary(fit2013)
fit2014 <- lm(projectAVG1sem~courseAVG1sem,data = Data2014)
summary(fit2014)

ggplot(aes(courseAVG1sem,projectAVG1sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVG1sem,projectAVG1sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem)& !projectAVG1sem%in%c(-0.5,0)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem) & !projectAVG1sem%in%c(-0.5,0)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')


fit <- lm(projectAVG2sem~courseAVG2sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG2sem~courseAVG2sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit2012 <- lm(projectAVG2sem~courseAVG2sem,data = Data2012)
summary(fit2012)
fit2013 <- lm(projectAVG2sem~courseAVG2sem,data = Data2013)
summary(fit2013)
fit2014 <- lm(projectAVG2sem~courseAVG2sem,data = Data2014)
summary(fit2014)

ggplot(aes(courseAVG2sem,projectAVG2sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=F,na.rm = T)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVG2sem,projectAVG2sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  scale_size_continuous(name='Number of points')+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG2sem),courseAVG2sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG2sem) & !is.na(courseAVG2sem)& !projectAVG2sem%in%c(4.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

ggplot(aes(factor(projectAVG2sem),courseAVG2sem), data =subset(Data, !is.na(projectAVG2sem) & !is.na(courseAVG2sem) & !projectAVG2sem%in%c(4.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

fit <- lm(projectAVG3sem~courseAVG3sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')


fit2012 <- lm(projectAVG3sem~courseAVG3sem,data = Data2012)
summary(fit2012)
fit2013 <- lm(projectAVG3sem~courseAVG3sem,data = Data2013)
summary(fit2013)
fit2014 <- lm(projectAVG3sem~courseAVG3sem,data = Data2014)
summary(fit2014)

ggplot(aes(courseAVG3sem,projectAVG3sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVG3sem,projectAVG3sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects') 

ggplot(aes(factor(projectAVG3sem),courseAVG3sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & projectAVG3sem!=5))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG3sem),courseAVG3sem), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & projectAVG3sem!=5))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

fit <- lm(projectAVG4sem~courseAVG4sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG4sem~courseAVG4sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

fit2012 <- lm(projectAVG4sem~courseAVG4sem,data = Data2012)
summary(fit2012)
fit2013 <- lm(projectAVG4sem~courseAVG4sem,data = Data2013)
summary(fit2013)
fit2014 <- lm(projectAVG4sem~courseAVG4sem,data = Data2014)
summary(fit2014)

ggplot(aes(courseAVG4sem,projectAVG4sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVG4sem,projectAVG4sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG4sem),courseAVG4sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG4sem) & !is.na(courseAVG4sem) & !projectAVG4sem%in%c(5.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG4sem),courseAVG4sem), data =subset(Data, !is.na(projectAVG4sem) & !is.na(courseAVG4sem) & !projectAVG4sem%in%c(5.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')
#5 semester
fit <- lm(projectAVG5sem~courseAVG5sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG5sem~courseAVG5sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

fit2012 <- lm(projectAVG5sem~courseAVG5sem,data = Data2012)
summary(fit2012)
fit2013 <- lm(projectAVG5sem~courseAVG5sem,data = Data2013)
summary(fit2013)
fit2014 <- lm(projectAVG5sem~courseAVG5sem,data = Data2014)
summary(fit2014)

ggplot(aes(courseAVG5sem,projectAVG5sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVG5sem,projectAVG5sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG5sem),courseAVG5sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG5sem) & !is.na(courseAVG5sem) & !projectAVG5sem%in%c(3)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG5sem),courseAVG5sem), data =subset(Data, !is.na(projectAVG5sem) & !is.na(courseAVG5sem) & !projectAVG5sem%in%c(3)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

#6 semester
fit <- lm(projectAVG6sem~courseAVG6sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG6sem~courseAVG6sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

fit2012 <- lm(projectAVG6sem~courseAVG6sem,data = Data2012)
summary(fit2012)
fit2013 <- lm(projectAVG6sem~courseAVG6sem,data = Data2013)
summary(fit2013)
fit2014 <- lm(projectAVG6sem~courseAVG6sem,data = Data2014)
summary(fit2014)

ggplot(aes(courseAVG6sem,projectAVG6sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVG6sem,projectAVG6sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG6sem),courseAVG6sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG6sem) & !is.na(courseAVG6sem) & !projectAVG6sem%in%c(8.5,9.5,11)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG6sem),courseAVG6sem), data =subset(Data, !is.na(projectAVG6sem) & !is.na(courseAVG6sem) & !projectAVG6sem%in%c(8.5,9.5,11)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

#Campus
#1 semester
ggplot(aes(courseAVG1sem,projectAVG1sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem) & !projectAVG1sem%in%c()))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title = 'Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

#2 semester
ggplot(aes(courseAVG2sem,projectAVG2sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG2sem),courseAVG2sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG2sem) & !is.na(courseAVG2sem) & !projectAVG2sem%in%c(4.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title = 'Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

#3 semester
ggplot(aes(courseAVG3sem,projectAVG3sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG3sem),courseAVG3sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & !projectAVG3sem%in%c(5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title = 'Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

#4 semester
ggplot(aes(courseAVG4sem,projectAVG4sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG4sem),courseAVG4sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG4sem) & !is.na(courseAVG4sem) & !projectAVG4sem%in%c(5.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title = 'Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')
#5 semester
ggplot(aes(courseAVG5sem,projectAVG5sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG5sem),courseAVG5sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG5sem) & !is.na(courseAVG5sem) & !projectAVG5sem%in%c(3)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title = 'Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

#6 semester
ggplot(aes(courseAVG6sem,projectAVG6sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(factor(projectAVG6sem),courseAVG6sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG6sem) & !is.na(courseAVG6sem) & !projectAVG6sem%in%c(8.5,9.5,11)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title = 'Campus'),size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

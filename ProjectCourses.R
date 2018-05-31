library(ggplot2)
library(reshape2)
library(ggpmisc)
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

subC <- Data[,c(2:4,11)]
subC$type <- 'Courses'
names(subC)[4] <- 'Grade'
subP <- Data[,c(2:4,18)]
subP$type <- 'Projects'
names(subP)[4] <- 'Grade'
Data.m.all <- rbind(subC,subP)

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


##############Plots################
#Year
ggplot(data=Data.m.all, aes(x=type,y=Grade,col=factor(startaar)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  scale_color_discrete(name='Year')+
  scale_size_continuous(name='Nuber of points')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=subset(Data.m.all,Grade!=-8), aes(x=type,y=Grade,col=factor(startaar)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  scale_color_discrete(name='Year')+
  scale_size_continuous(name='Nuber of points')+
  xlab('Activity')+
  ylab('Average grade')



fit <- lm(courseAVGallsem~factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

fit <- lm(projectAVGallsem~factor(startaar),data =Data)
summary(fit)
drop1(fit,test = 'F')

fit <- lm(courseAVGallsem~factor(startaar),data = subset(Data,courseAVGallsem!=-8))
summary(fit)
drop1(fit,test = 'F')

fit <- lm(projectAVGallsem~factor(startaar),data = subset(Data,projectAVGallsem!=-8))
summary(fit)
drop1(fit,test = 'F')

t.test(Data$courseAVGallsem,Data$projectAVGallsem,paired = T)



ggplot(data=Data.m.all, aes(x=type,y=Grade))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position=position_dodge(1),alpha=0.5)+
  facet_wrap(~factor(startaar))+
  scale_size_continuous(name='Nuber of points')+
  xlab('Activity')+
  ylab('Average grade')

#campus
ggplot(data=Data.m.all, aes(x=type,y=Grade,col=CourseLocation))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  scale_color_discrete(name='Campus',labels=c('Aalborg','Esbjerg','Copenhagen'))+
  scale_size_continuous(name='Nuber of points')+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data.m.all, aes(x=type,y=Grade))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  facet_wrap(~CourseLocation,labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  scale_color_discrete(name='Campus')+
  scale_size_continuous(name='Nuber of points')+
  xlab('Activity')+
  ylab('Average grade')

fit <- lm(courseAVGallsem~factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')

fit <- lm(projectAVGallsem~factor(CourseLocation),data=Data)
summary(fit)
drop1(fit,test = 'F')

fit <- lm(courseAVGallsem~factor(CourseLocation),data = subset(Data,courseAVGallsem!=-8))
summary(fit)
drop1(fit,test = 'F')

fit <- lm(projectAVGallsem~factor(CourseLocation),data = subset(Data,projectAVGallsem!=-8))
summary(fit)
drop1(fit,test = 'F')

#International

ggplot(data=Data.m.all, aes(x=type,y=Grade,col=factor(isIntl)))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  scale_color_discrete(name=' ',labels=c('Not international','International'))+
  xlab('Activity')+
  ylab('Average grade')

ggplot(data=Data.m.all, aes(x=type,y=Grade))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  facet_wrap(~factor(isIntl),labeller = as_labeller(c('0'='Not internatinal','1'='International')))+
  scale_color_discrete(name=' ',labels=c('Not international','International'))+
  xlab('Activity')+
  ylab('Average grade')

fit <- lm(courseAVGallsem~factor(isIntl),data =Data)
summary(fit)

fit <- lm(projectAVGallsem~factor(isIntl),data = Data)
summary(fit)

fit <- lm(courseAVGallsem~factor(isIntl),data = subset(Data,courseAVGallsem!=-8))
summary(fit)

fit <- lm(projectAVGallsem~factor(isIntl),data = subset(Data,projectAVGallsem!=-8))
summary(fit)

#Comparing semester wise

ggplot(data=Data.m,aes(x=factor(sem),y=value,col=type))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  scale_color_discrete(name='')+
  scale_y_continuous(breaks = c(-8,-3,2,4,7,10,12))+
  xlab('Semester')+
  ylab('Average grade')

t.test(Data$courseAVG1sem,Data$projectAVG1sem,paired = T)
t.test(Data$courseAVG2sem,Data$projectAVG2sem,paired = T)
t.test(Data$courseAVG3sem,Data$projectAVG3sem,paired = T)
t.test(Data$courseAVG4sem,Data$projectAVG4sem,paired = T)
t.test(Data$courseAVG5sem,Data$projectAVG5sem,paired = T)
t.test(Data$courseAVG6sem,Data$projectAVG6sem,paired = T)


ggplot(data=Data.m,aes(x=factor(sem),y=value,col=type))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  facet_wrap(~factor(startaar))+
  scale_color_discrete(name='')+
  scale_size_continuous(name='Number of points',breaks = c(25,50,75,100))+
  xlab('Semester')+
  ylab('Average grade')

ggplot(data=Data.m,aes(x=factor(sem),y=value,col=type))+
  geom_boxplot(outlier.shape = NA)+
  geom_count(position = position_dodge(0.75),alpha=0.5)+
  facet_wrap(~CourseLocation,labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  scale_color_discrete(name='')+
  scale_size_continuous(name='Number of points', breaks=c(25,50,75,100))+
  xlab('Semester')+
  ylab('Average grade')


ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVGallsem,projectAVGallsem, use = "pairwise.complete.obs"),2)),x=8,y=-5))+
  guides(col=guide_legend(title='Year'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

##Looking for differences between cohorts
fit <- lm(projectAVGallsem~courseAVGallsem*factor(startaar), data = Data)
summary(fit)
drop1(fit,test = 'F')

cor(Data$courseAVGallsem,Data$projectAVGallsem,use="pairwise.complete.obs")
r2012 <- cor(Data2012$courseAVGallsem,Data2012$projectAVGallsem,use="pairwise.complete.obs")
r2013 <- cor(Data2013$courseAVGallsem,Data2013$projectAVGallsem,use="pairwise.complete.obs")
r2014 <- cor(Data2014$courseAVGallsem,Data2014$projectAVGallsem,use="pairwise.complete.obs")

cor_dat <- data.frame(label=paste(c('r='),c(round(r2012,2),round(r2013,2),round(r2014,2))),startaar=c(2012,2013,2014))


ggplot(aes(courseAVGallsem,projectAVGallsem,col=factor(startaar)), data = Data)+
  geom_point()+geom_smooth(aes(fill=factor(startaar)),method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, position = position_nudge(x=10,y=-13))+
  geom_text(data=cor_dat,aes(label=label,x=10,y=c(-2,-3.5,-5)),show.legend = F)+
  guides(col=guide_legend(title='Year'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')


ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,aes(label=label,x=7,y=-5))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')
#different slope for 2014

##Looking at differences between campus
fit <- lm(projectAVGallsem~courseAVGallsem*factor(CourseLocation), data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, Data$CourseLocation), function(X) cor(X$courseAVGallsem, X$projectAVGallsem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))

ggplot(aes(courseAVGallsem,projectAVGallsem,col=CourseLocation), data = Data)+
  geom_point()+geom_smooth(aes(fill=CourseLocation),method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, position = position_nudge(x=10,y=-13))+
  guides(col=guide_legend(title='Campus'))+
  geom_text(data=cor_dat,aes(label=label,x=10,y=c(-2,-3.5,-5)),show.legend = F)+
  scale_color_discrete(labels=c('Aalborg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')



ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~CourseLocation,labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=5, y=-5, label=label))+
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

cor <- round(sapply(split(Data, list(Data$CourseLocation,Data$startaar)), function(X) cor(X$courseAVGallsem, X$projectAVGallsem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=rep(c('A','E','K'),3),startaar=rep(c(2012,2013,2014),each=3))


ggplot(aes(courseAVGallsem,projectAVGallsem, col=CourseLocation), data = Data)+
  geom_point()+
  geom_smooth(aes(fill=CourseLocation),show.legend = F,method='lm',se=T,na.rm = T, fullrange=T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.x = 0,label.y = c(-3.5,-5,-6.5))+
  guides(col=guide_legend(title='Campus'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=rep(c(-3.5,-5,-6.5),3), label=label))+
  scale_color_discrete(labels=c('Aalborg','Esbjerg','Copenhagen'))+
  facet_wrap(~startaar)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~CourseLocation+startaar,labeller = labeller(CourseLocation=as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen'))))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')


##Looking at difference by international students
fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl), data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, list(Data$isIntl)), function(X) cor(X$courseAVGallsem, X$projectAVGallsem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),isIntl=c(0,1))

ggplot(aes(courseAVGallsem,projectAVGallsem,col=factor(isIntl)), data = Data)+
  geom_point()+geom_smooth(aes(fill=factor(isIntl)),show.legend=F,method='lm',se=T,na.rm = T)+
  guides(col=guide_legend(title=''))+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.x = 5,label.y = c(-2,-5))+
  geom_text(data=cor_dat,mapping = aes(x=10, y=c(-2,-5), label=label))+
  scale_color_discrete(labels=c('Not International','International'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')



ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(isIntl),labeller = as_labeller(c('0'='Not international','1'='International')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl)*factor(startaar), data = Data)
summary(fit)
drop1(fit,test = 'F')


cor <- round(sapply(split(Data, list(Data$isIntl,Data$startaar)), function(X) cor(X$courseAVGallsem, X$projectAVGallsem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),isIntl=rep(c(0,1),3),startaar=rep(c(2012,2013,2014),each=2))


ggplot(aes(courseAVGallsem,projectAVGallsem, col=factor(isIntl)), data = Data)+
  geom_point()+
  geom_smooth(aes(fill=factor(isIntl)),show.legend = F,method='lm',se=T,na.rm = T)+
  guides(col=guide_legend(title=''))+
  scale_color_discrete(labels=c('Not international','international'))+
  facet_wrap(~startaar)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.x = 0,label.y = c(-5,-7))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=rep(c(-5,-7),3), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

ggplot(aes(courseAVGallsem,projectAVGallsem), data = Data)+
  geom_point()+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(isIntl)+startaar,labeller = as_labeller(c('0'='Not international','1'='International','2012'='2012','2013'='2013','2014'='2014')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=rep(c(-5,-7),3), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')

fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl),data=Data2012)
summary(fit)
drop1(fit,test = 'F')
#difference

fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl),data=Data2013)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVGallsem~courseAVGallsem+factor(isIntl),data=Data2013)
summary(fit)
drop1(fit,test = 'F')
#no difference

fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl),data=Data2014)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVGallsem~courseAVGallsem+factor(isIntl),data=Data2014)
summary(fit)
drop1(fit,test = 'F')


fit <- lm(projectAVGallsem~courseAVGallsem*factor(isIntl)*CourseLocation, data = Data)
summary(fit)
drop1(fit,test = 'F')

##Looking at data by semester

ggplot(aes(courseAVG1sem,projectAVG1sem), data = Data)+
  geom_count(alpha=0.5)+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVG1sem,projectAVG1sem, use = "pairwise.complete.obs"),2)),x=10,y=-5))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('First semester')

ggplot(aes(courseAVG2sem,projectAVG2sem), data = Data)+
  geom_count(alpha=0.5)+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVG2sem,projectAVG2sem, use = "pairwise.complete.obs"),2)),x=10,y=-5))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Second semester')

ggplot(aes(courseAVG3sem,projectAVG3sem), data = Data)+
  geom_count(alpha=0.5)+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVG3sem,projectAVG3sem, use = "pairwise.complete.obs"),2)),x=10,y=-5))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Third semester')

ggplot(aes(courseAVG4sem,projectAVG4sem), data = Data)+
  geom_count(alpha=0.5)+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVG4sem,projectAVG4sem, use = "pairwise.complete.obs"),2)),x=10,y=-5))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fourth semester')

ggplot(aes(courseAVG5sem,projectAVG5sem), data = Data)+
  geom_count(alpha=0.5)+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVG5sem,projectAVG5sem, use = "pairwise.complete.obs"),2)),x=10,y=-5))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fifth semester')

ggplot(aes(courseAVG6sem,projectAVG6sem), data = Data)+
  geom_count(alpha=0.5)+geom_smooth(method='lm',se=T,na.rm = T,show.legend = F)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.y = -5,label.x = 4)+
  geom_text(aes(label=paste('r=',round(cor(courseAVG6sem,projectAVG6sem, use = "pairwise.complete.obs"),2)),x=10,y=-5))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Sixth semester')

#Year
fit <- lm(projectAVG1sem~courseAVG1sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG1sem~courseAVG1sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
#No difference between years
cor <- round(sapply(split(Data, list(Data$startaar)), function(X) cor(X$courseAVG1sem, X$projectAVG1sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),startaar=c(2012,2013,2014))

ggplot(aes(courseAVG1sem,projectAVG1sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(startaar)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('First semester')




ggplot(aes(courseAVG1sem,projectAVG1sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('First semester')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem)& !projectAVG1sem%in%c(-0.5,0)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Grade in project')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem) & !projectAVG1sem%in%c(-0.5,0)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Grade in project')+
  ggtitle('First semester')


fit <- lm(projectAVG2sem~courseAVG2sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG2sem~courseAVG2sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
#different intercepts

cor <- round(sapply(split(Data, list(Data$startaar)), function(X) cor(X$courseAVG2sem, X$projectAVG2sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),startaar=c(2012,2013,2014))

ggplot(aes(courseAVG2sem,projectAVG2sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(startaar)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Second semester')


  

ggplot(aes(courseAVG2sem,projectAVG2sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  scale_size_continuous(name='Number of points')+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Second semester')

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
  xlab('Average grade in projects')+
  ggtitle('Second semester')

fit <- lm(projectAVG3sem~courseAVG3sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
cor <- round(sapply(split(Data, list(Data$startaar)), function(X) cor(X$courseAVG3sem, X$projectAVG3sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),startaar=c(2012,2013,2014))


ggplot(aes(courseAVG3sem,projectAVG3sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(startaar)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Third semester')


  

ggplot(aes(courseAVG3sem,projectAVG3sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Third semester')

ggplot(aes(factor(projectAVG3sem),courseAVG3sem,col=factor(startaar)), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & projectAVG3sem!=5))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)+
  ggtitle('Third semester')

ggplot(aes(factor(projectAVG3sem),courseAVG3sem), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & projectAVG3sem!=5))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
  facet_wrap(~factor(startaar))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  ggtitle('Third semester')

fit <- lm(projectAVG4sem~courseAVG4sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG4sem~courseAVG4sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, list(Data$startaar)), function(X) cor(X$courseAVG4sem, X$projectAVG4sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),startaar=c(2012,2013,2014))


ggplot(aes(courseAVG4sem,projectAVG4sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(startaar)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fourth semester')


  

ggplot(aes(courseAVG4sem,projectAVG4sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fourth semester')

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
  xlab('Average grade in projects')+
  ggtitle('Fourth semester')
#5 semester
fit <- lm(projectAVG5sem~courseAVG5sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG5sem~courseAVG5sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, list(Data$startaar)), function(X) cor(X$courseAVG5sem, X$projectAVG5sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),startaar=c(2012,2013,2014))
 

ggplot(aes(courseAVG5sem,projectAVG5sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(startaar)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fifth semester')

 

ggplot(aes(courseAVG5sem,projectAVG5sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fifth semester')

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
  xlab('Average grade in projects')+
  ggtitle('Ffifth semester')

#6 semester
fit <- lm(projectAVG6sem~courseAVG6sem*factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG6sem~courseAVG6sem+factor(startaar),data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, list(Data$startaar)), function(X) cor(X$courseAVG6sem, X$projectAVG6sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),startaar=c(2012,2013,2014))
  

ggplot(aes(courseAVG6sem,projectAVG6sem,col=factor(startaar)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(startaar)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Year'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Sixth semester')



ggplot(aes(courseAVG6sem,projectAVG6sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  facet_wrap(~factor(startaar))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Sixth semester')

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
  xlab('Average grade in projects')+
  ggtitle('Sixth semester')

#Campus
fit <- lm(projectAVG1sem~courseAVG1sem*factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG1sem~courseAVG1sem+factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
#No difference
cor <- round(sapply(split(Data, list(Data$CourseLocation)), function(X) cor(X$courseAVG1sem, X$projectAVG1sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))

ggplot(aes(courseAVG1sem,projectAVG1sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(CourseLocation)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE, label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  scale_color_discrete(label=c('Aalboeg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('First semester')




ggplot(aes(courseAVG1sem,projectAVG1sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
  facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('First semester')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem)& !projectAVG1sem%in%c(-0.5,0)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Grade in project')

ggplot(aes(factor(projectAVG1sem),courseAVG1sem), data =subset(Data, !is.na(projectAVG1sem) & !is.na(courseAVG1sem) & !projectAVG1sem%in%c(-0.5,0)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  ylab('Average grade in courses')+
  xlab('Grade in project')+
  ggtitle('First semester')


fit <- lm(projectAVG2sem~courseAVG2sem*factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG2sem~courseAVG2sem+factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
#different intercepts

cor <- round(sapply(split(Data, list(Data$CourseLocation)), function(X) cor(X$courseAVG2sem, X$projectAVG2sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))

ggplot(aes(courseAVG2sem,projectAVG2sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(CourseLocation)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  scale_color_discrete(label=c('Aalboeg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Second semester')




ggplot(aes(courseAVG2sem,projectAVG2sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
  scale_size_continuous(name='Number of points')+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Second semester')

ggplot(aes(factor(projectAVG2sem),courseAVG2sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG2sem) & !is.na(courseAVG2sem)& !projectAVG2sem%in%c(4.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')

ggplot(aes(factor(projectAVG2sem),courseAVG2sem), data =subset(Data, !is.na(projectAVG2sem) & !is.na(courseAVG2sem) & !projectAVG2sem%in%c(4.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  ggtitle('Second semester')

fit <- lm(projectAVG3sem~courseAVG3sem*factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
cor <- round(sapply(split(Data, list(Data$CourseLocation)), function(X) cor(X$courseAVG3sem, X$projectAVG3sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))


ggplot(aes(courseAVG3sem,projectAVG3sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(CourseLocation)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  scale_color_discrete(label=c('Aalboeg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Third semester')




ggplot(aes(courseAVG3sem,projectAVG3sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Third semester')

ggplot(aes(factor(projectAVG3sem),courseAVG3sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & projectAVG3sem!=5))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)+
  ggtitle('Third semester')

ggplot(aes(factor(projectAVG3sem),courseAVG3sem), data =subset(Data, !is.na(projectAVG3sem) & !is.na(courseAVG3sem) & projectAVG3sem!=5))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  ggtitle('Third semester')

fit <- lm(projectAVG4sem~courseAVG4sem*factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')


cor <- round(sapply(split(Data, list(Data$CourseLocation)), function(X) cor(X$courseAVG4sem, X$projectAVG4sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))


ggplot(aes(courseAVG4sem,projectAVG4sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(CourseLocation)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  scale_color_discrete(label=c('Aalboeg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fourth semester')




ggplot(aes(courseAVG4sem,projectAVG4sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=T,na.rm = T)+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fourth semester')

ggplot(aes(factor(projectAVG4sem),courseAVG4sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG4sem) & !is.na(courseAVG4sem) & !projectAVG4sem%in%c(5.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG4sem),courseAVG4sem), data =subset(Data, !is.na(projectAVG4sem) & !is.na(courseAVG4sem) & !projectAVG4sem%in%c(5.5,8.5)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  ggtitle('Fourth semester')
#5 semester
fit <- lm(projectAVG5sem~courseAVG5sem*factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG5sem~courseAVG5sem+factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, list(Data$CourseLocation)), function(X) cor(X$courseAVG5sem, X$projectAVG5sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))


ggplot(aes(courseAVG5sem,projectAVG5sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(CourseLocation)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  scale_color_discrete(label=c('Aalboeg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fifth semester')



ggplot(aes(courseAVG5sem,projectAVG5sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Fifth semester')

ggplot(aes(factor(projectAVG5sem),courseAVG5sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG5sem) & !is.na(courseAVG5sem) & !projectAVG5sem%in%c(3)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG5sem),courseAVG5sem), data =subset(Data, !is.na(projectAVG5sem) & !is.na(courseAVG5sem) & !projectAVG5sem%in%c(3)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  ggtitle('Ffifth semester')

#6 semester
fit <- lm(projectAVG6sem~courseAVG6sem*factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')
fit <- lm(projectAVG6sem~courseAVG6sem+factor(CourseLocation),data = Data)
summary(fit)
drop1(fit,test = 'F')

cor <- round(sapply(split(Data, list(Data$CourseLocation)), function(X) cor(X$courseAVG6sem, X$projectAVG6sem, use="pairwise.complete.obs")),2)
cor_dat <- data.frame(label=paste(c('r='),cor),CourseLocation=c('A','E','K'))


ggplot(aes(courseAVG6sem,projectAVG6sem,col=factor(CourseLocation)), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(aes(fill=factor(CourseLocation)),show.legend = F,method='lm',se=T,na.rm = T)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label..)), 
               parse = TRUE,label.x = 2,label.y = c(-3,-5,-7))+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  geom_text(data=cor_dat,mapping = aes(x=8, y=c(-3,-5,-7), label=label),show.legend = F)+
  scale_color_discrete(label=c('Aalboeg','Esbjerg','Copenhagen'))+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Sixth semester')



ggplot(aes(courseAVG6sem,projectAVG6sem), data = Data)+
  geom_count(alpha=0.5)+
  geom_smooth(method='lm',se=FALSE,na.rm = T)+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  geom_text(data=cor_dat,mapping = aes(x=7, y=-5, label=label))+
  scale_size_continuous(name='Number of points')+
  xlab('Average grade in courses')+
  ylab('Average grade in projects')+
  ggtitle('Sixth semester')

ggplot(aes(factor(projectAVG6sem),courseAVG6sem,col=factor(CourseLocation)), data =subset(Data, !is.na(projectAVG6sem) & !is.na(courseAVG6sem) & !projectAVG6sem%in%c(8.5,9.5,11)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(col=guide_legend(title='Campus'),size=guide_legend(title = 'Number of points'))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  scale_y_continuous(breaks = -8:12)

ggplot(aes(factor(projectAVG6sem),courseAVG6sem), data =subset(Data, !is.na(projectAVG6sem) & !is.na(courseAVG6sem) & !projectAVG6sem%in%c(8.5,9.5,11)))+
  geom_boxplot(na.rm = T, outlier.shape = NA)+
  geom_count(position=position_dodge(0.75),alpha=0.5)+
  guides(size=guide_legend(title = 'Number of points'))+
    facet_wrap(~factor(CourseLocation),labeller = as_labeller(c('A'='Aalborg','E'='Esbjerg','K'='Copenhagen')))+
  ylab('Average grade in courses')+
  xlab('Average grade in projects')+
  ggtitle('Sixth semester')

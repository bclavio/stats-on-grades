library(xlsx)
library(sqldf)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(MASS)
library(manipulate)

myWD <- ifelse(grep("BiancaClavio", getwd()), 'C:/Users/BiancaClavio/Dropbox/drop out initiative/dataAnalysis', "~/Dropbox/drop out initiative/dataAnalysis/")
setwd(myWD)

#before import save through google docs as csv to get rid of encoding problem with Danish letters

dfCGn <-read.csv("gradesTil2017Mar.csv", header = TRUE, fill=TRUE, sep = ",")
gradesPassedLUVec<-c('02'=1,'4'=1,'7'=1,'10'=1,'12'=1,'00'=0,'-3'=0,'B'=1,'EB'=-1,'U'=-1,'I'=-1)
dfCGn$ExamPassed<-gradesPassedLUVec[as.character(dfCGn$KARAKTER)]
dfCGn$aktivitet<-as.factor(dfCGn$aktivitet)
sqldf('select aktivitet, count(aktivitet) from dfCGn group by aktivitet order by count(aktivitet)')


## Packages
library(knitr)
library(rmarkdown)
library(xlsx)
library(sqldf)
library(plyr)
library(dplyr)
library(reshape2)
library(MASS)
library(manipulate)
library(lubridate)
library(stringr)
library(CTT)
library(Hmisc)
library(psych)
library(ggplot2)
library(reshape2)
library(ecdfHT)
library(compare)


### Import GPRO data

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/'} else {"~/SVN/01Projects/"}
setwd(SVNData)

# GPRO midterm grades from AAL and CPH
dfGPROmidtermAAL <- read.csv("GPRO/MidtermExamAAL-grades_16-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROmidtermCPH <- read.csv("GPRO/MidtermExamCPH-grades_16-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# Question topics for each question in the midterm exam and for the self-assessments in lecture 1-7 (before the midterm exam)):
dfGPROtopics <- read.csv("GPRO/GPRO_question_topics.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
#dfGPROtopicsT <- t(dfGPROtopics)

# gradebook
GPROgradebook <- read.csv("GPRO/GPRO-gradebook.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE, na.strings=c("-","NA"))
tutoring <- read.csv("GPRO/tutoring.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE, strip.white=TRUE)


dfGPROSA1 <- read.csv("GPRO/SA/SA1-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROSA2 <- read.csv("GPRO/SA/SA2-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROSA3 <- read.csv("GPRO/SA/SA3-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROSA4 <- read.csv("GPRO/SA/SA4-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROSA5 <- read.csv("GPRO/SA/SA5-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROSA6 <- read.csv("GPRO/SA/SA6-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROSA7 <- read.csv("GPRO/SA/SA7-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# adss a variable to differentiate between the SA's
dfGPROSA1['Quiz'] <- "SA1"
dfGPROSA2['Quiz'] <- "SA2"
dfGPROSA3['Quiz'] <- "SA3"
dfGPROSA4['Quiz'] <- "SA4"
dfGPROSA5['Quiz'] <- "SA5"
dfGPROSA6['Quiz'] <- "SA6"
dfGPROSA7['Quiz'] <- "SA7"

# assigns all SA data frames to a list
dfList <- list(dfGPROSA1,dfGPROSA2,dfGPROSA3,dfGPROSA4,dfGPROSA5,dfGPROSA6,dfGPROSA7)
# renames the columns about the quiz questions
dfList <- lapply(dfList, function(x) { names(x)[11:(ncol(x)-1)] = names(dfGPROtopics)[3:(ncol(x)-11+2)]
  x} )
dfList <- lapply(dfList, function(x) { names(x)[10] <- "Grade"
  x} )
# changes the format of when the attempt was Completed
dfList <- lapply(dfList, function(x) { x$Completed = as.POSIXct(x$Completed, format="%d %B %Y %I:%M %p")
  x} )
# removes non-completed attempts
dfList <- lapply(dfList, function(x) { x = x[complete.cases(x$Completed),]
  x} )
# creates a numeric variable of it - for each dataset
dfList <- lapply(dfList, function(x) { x['CompletedNum'] = as.numeric(x$Completed)
  x} )
dfGPROSA <- rbind.fill(dfList[[2]],dfList[[3]],dfList[[4]],dfList[[5]],dfList[[6]],dfList[[7]],dfList[[1]])

# takes the lastest attempt of a student - for each SA
dfGPROSACutting <- NULL
for (i in 1:nrow(dfGPROSA)){
  dfGPROSACut <- subset(dfGPROSA, dfGPROSA$`Email address` %in% dfGPROSA$`Email address`[i])
  dfGPROSACut <- subset(dfGPROSACut,dfGPROSACut$Quiz %in% dfGPROSA$Quiz[i])
  dfGPROSACut <- dfGPROSACut[which.max(dfGPROSACut$CompletedNum),]
  dfGPROSACutting <- rbind(dfGPROSACutting,dfGPROSACut)
}
# removes duplicates
dfGPROSACutting <- unique(dfGPROSACutting)
dfGPROSACutting[11:22] <- as.numeric(unlist(dfGPROSACutting[11:22]))
#dfGPROSACutting['ID']<-seq(1:nrow(dfGPROSACutting))
dfGPROSACutting1 <- data.frame(dfGPROSACutting$`Email address`,dfGPROSACutting[11:23])

dfGPROSA1cut <- subset(dfGPROSACutting1, grepl("SA1",dfGPROSACutting1$Quiz))
dfGPROSA2cut <- subset(dfGPROSACutting1, grepl("SA2",dfGPROSACutting1$Quiz))
dfGPROSA3cut <- subset(dfGPROSACutting1, grepl("SA3",dfGPROSACutting1$Quiz))
dfGPROSA4cut <- subset(dfGPROSACutting1, grepl("SA4",dfGPROSACutting1$Quiz))
dfGPROSA5cut <- subset(dfGPROSACutting1, grepl("SA5",dfGPROSACutting1$Quiz))
dfGPROSA6cut <- subset(dfGPROSACutting1, grepl("SA6",dfGPROSACutting1$Quiz))
dfGPROSA7cut <- subset(dfGPROSACutting1, grepl("SA7",dfGPROSACutting1$Quiz))

# merge the data: one entry per student??
names(dfGPROSA1cut)[2:8] <- c('SA1Q1','SA1Q2','SA1Q3','SA1Q4','SA1Q5','SA1Q6','SA1Q7')
names(dfGPROSA2cut)[2:13] <- c('SA2Q1','SA2Q2','SA2Q3','SA2Q4','SA2Q5','SA2Q6','SA2Q7','SA2Q8','SA2Q9','SA2Q10','SA2Q11','SA2Q12')
names(dfGPROSA3cut)[2] <- c('SA3Q1')
names(dfGPROSA4cut)[2:12] <- c('SA4Q1','SA4Q2','SA4Q3','SA4Q4','SA4Q5','SA4Q6','SA4Q7','SA4Q8','SA4Q9','SA4Q10','SA4Q11')
names(dfGPROSA5cut)[2:4] <- c('SA5Q1','SA5Q2','SA5Q3')
names(dfGPROSA6cut)[2:6] <- c('SA6Q1','SA6Q2','SA6Q3','SA6Q4','SA6Q5')
names(dfGPROSA7cut)[2:4] <- c('SA7Q1','SA7Q2','SA7Q3')


# SA7 is excluded

dfGPROSA1cut <- dfGPROSA1cut[,-c(9:ncol(dfGPROSA1cut))]
dfGPROSA2cut <- dfGPROSA2cut[,-ncol(dfGPROSA2cut)]
dfGPROSA3cut <- dfGPROSA3cut[,-c(3:ncol(dfGPROSA3cut))]
dfGPROSA4cut <- dfGPROSA4cut[,-c(13:ncol(dfGPROSA4cut))]
dfGPROSA5cut <- dfGPROSA5cut[,-c(5:ncol(dfGPROSA5cut))]
dfGPROSA6cut <- dfGPROSA6cut[,-c(7:ncol(dfGPROSA6cut))]
#dfGPROSA7cut <- dfGPROSA7cut[,-c(5:ncol(dfGPROSA7cut))]

dfGPROSAfinal <- Reduce(function(x, y) merge(x, y, all=TRUE), list(dfGPROSA1cut,dfGPROSA2cut,dfGPROSA3cut,dfGPROSA4cut,dfGPROSA5cut,dfGPROSA6cut)) #,dfGPROSA7cut))
dfGPROSAfinal[is.na(dfGPROSAfinal)] <- 0

CourseIntroduction <- data.frame(dfGPROSAfinal$dfGPROSACutting..Email.address., rowSums(dfGPROSAfinal[c(2:8)])-0.1)
VariablesDataMaths <- data.frame(dfGPROSAfinal$dfGPROSACutting..Email.address., rowSums(dfGPROSAfinal[c(9:10)])-0.1)
# excluded SA3
Branching <- data.frame(dfGPROSAfinal$dfGPROSACutting..Email.address., rowSums(dfGPROSAfinal[c(22:32)])-0.1)
Looping <- data.frame(dfGPROSAfinal$dfGPROSACutting..Email.address., rowSums(dfGPROSAfinal[c(33:35)])-0.1)
Functions <- data.frame(dfGPROSAfinal$dfGPROSACutting..Email.address., rowSums(dfGPROSAfinal[c(36:40)])-0.1)

dfGPROSAMerged <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                          list(CourseIntroduction, VariablesDataMaths, Branching, Looping, Functions))
names(dfGPROSAMerged) <- c('Email', 'CourseIntroduction', 'VariablesDataMaths', 'Branching', 'Looping', 'Functions')
dfGPROSAMerged <- sqldf('select * from dfGPROSAMerged group by "Email"')

# NEW 
normalizeSA <- function(x) {
  return ((x - min(x)) / (100 - min(x)))
}

dfGPROSAMerged1 <- dfGPROSAMerged[2:6]
dfGPROSAMerged1 <- as.data.frame(lapply(dfGPROSAMerged1, normalizeSA))
dfGPROSAMerged1['Email'] <- dfGPROSAMerged$Email
#dfGPROSAMerged1['type'] <- 'SA'

#dfGPROmidterm <- data.frame(lapply(dfGPROmidterm, function(x) { gsub("-", NA, x) }))
dfGPROmidtermCPH["Campus"]<-"CPH"
dfGPROmidtermAAL["Campus"]<-"AAL"

dfGPROmidterm <- rbind(dfGPROmidtermCPH,dfGPROmidtermAAL) 
names(dfGPROmidterm)[1] <- "Surname"
dfGPROmidterm <- dfGPROmidterm[!grepl("Overall average", dfGPROmidterm$Surname),]
dfGPROmidterm <- dfGPROmidterm[!grepl("User 01", dfGPROmidterm$Surname),]
dfGPROmidterm["rowID"] <- seq(1:nrow(dfGPROmidterm))
dfGPROmidterm$Surname <- gsub("-", " ", dfGPROmidterm$Surname)
dfGPROmidterm$`First name` <- gsub("-", " ", dfGPROmidterm$`First name`) 
dfGPROmidterm["Name"] <- paste(dfGPROmidterm$`First name`,dfGPROmidterm$Surname)
dfGPROmidterm <- data.frame(lapply(dfGPROmidterm, function(x) { gsub("-", 0, x) }))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfGPROmidtermStat <- data.frame(dfGPROmidterm[c(5,11:34)])
dfGPROmidtermStat[2:25] <- lapply(dfGPROmidterm[11:34], function(x) as.numeric(as.character(x)))
dfGPROmaxGrades <- data.frame("NA", lapply(dfGPROtopics[nrow(dfGPROtopics), -c(1:2)], function(x) as.numeric(as.character(x))) )
names(dfGPROmidtermStat) <- names(dfGPROmaxGrades) 
dfGPROmidtermStat <- rbind(dfGPROmidtermStat, dfGPROmaxGrades)
dfGPROmidtermStat[2:25] <- as.data.frame(lapply(dfGPROmidtermStat[2:25], normalize))
dfGPROmidtermStat <- dfGPROmidtermStat[-nrow(dfGPROmidtermStat),]
names(dfGPROmidtermStat)[1] <- "Email"
dfGPROmidtermStat <- sqldf('select * from dfGPROmidtermStat group by Email')

avgMidtermGrades <- NULL
avgMidtermGrades['Functions'] <- list(rowMeans(dfGPROmidtermStat[c(21:22)]))
avgMidtermGrades['Looping'] <- list(rowMeans(dfGPROmidtermStat[20]))
avgMidtermGrades['Branching'] <- list(rowMeans(dfGPROmidtermStat[c(16:19,23:25)]))
avgMidtermGrades['VariablesDataMaths'] <- list(rowMeans(dfGPROmidtermStat[4:14]))
avgMidtermGrades['CourseIntroduction'] <- list(rowMeans(dfGPROmidtermStat[2:3]))
avgMidtermGrades <- lapply(avgMidtermGrades, function(x) as.numeric(as.character(x)))
avgMidtermGrades['Email'] <- sqldf('select Email from dfGPROmidtermStat')

avgMidtermGrades <- merge(avgMidtermGrades,dfGPROSAMerged1[6:1], by="Email")
avgMidtermGrades["rowID"] <- list(seq(1:nrow(avgMidtermGrades)))
avgMidtermGradesMelt <- melt(avgMidtermGrades, id.vars = c("Email","rowID"))
avgMidtermGradesMelt['type'] <- ifelse(grepl('x',avgMidtermGradesMelt$variable), 'Midterm','SA')

ggplot(dfGPROmidtermStat[2:25], aes(rowSums(dfGPROmidtermStat[2:25]))) + geom_density()+
  scale_x_continuous(breaks=seq(0,30,1), limits = c(0,30), name ="Total score")
# ggplot(GPROprogress, aes( x=GPROprogress$PriorEXP, y=GPROprogress$MTscore))+ geom_density()+
#   +     scale_y_continuous(breaks=seq(0,30,1), limits = c(0,30), name ="Total score")
#ggplot(GPROprogress, aes(x=MTscore, y=priorExp)) + geom_point()

# dataset: 63 students
GPROgradebookPlot <- subset(GPROgradebook2, GPROgradebook2$Email %in% personalized_infoGPRO$Email)
plot(GPROgradebookPlot$MT, GPROgradebookPlot$SAavgScore)
plot(GPROgradebookPlot$MT, GPROgradebookPlot$SAavgScoreNA, pch=unclass(PrioEXP))
legend("bottomright", legend=c("beginner","novice"), pch=c(1,3))

GPROgradebookPlot$PriorEXP <- gsub("beginner","1",GPROgradebookPlot$PriorEXP)
GPROgradebookPlot$PriorEXP <- gsub("novice","1",GPROgradebookPlot$PriorEXP)
GPROgradebookPlot$PriorEXP <- gsub("intermediate","2",GPROgradebookPlot$PriorEXP)
GPROgradebookPlot$PriorEXP <- gsub("expert","2",GPROgradebookPlot$PriorEXP)
GPROgradebookPlot$PriorEXP <- as.factor(GPROgradebookPlot$PriorEXP)


# shows when the inexperienced programmers complete the SA and score high in them, they get a high MT score. 
# This same is not applicaple for experienced prgrammers
ggplot(GPROgradebookPlot, aes(x=SAavgScore, y=MT, shape=PriorEXP, color=PriorEXP)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=PriorEXP)) #se=FALSE

# shows when the experienced programmers score high in completed SA's, they achieve a high MT score (don't know the cause and effect).
# When the inexperienced programmers score high in the SA's in these that they complete, they get a high MT score. 
ggplot(GPROgradebookPlot, aes(x=SAavgScoreNA, y=MT, shape=PriorEXP, color=PriorEXP)) +
  geom_point()  + 
  geom_smooth(method=lm, aes(fill=PriorEXP))


####################################################################
GPROgradebook1 <- GPROgradebook[,c(6:14)]
names(GPROgradebook1) <- c('Email','SA1','SA2','SA3','SA4','SA5','SA6','SA7','MT')
GPROgradebook1[89,9] <- 65.49 # the only re-exam
GPROprogress <- read.csv("GPRO/GPROprogress.csv", stringsAsFactors = FALSE) 
# 64 out of 110 AAL students with SSP, all with MT scores, excluded CPH
# half of the students are PHP and re-examiners
GPROprogressAAL <- subset(GPROprogress, grepl("AAL", GPROprogress$Campus))

GPROgradebook1['SAcompleted'] <- rowSums(!is.na(GPROgradebook1[2:8]))
GPROgradebook1['SAcompletedPERC'] <- (GPROgradebook1$SAcompleted/7)*100
is.num <- sapply(GPROgradebook1, is.numeric)
GPROgradebook1[is.num] <- sapply(GPROgradebook1[is.num], round, 2)
GPROgradebook1['SAavgScoreNA'] <- rowMeans(GPROgradebook1[2:8], na.rm=TRUE)

GPROgradebook1[is.na(GPROgradebook1)] <- 0
GPROgradebook1['SAavgScore'] <- rowMeans(GPROgradebook1[2:8]) # 130 rows

GPROgradebook2 <- merge(GPROgradebook1, tutoring, by="Email", all=TRUE) # 135 rows
#GPROgradebook2[is.na(GPROgradebook2)] <- "-"
names(GPROprogressAAL)[2] <- "Email"
GPROgradebook2 <- merge(GPROgradebook2, GPROprogressAAL, by="Email", all.x=TRUE) # 135, 
GPROgradebook2 <- GPROgradebook2[,-c(15:17)]

lowEXP <- subset(GPROgradebook2, grepl("beginner", GPROgradebook2$PriorEXP) | grepl("novice", GPROgradebook2$PriorEXP) ) # 54 students
# cor(lowEXP$MT, lowEXP$SAavgScore)     = 0.3728291
# cor(lowEXP$MT, lowEXP$SAavgScoreNA)   = 0.2701078
highEXP <- subset(GPROgradebook2, grepl("intermediate", GPROgradebook2$PriorEXP) | grepl("expert", GPROgradebook2$PriorEXP) ) # 54 students
# cor(highEXP$MT, highEXP$SAavgScore)   = 0.2244853
# cor(highEXP$MT, highEXP$SAavgScoreNA) = 0.7607267

#######################################################################

setwd('C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO')

dfGPROmidtermAAL <- subset(dfGPROmidterm, grepl("AAL",dfGPROmidterm$Campus))
names(dfGPROmidtermAAL)[5] <- "Email"
dfGPROmidtermAAL <- merge(dfGPROmidtermAAL,avgMidtermGrades, by="Email")
avgPerGPROAAL <- data.frame(avgMidtermGrades, apply(avgMidtermGrades[,2:6], 2, function(c) ecdf(c)(c))*100, 
                            apply(avgMidtermGrades[,7:11], 2, function(c) ecdf(c)(c))*100, dfGPROmidtermAAL$Name)
names(avgPerGPROAAL)[ncol(avgPerGPROAAL)] <- "Name"
avgPerGPROAAL['Campus'] <- "AAL"
avgPerGPROAAL['initials'] <-  gsub("@student.aau.dk", "",avgMidtermGrades[,1])
#avgPerGPROAAL <- merge(avgPerGPROAAL, dfGPROSAMerged1, by = "Email")

# removes re-examiners and pdp student
GPROgradebook3 <- GPROgradebook2[!is.na(GPROgradebook2$PriorEXP),]
avgPerGPROAAL <- subset(avgPerGPROAAL, avgPerGPROAAL$Email %in% GPROgradebook3$Email)

write.csv(avgPerGPROAAL,file = "studentDataGPROSA.csv")
personalized_infoGPROAAL <- read.csv(file = "studentDataGPROSA.csv")
write.csv(avgPerGPROAAL$Email,file = "avgPerGPROAALEmail.csv") # used in the other R script
write.csv(GPROgradebook2,file = "GPROgradebook2.csv")


# create one for AAL and one for CPH
for (i in 1:nrow(personalized_infoGPROAAL)){
  rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/stats-on-grades/docs/GPRO-AAL-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(" ", "", paste("GPRO-MT_", ifelse(personalized_infoGPROAAL$Campus[i] == 'AAL', 
                                                                       "AAL_Individual-StudentFeedback_", 
                                                                       "CPH_Individual-StudentFeedback_"),
                                                      personalized_infoGPROAAL$initials[i], ".pdf", sep='')),
                    output_dir = "handoutsGPROSA/")
}

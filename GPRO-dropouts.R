
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
library(stringr)
library(CTT)
#library(gsubfn)
library(Hmisc)
library(psych)
library(ggplot2)
library(reshape2)

library(ecdfHT)

# data for Brian: SSP, GPRO self-assessment quizzes (from gradebook) and the mid-term exam for both AAL and CPH 
# TODO: The time they use in quizzes can show their study effort. Include this as one of the variables in the analysis.


### Import SSP data

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/SSP/'} else {"~/SVN/01Projects/SSP/"}
setwd(SVNData)

dfQAGrades<-read.csv("QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)
dfSSPgradesCPH<-read.csv("SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesAAL<-read.csv("SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

####################################
###### the answers are not in use
dfSSPanswersCPH<-read.csv("SSPanswersTestCPH 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPanswersAAL<-read.csv("SSPanswersTestAAL 10-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("No influence","Not at all true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Limited influence","Slightly true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Some influence","Some influence",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Decisive influence","Completely true",dfSSPanswersAAL))
dfSSPanswersCPH["Campus"]<-"CPH"
dfSSPanswersAAL["Campus"]<-"AAL"
dfSSPanswers <- rbind(dfSSPanswersCPH,dfSSPanswersAAL) 
#dfSSPanswers[c('Response 93','Response 95','Response 96')] <- data.frame(lapply(dfSSPanswers[c('Response 93','Response 95','Response 96')], function(x) { gsub("-", 0, x) }))
###################################

dfSSPgradesCPH["Campus"]<-"CPH"
dfSSPgradesAAL["Campus"]<-"AAL"
dfSSPgradesAAL <- dfSSPgradesAAL[-nrow(dfSSPgradesAAL),]
dfSSPgradesCPH <- dfSSPgradesCPH[-nrow(dfSSPgradesCPH),]

dfSSPgrades <- rbind(dfSSPgradesCPH,dfSSPgradesAAL) 
dfSSPgrades <- dfSSPgrades[!grepl("In progress", dfSSPgrades$State),]
dfSSPgrades["rowID"] <- seq(1:nrow(dfSSPgrades))

# computes grades for Q93, Q95 and Q96 (note: change the question type next year to avoid this conversion)
# Problem Q93, Q95 and Q96 contained strings, so I had to change them manually:
# Q93 (study hours): 0 > x < 29 (0%) ; 30 > x < 34 (30%) ; 35 > x < 40 (60%) ; 41 > x (100%)
dfSSPgrades$`Q. 93 /0.09` [findInterval(dfSSPanswers$`Response 93`, c(0,30)) == 1L] <- 0
dfSSPgrades$`Q. 93 /0.09`[findInterval(dfSSPanswers$`Response 93`, c(30,35)) == 1L] <- 0.3
dfSSPgrades$`Q. 93 /0.09`[findInterval(dfSSPanswers$`Response 93`, c(35,40)) == 1L] <- 0.6
dfSSPgrades$`Q. 93 /0.09`[findInterval(dfSSPanswers$`Response 93`, c(40,1000)) == 1L] <- 0.9

# Q95 (related work): 0 = x (0%) ; 1 > x < 4 (30%) ; 5 > x < 9 (60%) ; 10 > x (100%)
dfSSPgrades$`Q. 95 /0.09`[findInterval(dfSSPanswers$`Response 95`, c(0,0)) == 1L] <- 0
dfSSPgrades$`Q. 95 /0.09`[findInterval(dfSSPanswers$`Response 95`, c(0,4)) == 1L] <- 0.3
dfSSPgrades$`Q. 95 /0.09`[findInterval(dfSSPanswers$`Response 95`, c(5,9)) == 1L] <- 0.6
dfSSPgrades$`Q. 95 /0.09`[findInterval(dfSSPanswers$`Response 95`, c(10,1000)) == 1L] <- 0.9

# Q95 (unrelated work): 0 = x (100%) ; 1 > x < 4 (60%) ; 5 > x < 9 (30%) ; 10 > x (0%)
dfSSPgrades$`Q. 96 /0.09`[findInterval(dfSSPanswers$`Response 96`, c(0,0)) == 1L] <- 0.9
dfSSPgrades$`Q. 96 /0.09`[findInterval(dfSSPanswers$`Response 96`, c(0,4)) == 1L] <- 0.6
dfSSPgrades$`Q. 96 /0.09`[findInterval(dfSSPanswers$`Response 96`, c(5,9)) == 1L] <- 0.3
dfSSPgrades$`Q. 96 /0.09`[findInterval(dfSSPanswers$`Response 96`, c(10,1000)) == 1L] <- 0



##### IMPORT allGPRO quizzes 

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO/'} else {"~/SVN/01Projects/GPRO/"}
setwd(SVNData)

# Note: I haven't imported the Midterm exam responses, but they are in SVN

# imports and merges the midterm exam grades from AAL and CPH
dfMidtermGradesCPH<-read.csv("MidtermExamCPH-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfMidtermGradesAAL<-read.csv("MidtermExamAAL-grades.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfMidtermGradesCPH["Campus"]<-"CPH"
dfMidtermGradesAAL["Campus"]<-"AAL"
dfMidtermGradesCPH <- dfMidtermGradesCPH[-nrow(dfMidtermGradesCPH),]
dfMidtermGradesAAL <- dfMidtermGradesAAL[-nrow(dfMidtermGradesAAL),]
dfMidtermGradesAAL <- dfMidtermGradesAAL[!grepl("tmpuser_20171005@its.aau.dk", dfMidtermGradesAAL$`Email address`),] # removes temp user
names(dfMidtermGradesAAL) <- names(dfMidtermGradesCPH) 
dfMidtermGrades <- rbind(dfMidtermGradesAAL, dfMidtermGradesCPH)
dfMidtermGrades$Institution <- NULL
dfMidtermGrades$Department <- NULL

# Note: Self assessment quizes are only used in AAL. 
# Imports the Midterm exam grade (note: the midterm exam is also in here):
dfGPROquizzesAAL<-read.csv("GPRO-gradebook-quiz.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# merges GPRO Midterm exam and SSP (not self-assessments)
names(dfSSPgrades)[1]<-"Surname"
names(dfMidtermGrades)[1]<-"Surname"
dfMED1data <- merge(dfSSPgrades, dfMidtermGrades, by= c("Surname", "First name", "Email address", "Campus"))
# Note: some data/students disappears in this merge: re-exams in GPRO, MED1 dropouts, merits to the GPRO exam

# Note: SSP has 111 questions, and GPRO has 24 questions.
setwd('C:/Users/BiancaClavio/Documents/stats-on-grades/')
write.csv(dfMED1data,file = "DropoutMED1.csv")




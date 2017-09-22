
### NOTES FROM SSP 2017:
# The study verification test (SSP) was conducted 15th of September 2017 for 1 hour on all MED1 student, in Aalborg at 9 am and in Copenhagen at 12:30 am.
# First result: A few students (around 7 students) dropped out in MED1, and they will be replaced with ~7 new students. 
# All active students took the test as requested.

### Data analysis of the SSP is required for:
# A)  High priority: Detecting students with high risk of dropping out. 
#     The student counselors should be informed about who is the high-risk dropout students so they can "take action". 
# B)  Low priority: Creating a tool for predicting dropouts 

### TODOS:
# Import remaining data from Moodle of the newcoming students
# Aggegation of variables
# Analysis for each student for the student counselors, such as https://www.ruffalonl.com/complete-enrollment-management/student-success/rnl-retention-management-system-plus/samples

library(xlsx)
library(sqldf)
library(plyr)
library(dplyr)
library(reshape2)
library(MASS)
library(manipulate)
library(stringr)
#library(gsubfn)


SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/SSP/'} else {"~/SVN/01Projects/SSP/"}
setwd(SVNData)

# Created manually from the SSP Moodle quiz, as the quiz questions from Moodle can be exported to anything useful outside Moodle
dfQAGrades<-read.csv("QuestionsOverview.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)

# SSP data retrieved from Moodle
dfSSPanswersCPH<-read.csv("SSPanswersTestCPH 21-09.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)
dfSSPanswersCPH["Campus"]<-"CPH"
dfSSPanswersAAL<-read.csv("SSPanswersTestAAL 21-09.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)
dfSSPanswersAAL["Campus"]<-"AAL"

# Answer types from questions 24-32 in AAL (No influence, Limited influence, Some influence, Decisive influence)
# are converted to the ones in CHP (Not at all true, Slightly true, Somewhat true, Completely true).
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("No influence","Not at all true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Limited influence","Slightly true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Some influence","Some influence",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Decisive influence","Completely true",dfSSPanswersAAL))
#dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsubfn(".", list("No influence"="Not at all true", "Limited influence"="Slightly true", "Some influence"="Some influence", "Decisive influence"="Completely true"), dfSSPanswersAAL))

# Combine data from AAL and CPH
dfSSPanswers <- rbind(dfSSPanswersAAL,dfSSPanswersCPH)

# Removes Institution and Department columns
dfSSPanswers <- dfSSPanswers[ ,-c(3:4)]

# TODO: Convert answer strings to numbers

# TODO: Perform one tailed t-test









### This script is a draft. The more cleaned up script is in the markdown version. 

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


############################
## IMPORT & DATA CLEANING ##
############################

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/SSP/'} else {"~/SVN/01Projects/SSP/"}
setwd(SVNData)

# Created manually from the SSP Moodle quiz, as the quiz questions from Moodle can be exported to anything useful outside Moodle
dfQAGrades<-read.csv("QuestionsOverview.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)

# SSP grades and answers retrieved from Moodle in Aalborg and Copenhagen
dfSSPgradesCPH<-read.csv("SSPgradesTestCPH 28-09.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, na.strings = c("-","Requires grading"))
dfSSPgradesAAL<-read.csv("SSPgradesTestAAL 28-09.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, na.strings = c("-","Requires grading"))
dfSSPanswersCPH<-read.csv("SSPanswersTestCPH 28-09.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)
dfSSPanswersAAL<-read.csv("SSPanswersTestAAL 28-09.csv",header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)

# Answer types from questions 24-32 in AAL (No influence, Limited influence, Some influence, Decisive influence)
# are converted to the ones in CHP (Not at all true, Slightly true, Somewhat true, Completely true).
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("No influence","Not at all true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Limited influence","Slightly true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Some influence","Some influence",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Decisive influence","Completely true",dfSSPanswersAAL))
#dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsubfn(".", list("No influence"="Not at all true", "Limited influence"="Slightly true", "Some influence"="Some influence", "Decisive influence"="Completely true"), dfSSPanswersAAL))

# Add campus variable
dfSSPgradesCPH["Campus"]<-"CPH"
dfSSPgradesAAL["Campus"]<-"AAL"
dfSSPanswersCPH["Campus"]<-"CPH"
dfSSPanswersAAL["Campus"]<-"AAL"

# Removes the last row for the dataset with grades (called overall)
dfSSPgradesAAL <- dfSSPgradesAAL[-nrow(dfSSPgradesAAL),]
dfSSPgradesCPH <- dfSSPgradesCPH[-nrow(dfSSPgradesCPH),]

# Combine data from AAL and CPH. 
# Note: Make sure that the two variables have the same number of observations.
names(dfSSPgradesAAL) <- names(dfSSPgradesCPH) # debugging of weird column names 
dfSSPgrades <- rbind(dfSSPgradesAAL,dfSSPgradesCPH) # error
dfSSPanswers <- rbind(dfSSPanswersAAL,dfSSPanswersCPH)

# Combines Surname with First name
dfSSPgrades["Name"] <- paste(dfSSPgrades$'First name', dfSSPgrades$Surname)
dfSSPanswers["Name"] <- paste(dfSSPanswers$'First name', dfSSPanswers$Surname)

# Removes Surname, First name, Institution, and Department columns.
dfSSPgrades <- dfSSPgrades[ ,-c(1:4)]
dfSSPanswers <- dfSSPanswers[ ,-c(1:4)]

# Replaces all values with NA for questions about study/working hours:
dfSSPgrades$`Q. 93 /0.09` <- NA
dfSSPgrades$`Q. 95 /0.09` <- NA
dfSSPgrades$`Q. 96 /0.09` <- NA


###########################
#####  DATA ANALYSIS  #####
###########################


# TODO: Compute grades statistics (avg, percent rank, frequency..) for each student

dfSSPgradesStat <- data.frame(dfSSPgrades[7:117])
dfSSPgradesStat["Mean"] <- rowMeans(dfSSPgradesStat[1:111], na.rm=TRUE)

fqGrades <- cut(dfSSPgradesStat$Mean, seq((min(dfSSPgradesStat$Mean)), (max(dfSSPgradesStat$Mean)), by=0.001))
fqGrades <- data.frame(table(fqGrades))
hist(dfSSPgradesStat$Mean, main="Histogram for Average Grades Per Student", xlab="Average Grade", xlim=c((min(dfSSPgradesStat$Mean)-0.01),(max(dfSSPgradesStat$Mean)+0.01)), ylim=c(0,50), las=1)
hist(dfSSPgradesStat$Mean, seq(0.0, 0.1, by=0.005))

quantile(dfSSPgradesStat, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)

perc.rank <- function(x) trunc(rank(x))/length(x)

percGrades <- dfSSPgradesStat[,1:4]
percGrades <- within(percGrades, xr <- perc.rank(dfSSPgradesStat$'Q..1..0.09'))

# work with this!
quantile(dfSSPgradesStat$'Q..2..0.09')
length(dfSSPgradesStat$'Q..2..0.09'[dfSSPgradesStat$'Q..2..0.09' <= 0.02]) / length(dfSSPgradesStat$'Q..2..0.09') * 100


# TODO: Compute grades statistics(avg, percent rank, frequency) for each category
dfSSPgradesStat["DemographicsMean"] <- rowMeans(dfSSPgradesStat[1:9], na.rm=TRUE)
dfSSPgradesStat["AttitudeMean"] <- rowMeans(dfSSPgradesStat[10:15], na.rm=TRUE)
dfSSPgradesStat["ReasonsMean"] <- rowMeans(dfSSPgradesStat[16:23], na.rm=TRUE)
dfSSPgradesStat["ChoiceMean"] <- rowMeans(dfSSPgradesStat[24:33], na.rm=TRUE)
dfSSPgradesStat["HSBehaveMean"] <- rowMeans(dfSSPgradesStat[34:45], na.rm=TRUE)
dfSSPgradesStat["HSTrustMean"] <- rowMeans(dfSSPgradesStat[46:50], na.rm=TRUE)
dfSSPgradesStat["BelongingMean"] <- rowMeans(dfSSPgradesStat[51:56], na.rm=TRUE)
dfSSPgradesStat["GritMean"] <- rowMeans(dfSSPgradesStat[57:61], na.rm=TRUE)
dfSSPgradesStat["GrowthMean"] <- rowMeans(dfSSPgradesStat[62:64], na.rm=TRUE)
dfSSPgradesStat["ControlMean"] <- rowMeans(dfSSPgradesStat[65:74], na.rm=TRUE)
dfSSPgradesStat["TraitsMean"] <- rowMeans(dfSSPgradesStat[75:87], na.rm=TRUE)
dfSSPgradesStat["AcademicMean"] <- rowMeans(dfSSPgradesStat[88:92], na.rm=TRUE)
dfSSPgradesStat["HoursMean"] <- rowMeans(dfSSPgradesStat[93:97], na.rm=TRUE)
dfSSPgradesStat["MedialogyMean"] <- rowMeans(dfSSPgradesStat[98:11], na.rm=TRUE)

dfSSPgradesClassMeans <- sqldf("select DemographicsMean, AttitudeMean, ReasonsMean, ChoiceMean, HSBehaveMean, HSTrustMean, BelongingMean, GritMean, GrowthMean, ControlMean, 
                                  TraitsMean, AcademicMean, HoursMean, MedialogyMean from dfSSPgradesStat")

# TODO: Perform one tailed t-test



# TODO: General summery and individual summary compared to the cohort. 


#write.csv(dfSSPgrades,file = "SSPgrades.csv")




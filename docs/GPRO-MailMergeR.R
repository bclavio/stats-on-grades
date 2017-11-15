
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
library(Hmisc)
library(psych)
library(ggplot2)
library(reshape2)
library(ecdfHT)



### Import GPRO data

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/'} else {"~/SVN/01Projects/"}
setwd(SVNData)

dfQAGrades<-read.csv("SSP/QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)
dfSSPgradesCPH<-read.csv("SSP/SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesAAL<-read.csv("SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# GPRO midterm grades from AAL and CPH
dfGPROmidtermAAL <- read.csv("GPRO/MidtermExamAAL-grades_15-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROmidtermCPH <- read.csv("GPRO/MidtermExamCPH-grades_15-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# Question topics for each question in the midterm exam and for the self-assessments in lecture 1-7 (before the midterm exam)):
dfGPROtopics <- read.csv("GPRO/GPRO_question_topics.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
#dfGPROmidterm <- data.frame(lapply(dfGPROmidterm, function(x) { gsub("-", NA, x) }))


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

# NEW
dfGPROmidtermCPH["Campus"]<-"CPH"
dfGPROmidtermAAL["Campus"]<-"AAL"


dfGPROmidterm <- rbind(dfGPROmidtermCPH,dfGPROmidtermAAL) 
names(dfGPROmidterm)[1] <- "Surname"
dfGPROmidterm <- dfGPROmidterm[!grepl("Overall average", dfGPROmidterm$Surname),]
dfGPROmidterm <- dfGPROmidterm[!grepl("User 01", dfGPROmidterm$Surname),]
dfGPROmidterm["rowID"] <- seq(1:nrow(dfGPROmidterm))
dfGPROmidterm$First.name <- gsub("-", " ", dfGPROmidterm$First.name)
dfGPROmidterm$Surname <- gsub("-", " ", dfGPROmidterm$Surname)
dfGPROmidterm["Name"] <- paste(dfGPROmidterm$First.name,dfGPROmidterm$Surname)
dfGPROmidterm <- data.frame(lapply(dfGPROmidterm, function(x) { gsub("-", 0, x) }))


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

# counts study hours for the campi
studyHoursTable <- data.frame(
    c(
    "0-29" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.0 & dfSSPgrades$Campus == "AAL", ]),
    "30-34" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.3 & dfSSPgrades$Campus == "AAL", ]),
    "35-40" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.6 & dfSSPgrades$Campus == "AAL", ]),
    "41+" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.9 & dfSSPgrades$Campus == "AAL", ]),
    "Total" = nrow(dfSSPgrades[dfSSPgrades$Campus == "AAL", ])),
    c(
    "0-29" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.0 & dfSSPgrades$Campus == "CPH", ]),
    "30-34" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.3 & dfSSPgrades$Campus == "CPH", ]),
    "35-40" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.6 & dfSSPgrades$Campus == "CPH", ]),
    "41+" = nrow(dfSSPgrades[dfSSPgrades$`Q. 93 /0.09` == 0.9 & dfSSPgrades$Campus == "CPH", ]),
    "Total" = nrow(dfSSPgrades[dfSSPgrades$Campus == "CPH", ])))
names(studyHoursTable)[1]<-"AAL"
names(studyHoursTable)[2]<-"CPH"


dfSSPgrades$`First name` <- gsub("-", " ", dfSSPgrades$`First name`)
dfSSPgrades <- data.frame(lapply(dfSSPgrades, function(x) { gsub("-", 0, x) }))
dfSSPgradesStat <- data.frame( lapply(dfSSPgrades[11:121], function(x) as.numeric(as.character(x))) )

# computes avg grade for each category and for each student
avgGrades <- NULL
avgGrades['Understanding of Medialogy'] <- list(rowMeans(dfSSPgradesStat[c(98:106)])) # removed 5
avgGrades['Study and work'] <- list(rowMeans(dfSSPgradesStat[93:97]))
avgGrades['Growth mindset'] <- list(rowMeans(dfSSPgradesStat[62:64]))
avgGrades['Grit'] <- list(rowMeans(dfSSPgradesStat[57:61]))
avgGrades['Study habits'] <- list(rowMeans(dfSSPgradesStat[65:68]))
avgGrades['High school habits'] <- list(rowMeans(dfSSPgradesStat[34:45]))
avgGrades['Social support for studying'] <- list(rowMeans(dfSSPgradesStat[c(2:4,7:8,55:56)])) # removed 4, added 2
avgGrades <- data.frame( lapply(avgGrades, function(x) as.numeric(as.character(x))) )
# New - check: avgGrades <- na.omit(avgGrades)

# NEW 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfGPROmidtermStat <- data.frame( lapply(dfGPROmidterm[11:34], function(x) as.numeric(as.character(x))) )
dfGPROmaxGrades <- data.frame( lapply(dfGPROtopics[nrow(dfGPROtopics), -c(1:2)], function(x) as.numeric(as.character(x))) )
names(dfGPROmidtermStat) <- names(dfGPROmaxGrades) 
dfGPROmidtermStat <- rbind(dfGPROmidtermStat, dfGPROmaxGrades)
dfGPROmidtermStat <- as.data.frame(lapply(dfGPROmidtermStat, normalize))
dfGPROmidtermStat <- dfGPROmidtermStat[-c(1),]

avgMidtermGrades <- NULL
avgMidtermGrades['If statements'] <- list(rowMeans(dfGPROmidtermStat[22:23]))
avgMidtermGrades['Functions'] <- list(rowMeans(dfGPROmidtermStat[20:21]))
avgMidtermGrades['Loops'] <- list(rowMeans(dfGPROmidtermStat[19]))
avgMidtermGrades['Nesting'] <- list(rowMeans(dfGPROmidtermStat[18]))
avgMidtermGrades['Logical expressions'] <- list(rowMeans(dfGPROmidtermStat[c(15:17,24)]))
avgMidtermGrades['Mouse and keyboard input'] <- list(rowMeans(dfGPROmidtermStat[14]))
avgMidtermGrades['Math and expressions'] <- list(rowMeans(dfGPROmidtermStat[10:13]))
avgMidtermGrades['Data type'] <- list(rowMeans(dfGPROmidtermStat[9]))
avgMidtermGrades['Variables'] <- list(rowMeans(dfGPROmidtermStat[3:8]))
avgMidtermGrades['General knowledge'] <- list(rowMeans(dfGPROmidtermStat[2]))
avgMidtermGrades['Programming rules'] <- list(rowMeans(dfGPROmidtermStat[1]))
avgMidtermGrades <- data.frame( lapply(avgMidtermGrades, function(x) as.numeric(as.character(x))) )
avgMidtermGrades["rowID"] <- seq(1:nrow(avgMidtermGrades))
avgMidtermGradesMelt <- melt(avgMidtermGrades, id.vars = "rowID")


ggplot(dfGPROmidtermStat, aes(rowSums(dfGPROmidtermStat))) + geom_density()+
  scale_x_continuous(breaks=seq(0,120,10), limits = c(0,120), name ="Total score")


# normalizes avg grades
norm.avgGrades <- as.data.frame(lapply(avgGrades, normalize))
norm.avgGrades["rowID"] <- seq(1:nrow(norm.avgGrades))
norm.avgGrades["Campus"] <- dfSSPgrades$Campus
ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_density()+
  scale_x_discrete(breaks=seq(7,11,1), name ="Total score")


selectedTopics <- dfSSPgradesStat[,c(2:4,7:8,34:45,55:56,65:68,57:61,62:64,93:97,98:106)]
dfSSPgradesSum <- data.frame(rowID = norm.avgGrades$rowID, campus = norm.avgGrades$Campus, gradeSums = rowSums(selectedTopics))
ggplot(dfSSPgradesStat, aes(rowSums(selectedTopics))) + geom_histogram()

highRiskStudents <- data.frame(gradeSums= dfSSPgradesSum$gradeSums[order(dfSSPgradesSum$gradeSums)[1:20]])
highRiskStudents<- data.frame(dfSSPgradesSum[dfSSPgradesSum$gradeSums %in% highRiskStudents$gradeSums,])
norm.avgGradesMelt <- NULL
norm.avgGradesMelt <- melt(norm.avgGrades, id.vars = c("rowID", "Campus"))
norm.avgGradesMelt$variable <- gsub("\\.", " ", norm.avgGradesMelt$variable)
norm.avgGradesMelt['highRisk'] <- ifelse(norm.avgGradesMelt$rowID %in% highRiskStudents$rowID, 1, 0)
norm.avgGradesMelt$variable <- factor(norm.avgGradesMelt$variable, levels = c('Understanding of Medialogy', 'Study and work', 
                                                                              'Growth mindset','Grit','Study habits',
                                                                              'High school habits','Social support for studying'),ordered = TRUE)

dfSSPanswers["rowID"] <- seq(1:nrow(dfSSPanswers))
studyHours <- dfSSPanswers [, c('rowID', 'Campus', 'Response 93')]
studyHours['highRisk'] <- ifelse(studyHours$rowID %in% highRiskStudents$rowID, 1, 0)
names(studyHours)[3]<-"hours"





#######################################################################

setwd('C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO')
avgPerGPRO <- data.frame(avgMidtermGrades, apply(avgMidtermGrades[,11:1], 2, function(c) ecdf(c)(c))*100, 
                         dfGPROmidterm$Campus, dfGPROmidterm$Name, dfGPROmidterm$Email.address )
names(avgPerGPRO)[ncol(avgPerGPRO)] <- "Email"
names(avgPerGPRO)[ncol(avgPerGPRO)-1] <- "Name"
names(avgPerGPRO)[ncol(avgPerGPRO)-2] <- "Campus"
avgPerGPRO['initials'] <-  gsub("@student.aau.dk", "",dfGPROmidterm[,5])
write.csv(avgPerGPRO,file = "studentDataGPRO.csv")
personalized_infoGPRO <- read.csv(file = "studentDataGPRO.csv")


for (i in 1:nrow(personalized_infoGPRO)){
  rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/stats-on-grades/docs/GPRO-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(" ", "", paste("GPRO-MT_", ifelse(personalized_infoGPRO$Campus[i] == 'AAL', 
                                                                       "AAL_Individual-StudentFeedback_", 
                                                                       "CPH_Individual-StudentFeedback_"),
                                                      personalized_infoGPRO$initials[i], ".pdf", sep='')),
                    output_dir = "handoutsGPRO/")
}

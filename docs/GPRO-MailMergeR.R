
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

# GPRO midterm grades from AAL and CPH
dfGPROmidtermAAL <- read.csv("GPRO/MidtermExamAAL-grades_16-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfGPROmidtermCPH <- read.csv("GPRO/MidtermExamCPH-grades_16-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# Question topics for each question in the midterm exam and for the self-assessments in lecture 1-7 (before the midterm exam)):
dfGPROtopics <- read.csv("GPRO/GPRO_question_topics.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
#dfGPROmidterm <- data.frame(lapply(dfGPROmidterm, function(x) { gsub("-", NA, x) }))

# NEW
dfGPROmidtermCPH["Campus"]<-"CPH"
dfGPROmidtermAAL["Campus"]<-"AAL"

dfGPROmidterm <- rbind(dfGPROmidtermCPH,dfGPROmidtermAAL) 
names(dfGPROmidterm)[1] <- "Surname"
dfGPROmidterm <- dfGPROmidterm[!grepl("Overall average", dfGPROmidterm$Surname),]
dfGPROmidterm <- dfGPROmidterm[!grepl("User 01", dfGPROmidterm$Surname),]
dfGPROmidterm["rowID"] <- seq(1:nrow(dfGPROmidterm))
dfGPROmidterm$`First name` <- gsub("-", " ", dfGPROmidterm$`First name`)
dfGPROmidterm$Surname <- gsub("-", " ", dfGPROmidterm$Surname)
dfGPROmidterm["Name"] <- paste(dfGPROmidterm$`First name`,dfGPROmidterm$Surname)
dfGPROmidterm <- data.frame(lapply(dfGPROmidterm, function(x) { gsub("-", 0, x) }))

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
dfGPROmidtermStat["rowID"] <- seq(1:nrow(dfGPROmidtermStat))

# avgMidtermGrades <- NULL
# avgMidtermGrades['If statements'] <- list(rowMeans(dfGPROmidtermStat[22:23]))
# avgMidtermGrades['Functions'] <- list(rowMeans(dfGPROmidtermStat[20:21]))
# avgMidtermGrades['Loops'] <- list(rowMeans(dfGPROmidtermStat[19]))
# avgMidtermGrades['Nesting'] <- list(rowMeans(dfGPROmidtermStat[18]))
# avgMidtermGrades['Logical expressions'] <- list(rowMeans(dfGPROmidtermStat[c(15:17,24)]))
# avgMidtermGrades['Mouse and keyboard input'] <- list(rowMeans(dfGPROmidtermStat[14]))
# avgMidtermGrades['Math and expressions'] <- list(rowMeans(dfGPROmidtermStat[10:13]))
# avgMidtermGrades['Data type'] <- list(rowMeans(dfGPROmidtermStat[9]))
# avgMidtermGrades['Variables'] <- list(rowMeans(dfGPROmidtermStat[3:8]))
# avgMidtermGrades['General knowledge'] <- list(rowMeans(dfGPROmidtermStat[2]))
# avgMidtermGrades['Programming rules'] <- list(rowMeans(dfGPROmidtermStat[1]))
# avgMidtermGrades <- data.frame( lapply(avgMidtermGrades, function(x) as.numeric(as.character(x))) )
# avgMidtermGrades["rowID"] <- seq(1:nrow(avgMidtermGrades))
# avgMidtermGradesMelt <- melt(avgMidtermGrades, id.vars = "rowID")

avgMidtermGrades <- NULL
avgMidtermGrades['rowID'] <- list(dfGPROmidtermStat$rowID)
avgMidtermGrades['CourseIntroduction'] <- list(rowMeans(dfGPROmidtermStat[1:2]))
avgMidtermGrades['VariablesDataMaths'] <- list(rowMeans(dfGPROmidtermStat[3:13]))
avgMidtermGrades['Branching'] <- list(rowMeans(dfGPROmidtermStat[c(15:18,22:24)]))
avgMidtermGrades['Looping'] <- list(rowMeans(dfGPROmidtermStat[19]))
avgMidtermGrades['Functions'] <- list(rowMeans(dfGPROmidtermStat[c(20:21)]))
avgMidtermGrades <- as.data.frame(lapply(avgMidtermGrades, function(x) as.numeric(as.character(x))))
#avgMidtermGrades['Email'] <- sqldf('select Email from dfGPROmidtermStat')

# remove the 
avgMidtermGradesMelt <- melt(avgMidtermGrades, id.vars = "rowID")
avgMidtermGradesMelt$variable <- gsub("Branching","Branching (Q15-Q18, Q22-Q24)",avgMidtermGradesMelt$variable)
avgMidtermGradesMelt$variable <- gsub("Looping","Looping (Q19)",avgMidtermGradesMelt$variable)
avgMidtermGradesMelt$variable <- gsub("Functions","Functions (Q20-Q21)",avgMidtermGradesMelt$variable)

ggplot(dfGPROmidtermStat, aes(rowSums(dfGPROmidtermStat))) + geom_density()+
  scale_x_continuous(breaks=seq(0,120,10), limits = c(0,120), name ="Total score")


# normalizes avg grades
# norm.avgGrades <- as.data.frame(lapply(avgGrades, normalize))
# norm.avgGrades["rowID"] <- seq(1:nrow(norm.avgGrades))
# norm.avgGrades["Campus"] <- dfSSPgrades$Campus
# ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_density()+
#   scale_x_discrete(breaks=seq(7,11,1), name ="Total score")


# selectedTopics <- dfSSPgradesStat[,c(2:4,7:8,34:45,55:56,65:68,57:61,62:64,93:97,98:106)]
# dfSSPgradesSum <- data.frame(rowID = norm.avgGrades$rowID, campus = norm.avgGrades$Campus, gradeSums = rowSums(selectedTopics))
# ggplot(dfSSPgradesStat, aes(rowSums(selectedTopics))) + geom_histogram()

# highRiskStudents <- data.frame(gradeSums= dfSSPgradesSum$gradeSums[order(dfSSPgradesSum$gradeSums)[1:20]])
# highRiskStudents<- data.frame(dfSSPgradesSum[dfSSPgradesSum$gradeSums %in% highRiskStudents$gradeSums,])
# norm.avgGradesMelt <- NULL
# norm.avgGradesMelt <- melt(norm.avgGrades, id.vars = c("rowID", "Campus"))
# norm.avgGradesMelt$variable <- gsub("\\.", " ", norm.avgGradesMelt$variable)
# norm.avgGradesMelt['highRisk'] <- ifelse(norm.avgGradesMelt$rowID %in% highRiskStudents$rowID, 1, 0)
# norm.avgGradesMelt$variable <- factor(norm.avgGradesMelt$variable, levels = c('Understanding of Medialogy', 'Study and work',
#                                                                              'Growth mindset','Grit','Study habits',
#                                                                              'High school habits','Social support for studying'),ordered = TRUE)

# dfSSPanswers["rowID"] <- seq(1:nrow(dfSSPanswers))
# studyHours <- dfSSPanswers [, c('rowID', 'Campus', 'Response 93')]
# studyHours['highRisk'] <- ifelse(studyHours$rowID %in% highRiskStudents$rowID, 1, 0)
# names(studyHours)[3]<-"hours"



# prior experience and MT score - students who took the SSP and MT
dfSSPgradesCPH<-read.csv("SSP/SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesAAL<-read.csv("SSP/SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesCPH["Campus"]<-"CPH"
dfSSPgradesAAL["Campus"]<-"AAL"
dfSSPgradesAAL <- dfSSPgradesAAL[-nrow(dfSSPgradesAAL),]
dfSSPgradesCPH <- dfSSPgradesCPH[-nrow(dfSSPgradesCPH),]
dfSSPgrades <- rbind(dfSSPgradesCPH,dfSSPgradesAAL) 
dfSSPgrades <- data.frame(dfSSPgrades$`Email address`, dfSSPgrades$`Q. 33 /0.09`)
names(dfSSPgrades)[1] <- "Email.address"
dfSSPgrades$dfSSPgrades..Q..33..0.09. <- gsub("0.03","novice", dfSSPgrades$dfSSPgrades..Q..33..0.09.)
dfSSPgrades$dfSSPgrades..Q..33..0.09. <- gsub("0.06","intermediate ", dfSSPgrades$dfSSPgrades..Q..33..0.09.)
dfSSPgrades$dfSSPgrades..Q..33..0.09. <- gsub("0.09","expert ", dfSSPgrades$dfSSPgrades..Q..33..0.09.)
dfSSPgrades$dfSSPgrades..Q..33..0.09. <- gsub("0.00","beginner", dfSSPgrades$dfSSPgrades..Q..33..0.09.)
dfSSPgrades$dfSSPgrades..Q..33..0.09. <- gsub("0","beginner", dfSSPgrades$dfSSPgrades..Q..33..0.09.)
GPROprogress <- data.frame(dfGPROmidterm$Email.address, dfGPROmidterm$Campus, dfGPROmidterm$Grade.100.00)
names(GPROprogress)[1] <- "Email.address"
GPROprogress <- merge(GPROprogress,dfSSPgrades, by= "Email.address", all=TRUE)
names(GPROprogress)[2] <- "Campus"
names(GPROprogress)[3] <- "MTscore"
names(GPROprogress)[4] <- "PriorEXP"
GPROprogress$MTscore <- as.numeric(as.character(GPROprogress$MTscore))

GPROprogress <- na.omit(GPROprogress)
GPROprogressStat1 <- as.data.frame(subset(GPROprogress[,-1], grepl("beginner",GPROprogress$PriorEXP)))
GPROprogressStat2 <- as.data.frame(subset(GPROprogress[,-1], grepl("novice",GPROprogress$PriorEXP)))
GPROprogressStat3 <- as.data.frame(subset(GPROprogress[,-1], grepl("intermediate",GPROprogress$PriorEXP)))
GPROprogressStat4 <- as.data.frame(subset(GPROprogress[,-1], grepl("expert",GPROprogress$PriorEXP)))
GPROprogressTable1 <- data.frame("beginner", min(GPROprogressStat1$MTscore), max(GPROprogressStat1$MTscore), mean(GPROprogressStat1$MTscore), length(GPROprogressStat1$MTscore))                                  
GPROprogressTable2 <- data.frame("novice", min(GPROprogressStat2$MTscore), max(GPROprogressStat2$MTscore), mean(GPROprogressStat2$MTscore), length(GPROprogressStat2$MTscore))                                  
GPROprogressTable3 <- data.frame("intermediate", min(GPROprogressStat3$MTscore), max(GPROprogressStat3$MTscore), mean(GPROprogressStat3$MTscore), length(GPROprogressStat3$MTscore))
GPROprogressTable4 <- data.frame("expert", min(GPROprogressStat4$MTscore), max(GPROprogressStat4$MTscore), mean(GPROprogressStat4$MTscore), length(GPROprogressStat4$MTscore))
names(GPROprogressTable1) <- c('PriorEXP', 'min','max','avg', 'N')
names(GPROprogressTable2) <- c('PriorEXP', 'min','max','avg', 'N')
names(GPROprogressTable3) <- c('PriorEXP', 'min','max','avg', 'N')
names(GPROprogressTable4) <- c('PriorEXP', 'min','max','avg', 'N')
GPROprogressTable <- rbind(GPROprogressTable1,GPROprogressTable2,GPROprogressTable3,GPROprogressTable4)
GPROprogressTable$PriorEXP <- gsub("beginner","beginner (no experience)",GPROprogressTable$PriorEXP)
GPROprogressTable$PriorEXP <- gsub("novice","novice (small projects less than 900 lines)",GPROprogressTable$PriorEXP)
GPROprogressTable$PriorEXP <- gsub("intermediate","intermediate (projects btw. 900-40.000 lines)",GPROprogressTable$PriorEXP)
GPROprogressTable$PriorEXP <- gsub("expert","expert (projects more than 40.000 lines)",GPROprogressTable$PriorEXP)

GPROprogressTable$avg <- round(GPROprogressTable$avg, digits = 2)


# dfGPROmidtermAAL <- subset(dfGPROmidterm, grepl("AAL",dfGPROmidterm$Campus))
# names(dfGPROmidtermAAL)[5] <- "Email"
# dfGPROmidtermAAL <- merge(dfGPROmidtermAAL,avgMidtermGrades, by="Email")
# avgPerGPROAAL <- data.frame(avgMidtermGrades, apply(avgMidtermGrades[,2:6], 2, function(c) ecdf(c)(c))*100, 
#                             apply(avgMidtermGrades[,7:11], 2, function(c) ecdf(c)(c))*100, dfGPROmidtermAAL$Name)
# names(avgPerGPROAAL)[ncol(avgPerGPROAAL)] <- "Name"
# avgPerGPROAAL['Campus'] <- "AAL"
# avgPerGPROAAL['initials'] <-  gsub("@student.aau.dk", "",avgMidtermGrades[,1])
# #avgPerGPROAAL <- merge(avgPerGPROAAL, dfGPROSAMerged1, by = "Email")
# write.csv(avgPerGPROAAL,file = "studentDataGPRO.csv")
# personalized_infoGPRO <- read.csv(file = "studentDataGPRO.csv")

setwd('C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO')
avgPerGPRO <- data.frame(avgMidtermGrades, apply(avgMidtermGrades[,2:6], 2, function(c) ecdf(c)(c))*100, 
                         dfGPROmidterm$Campus, dfGPROmidterm$Name, dfGPROmidterm$Email.address )
names(avgPerGPRO)[ncol(avgPerGPRO)] <- "Email"
names(avgPerGPRO)[ncol(avgPerGPRO)-1] <- "Name"
names(avgPerGPRO)[ncol(avgPerGPRO)-2] <- "Campus"
avgPerGPRO['initials'] <-  gsub("@student.aau.dk", "",dfGPROmidterm[,5])

#only CHP, PDP and people who didn't complete the SA's:
#removeSAstudents <- read.csv(file = "avgPerGPROAALEmail.csv")
avgPerGPRO <- subset(avgPerGPRO, avgPerGPRO$Campus != "AAL")
avgPerGPRO <- subset(avgPerGPRO, avgPerGPRO$Email %in% GPROprogress$Email.address) #&& (avgPerGPRO$Campus != "AAL"))


write.csv(avgPerGPRO,file = "studentDataGPRO.csv")
write.csv(GPROprogressTable,file = "GPROprogressTable.csv")
write.csv(GPROprogress,file = "GPROprogress.csv")
personalized_infoGPRO <- read.csv(file = "studentDataGPRO.csv")


#######################################################################
# Failing students (MT grade less than 64) - 173 students in total

# SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO'} else {"~/SVN/01Projects/"}
# setwd(SVNData)
# 
# # 81 students from AAlborg??
# GPROgradebook2 <- read.csv(file = "GPROgradebook2.csv")
# personalized_infoGPROAAL <- read.csv(file = "studentDataGPROSA.csv")
# interviewAAL <- subset(GPROgradebook2[2], GPROgradebook2$Email %in% personalized_infoGPROAAL$Email)
# 
# # 92 students from Copenhagen
# interviewCPH <- subset(personalized_infoGPRO[15], (grepl("CPH", personalized_infoGPRO$Campus)) &  
#                          personalized_infoGPRO$Email %in% personalized_info$email)
# 
# interviewees <- rbind(interviewAAL,interviewCPH)
# 
# # find their SSP and MT grade 
# 
# #highRiskStudents <- data.frame(gradeSums= dfSSPgradesSum$gradeSums[order(dfSSPgradesSum$gradeSums)[1:20]])
# #highRiskStudents<- data.frame(dfSSPgradesSum[dfSSPgradesSum$gradeSums %in% highRiskStudents$gradeSums,])


####################################################################



for (i in 1:nrow(personalized_infoGPRO)){
  rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/stats-on-grades/docs/GPRO-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(" ", "", paste("GPRO-MT_", ifelse(personalized_infoGPRO$Campus[i] == 'AAL', 
                                                                       "AAL_Individual-StudentFeedback_", 
                                                                       "CPH_Individual-StudentFeedback_"),
                                                      personalized_infoGPRO$initials[i], ".pdf", sep='')),
                    output_dir = "handoutsGPRO/")
}





############################################################################


# Sends mails with GPRO student in CPH reports:

# library(sendmailR)
# library(RDCOMClient)
# 
# for (i in 1:nrow(personalized_infoGPRO)){
#   subject <- "GPRO-MT_CPH_Individual-StudentFeedback"
#   attachmentPath <- gsub(" ", "", paste('C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO/handoutsGPRO/', subject,"_", personalized_infoGPRO$initial[i],'.pdf'))
#   name <- as.character(personalized_infoGPRO$Name[i])
#   #coordinator <- ifelse(personalized_infoGPRO$Campus[i] == 'AAL', "Rikke (rg@create.aau.dk)", "")
#   mailBody <- paste("Dear",name, "
# 
# We have analysed your answers from the midterm exam in introduction to programming (GPRO). The analysis attached in this mail compares your score with the first semester students in Aalborg and Copenhagen in 2017.
# Currently only the semester coordinator, supervisors, and the study board have access to this information. But you are welcome to share your student reports with peers, study counsellors, teachers, or others - should you desire to do so. Depending on individual results and needs we might approach you again in the future.
# 
# Please contact me via email (bcch@create.aau.dk) and add Dannie (dmk@create.aau.dk) in CC if you have any questions.
# 
# Best regards,
# Bianca
# 
#   ")
#   ## init com api
#   OutApp <- COMCreate("Outlook.Application")
#   ## create an email
#   outMail = OutApp$CreateItem(0)
#   ## configure  email parameter
#   outMail[["To"]] = as.character(personalized_infoGPRO$Email[i])
#   outMail[["Cc"]] = "dmk@create.aau.dk"
#   outMail[["subject"]] = subject
#   outMail[["body"]] = mailBody
#   outMail[["Attachments"]]$Add(attachmentPath)
#   
#   ####### Uncomment below to send mails:
#   ###outMail$Send()
# }





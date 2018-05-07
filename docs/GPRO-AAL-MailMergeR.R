
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
library(sm)


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

normalizeSA <- function(x) {
  return ((x - min(x)) / (100 - min(x)))
}
GPROgradebook0 <- GPROgradebook[, c(12:10,8:7)]
GPROgradebook0[is.na(GPROgradebook0)] <- 0
GPROgradebook0 <- as.data.frame(lapply(GPROgradebook0, normalizeSA))
GPROgradebook0['Email'] <- GPROgradebook$Email
names(GPROgradebook0) <- c('Functions','Looping','Branching','VariablesDataMaths','CourseIntroduction','Email')

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

avgMidtermGrades <-NULL
avgMidtermGrades['Functions'] <- list(rowMeans(dfGPROmidtermStat[c(21:22)]))
avgMidtermGrades['Looping'] <- list(rowMeans(dfGPROmidtermStat[20]))
avgMidtermGrades['Branching'] <- list(rowMeans(dfGPROmidtermStat[c(16:19,23:25)]))
avgMidtermGrades['VariablesDataMaths'] <- list(rowMeans(dfGPROmidtermStat[4:14]))
avgMidtermGrades['CourseIntroduction'] <- list(rowMeans(dfGPROmidtermStat[2:3]))
#avgMidtermGrades <- lapply(avgMidtermGrades, function(x) as.numeric(as.character(x)))
avgMidtermGrades['Email'] <- sqldf('select Email from dfGPROmidtermStat')

avgMidtermGrades <- merge(avgMidtermGrades,GPROgradebook0, by="Email")
avgMidtermGrades["rowID"] <- list(seq(1:nrow(avgMidtermGrades)))
avgMidtermGradesMelt <- melt(avgMidtermGrades, id.vars = c("Email","rowID"))
avgMidtermGradesMelt['type'] <- ifelse(grepl('x',avgMidtermGradesMelt$variable), 'Midterm','SA')

ggplot(dfGPROmidtermStat[2:25], aes(rowSums(dfGPROmidtermStat[2:25]))) + geom_density()+
  scale_x_continuous(breaks=seq(0,30,1), limits = c(0,30), name ="Total score")
# ggplot(GPROprogress, aes( x=GPROprogress$PriorEXP, y=GPROprogress$MTscore))+ geom_density()+
#   +     scale_y_continuous(breaks=seq(0,30,1), limits = c(0,30), name ="Total score")
#ggplot(GPROprogress, aes(x=MTscore, y=priorExp)) + geom_point()




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


# dataset: 63 students
personalized_infoGPRO$Email <- as.character(personalized_infoGPRO$Email)
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


###################################################################

GPROgradebook3 <- subset(GPROgradebook2, !is.na(GPROgradebook2$PriorEXP))
GPROgradebook3$PriorEXP <- gsub("expert", "intermediate", GPROgradebook3$PriorEXP)
GPROgradebook3$PriorEXP <- factor(GPROgradebook3$PriorEXP,
   levels = c('intermediate', 'beginner','novice'),ordered = TRUE)

sm.density.compare(GPROgradebook3$PriorEXP)

ggplot(GPROgradebook3, aes(x = MT, fill = PriorEXP)) + geom_density(alpha = 0.5) + ylim(0,0.025) + xlim(0,150)

#######################################################################

setwd('C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO')

#dfGPROmidtermAAL <- subset(dfGPROmidterm, grepl("AAL",dfGPROmidterm$Campus))
dfGPROmidtermAAL["Name"] <- paste(dfGPROmidtermAAL$`First name`,dfGPROmidtermAAL$Surname)
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
  rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/PBLstats-on-grades/docs/GPRO-AAL-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(" ", "", paste("GPRO-MT_", ifelse(personalized_infoGPROAAL$Campus[i] == 'AAL', 
                                                                       "AAL_Individual-StudentFeedback_", 
                                                                       "CPH_Individual-StudentFeedback_"),
                                                      personalized_infoGPROAAL$initials[i], ".pdf", sep='')),
                    output_dir = "handoutsGPROSA/")
}


############################################################################


# Sends mails with GPRO student in AAL reports:

# library(sendmailR)
# library(RDCOMClient)
# 
# for (i in 1:nrow(personalized_infoGPROAAL)){ 
#   subject <- "GPRO-MT_AAL_Individual-StudentFeedback"
#   attachmentPath <- gsub(" ", "", paste('C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO/handoutsGPROSA/', subject,"_", as.character(personalized_infoGPROAAL$initials[i]),'.pdf'))
#   name <- as.character(personalized_infoGPROAAL$Name[i])
#   #coordinator <- ifelse(personalized_infoGPROAAL$Campus[i] == 'AAL', "", "")
#   mailBody <- paste("Dear",name, "
# 
# We have analysed your answers from the midterm exam in introduction to programming (GPRO). The analysis attached in this mail compares your score with the first semester students in Aalborg and Copenhagen in 2017.
# Currently only the semester coordinator, supervisors, and the study board have access to this information. But you are welcome to share your student reports with peers, study counsellors, teachers, or others - should you desire to do so. Depending on individual results and needs we might approach you again in the future.
# 
# Please contact me via email (bcch@create.aau.dk) and add Rikke (rg@create.aau.dk) in CC if you have any questions.
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
#   outMail[["To"]] = as.character(personalized_infoGPROAAL$Email[i])
#   outMail[["subject"]] = subject
#   outMail[["body"]] = mailBody
#   outMail[["Attachments"]]$Add(attachmentPath)
#   
#   ####### Uncomment below to send mails:
#   ####outMail$Send()
# }

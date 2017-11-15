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
names(dfSSPgrades)[1]<-"Surname"

dfSSPgrades$`First name` <- gsub("-", " ", dfSSPgrades$`First name`)
dfSSPgrades$Surname <- gsub("-", " ", dfSSPgrades$Surname)
dfSSPgrades <- data.frame(lapply(dfSSPgrades, function(x) { gsub("-", 0, x) }))
dfSSPgradesStat <- data.frame( lapply(dfSSPgrades[11:121], function(x) as.numeric(as.character(x))) )

avgGrades <- NULL
avgGrades['Understanding of Medialogy'] <- list(rowMeans(dfSSPgradesStat[c(98:106)])) # removed 5
avgGrades['Study and work'] <- list(rowMeans(dfSSPgradesStat[93:97]))
avgGrades['Growth mindset'] <- list(rowMeans(dfSSPgradesStat[62:64]))
avgGrades['Grit'] <- list(rowMeans(dfSSPgradesStat[57:61]))
avgGrades['Study habits'] <- list(rowMeans(dfSSPgradesStat[65:68]))
avgGrades['High school habits'] <- list(rowMeans(dfSSPgradesStat[34:45]))
avgGrades['Social support for studying'] <- list(rowMeans(dfSSPgradesStat[c(2:4,7:8,55:56)])) # removed 4, added 2
avgGrades <- data.frame( lapply(avgGrades, function(x) as.numeric(as.character(x))) )


# normalizes avg grades
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
norm.avgGrades <- as.data.frame(lapply(avgGrades, normalize))
norm.avgGrades["Campus"] <- dfSSPgrades$Campus
norm.avgGrades["rowID"] <- seq(1:nrow(norm.avgGrades))

####################################
# Students with averages above 1 are most likely to continue Medialogy while
# students with averages below -1 are most likely to dropout.
scaled.avgGrades <- avgGrades
scaled.avgGrades[1:14] <- scale(avgGrades[1:14])

# checks that we get mean of 0 and sd of 1
colMeans(scaled.avgGrades)
apply(scaled.avgGrades, 2, sd)

scaled.avgGrades["Campus"] <- dfSSPgrades$Campus
scaled.avgGrades["rowID"] <- seq(1:nrow(scaled.avgGrades))

####################################

ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_density()+
  scale_x_discrete(breaks=seq(7,11,1), name ="Total score")

# OLD: gradesum calculated for all SSP topics, high risk student 
#dfSSPgradesSum <- data.frame(rowID = scaled.avgGrades$rowID, campus = scaled.avgGrades$Campus, gradeSums = rowSums(dfSSPgradesStat))
#dfSSPgradesSum <- data.frame(rowID = norm.avgGrades$rowID, campus = norm.avgGrades$Campus, gradeSums = rowSums(dfSSPgradesStat))
#ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_histogram()

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

# Preparing the dataset to the mailmerge in rmarkdown

studentData <- sqldf('Select rowID,Campus from dfSSPgrades')
studentData['name'] <- paste(dfSSPgrades[,2],dfSSPgrades[,1])
studentData['email'] <- dfSSPgrades[,5]
avgPer <- data.frame(norm.avgGrades, apply(norm.avgGrades[, 7:1], 2, function(c) ecdf(c)(c))*100, dfSSPanswers[, c(67,104,106:107,109:116)])
studentData <- merge(studentData, avgPer, by= c("rowID","Campus"))
studentData$rowID <- as.numeric(levels(studentData$rowID))[studentData$rowID]
studentData <- studentData[with(studentData, order(studentData$rowID)),]

setwd('C:/Users/BiancaClavio/Documents/SVN/01Projects/SSP')
write.csv(studentData,file = "studentData.csv")
write.csv(highRiskStudents,file = "highRiskStudents.csv")
personalized_info <- read.csv(file = "studentData.csv")
highRiskStudents <- read.csv(file = "highRiskStudents.csv")

# The for loop renders the student report pdf files
for (i in 1:nrow(personalized_info)){
  rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/stats-on-grades/docs/SSP-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(" ", "", paste(ifelse(personalized_info$Campus[i] == 'AAL', 
                                               "SSP-AAL’17:IndividualStudentFeedback_", 
                                               "SSP-CPH’17:IndividualStudentFeedback_"),
                                        personalized_info$name[i], ".pdf", sep='')),
                    output_dir = "handouts/")
}

############################################################################


# Sends mails with SSP student reports:

library(sendmailR)
library(RDCOMClient)

for (i in 1:nrow(personalized_info)){
  subject <- ifelse(personalized_info$Campus[i] == 'AAL', "SSP-AAL’17:IndividualStudentFeedback_", "SSP-CPH’17:IndividualStudentFeedback_")
  attachmentPath <- gsub(" ", "", paste('C:/Users/BiancaClavio/Documents/stats-on-grades/docs/handouts/', subject, personalized_info$name[i],'.pdf'))
  name <- personalized_info$name[i]
  coordinator <- ifelse(personalized_info$Campus[i] == 'AAL', "Hendrik (hk@create.aau.dk)", "Jon (jpe@create.aau.dk)")
  mailBody <- paste("Dear",name, "
  
  In September, you participated in the Study Verification Test (SSP), in which you answered a series of questions on Moodle. We collected this information to better understand your hopes, expectations, and worries about student life at AAU in general and about Medialogy in particular. We use the information to improve your study environment and to reach out and provide individual support to those of you who seek or need it to adjust to university life and master your chosen programme.
  We have analysed your responses in your respective cohort; more specifically within the first semester students in Aalborg and Copenhagen in 2017. One major outcome of this effort is the attached student report. It provides personalized feedback on important factors for finishing a university degree as well as specific recommendations and links to AAU resources that can be of help.
  Currently only the semester coordinator, supervisors, and the study board have access to this information. But you are welcome to share your student reports with peers, study counsellors, teachers, or others - should you desire to do so. Depending on individual results and needs we might approach you again in the future.
  
  Please contact",coordinator,"via email and add Bianca in CC (bcch@create.aau.dk) if you have any questions.
  
  Best regards,
  Jon, Bianca, and Hendrik
  
  ")
  ## init com api
  OutApp <- COMCreate("Outlook.Application")
  ## create an email
  outMail = OutApp$CreateItem(0)
  ## configure  email parameter
  outMail[["To"]] = as.character(personalized_info$email[i])
  outMail[["Cc"]] = ifelse(personalized_info$Campus[i] == 'AAL', "hk@create.aau.dk", "jpe@create.aau.dk")
  outMail[["subject"]] = 
    outMail[["body"]] = mailBody
  outMail[["Attachments"]]$Add(attachmentPath)
  
  ####### Uncomment below to send mails:
  ##### outMail$Send()
}

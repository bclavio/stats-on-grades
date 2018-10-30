## Packages
library(knitr)
library(rmarkdown)
library(xlsx)
library(openxlsx)
library(sqldf)
library(plyr)
library(dplyr)
library(reshape2)
library(MASS)
library(manipulate)
library(stringr)
library(Hmisc)
library(ggplot2)
library(reshape2)
#detach("package:RMySQL", unload=TRUE)



#######################################################################################################
### Instructions for SSP download (on Moodle)

# 1) Go to results in the SSP quiz in order to download both grades and answers submissions
# 2) When downloading choose lastest and finished attempts



#######################################################################################################
### Import SSP data

# setting the working directory to the fileshare  
setwd('Z:/BNC/PBL development project/data/analysis_data/SSP/2019/')

# SSP answers import
dfSSPanswersCPH<-read.csv("SSPanswersCPHTest.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPanswersAAL<-read.csv("SSPanswersAALTest.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPanswersCPH["Campus"]<-"CPH"
dfSSPanswersAAL["Campus"]<-"AAL"
dfSSPanswers <- rbind(dfSSPanswersCPH,dfSSPanswersAAL) 
names(dfSSPanswers)[1] <- "Surname"

# SSP scores import
dfSSPgradesCPH<-read.csv("SSPgradesAALTest.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesAAL<-read.csv("SSPgradesCPHTest.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesCPH["Campus"]<-"CPH"
dfSSPgradesAAL["Campus"]<-"AAL"
dfSSPgrades <- rbind(dfSSPgradesCPH,dfSSPgradesAAL) 
names(dfSSPgrades)[1] <- "Surname"

#dfSSPgrades["rowID"] <- seq(1:nrow(dfSSPgrades))


#######################################################################################################
### Data cleaning: study hours

# creates extra column
dfSSPanswers['studyHoursNum'] <- dfSSPanswers$`Response 113`

# replaces strings containing "-" with -5 (e.g. dates)
dfSSPanswers$studyHoursNum <- ifelse(grepl("-", dfSSPanswers$studyHoursNum), "-5", dfSSPanswers$studyHoursNum)

# removes non-numeric characters, e.g. "hours"                                      
dfSSPanswers$studyHoursNum <- gsub("[^0-9\\.]", "", dfSSPanswers$studyHoursNum)

# replaces empty strings wth -5 
dfSSPanswers$studyHoursNum[nchar(dfSSPanswers$studyHoursNum)==0] <- "-5"

# changes the scores based on the new study hours column 
dfSSPgrades$`Q. 113 /0.75`[findInterval(dfSSPanswers$studyHoursNum, c(-5,29)) == 1L] <- 0
dfSSPgrades$`Q. 113 /0.75`[findInterval(dfSSPanswers$studyHoursNum, c(30,37)) == 1L] <- 0.25
dfSSPgrades$`Q. 113 /0.75`[findInterval(dfSSPanswers$studyHoursNum, c(38,41)) == 1L] <- 0.50
dfSSPgrades$`Q. 113 /0.75`[findInterval(dfSSPanswers$studyHoursNum, c(42,1000)) == 1L] <- 0.75


###############################################
# counts study hours for the campi (only used for p1 semester start)
# studyHoursTable <- data.frame(
#     c(
#       "0-29" = nrow(dfSSPgrades[dfSSPgrades$`Q. 113 /0.75`== 0.0 & dfSSPgrades$Campus == "AAL", ]),
#       "30-37" = nrow(dfSSPgrades[dfSSPgrades$`Q. 113 /0.75` == 0.25 & dfSSPgrades$Campus == "AAL", ]),
#       "38-41" = nrow(dfSSPgrades[dfSSPgrades$`Q. 113 /0.75` == 0.50 & dfSSPgrades$Campus == "AAL", ]),
#       "42+" = nrow(dfSSPgrades[dfSSPgrades$`Q. 113 /0.75` == 0.75 & dfSSPgrades$Campus == "AAL", ]),
#     "Total" = nrow(dfSSPgrades[dfSSPgrades$Campus == "AAL", ])))
#names(studyHoursTable)[1]<-"AAL"
# names(studyHoursTable)[2]<-"CPH"
# names(dfSSPgrades)[1]<-"Surname"




##############################################################################################
# Computes topic averages for each student

# dont remove comment columns, but set them to zero (will skip them in the next step with avgGrades)
dfSSPgrades[, c(16,24,31,41,50,54,67,72,78,84,88,99,113,122,126,135,142,143)] <- 0
dfSSPgradesStat <- data.frame( lapply(dfSSPgrades[11:143], function(x) as.numeric(as.character(x))) )

# manually selected SSP items into new categories
avgGrades <- NULL
avgGrades['Understanding of Medialogy'] <- list(rowMeans(dfSSPgradesStat[117:124]))
avgGrades['Study and work'] <- list(rowMeans(dfSSPgradesStat[113:115]))
avgGrades['Growth mindset'] <- list(rowMeans(dfSSPgradesStat[75:77]))
avgGrades['Grit'] <- list(rowMeans(dfSSPgradesStat[69:73]))
avgGrades['Study habits'] <- list(rowMeans(dfSSPgradesStat[79:82]))
avgGrades['High school habits'] <- list(rowMeans(dfSSPgradesStat[45:56])) 
avgGrades['Social support for studying'] <- list(rowMeans(dfSSPgradesStat[c(7:9,12:13,67)]))
avgGrades <- data.frame( lapply(avgGrades, function(x) as.numeric(as.character(x))) )

# normalizes avg scores based on the maximum possible score 
normalize <- function(x) {
  return ((x) / (0.75))
}

norm.avgGrades <- as.data.frame(lapply(avgGrades, normalize))
norm.avgGrades["Campus"] <- dfSSPgrades$Campus
norm.avgGrades["rowID"] <- seq(1:nrow(norm.avgGrades))
norm.avgGrades["Email"] <- dfSSPgrades$Email.address


##############################################################################################
## Graphs

# density distribution of total SSP score
# ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_density()+
#   scale_x_discrete(breaks=seq(7,11,1), name ="Total score")

# OLD: gradesum calculated for all SSP topics, high risk student 
dfSSPgradesSum <- data.frame(rowID = scaled.avgGrades$rowID, campus = scaled.avgGrades$Campus, gradeSums = rowSums(dfSSPgradesStat))
dfSSPgradesSum <- data.frame(rowID = norm.avgGrades$rowID, campus = norm.avgGrades$Campus, gradeSums = rowSums(dfSSPgradesStat))
#ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_histogram()

# histogram of total scores
selectedTopics <- dfSSPgradesStat[,c(7:9,12:13,67,45:56,79:82,69:73,75:77,113:115,117:124)]
dfSSPgradesSum <- data.frame(rowID = norm.avgGrades$rowID, Campus = norm.avgGrades$Campus, email = norm.avgGrades$Email, gradeSums = rowSums(selectedTopics))
ggplot(dfSSPgradesStat, aes(rowSums(selectedTopics))) + geom_histogram()

#######################################################################
# high risk students
highRiskStudents <- data.frame(gradeSums= dfSSPgradesSum$gradeSums[order(dfSSPgradesSum$gradeSums)[1:20]])
highRiskStudents<- data.frame(dfSSPgradesSum[dfSSPgradesSum$gradeSums %in% highRiskStudents$gradeSums,])

# use in CPH script
norm.avgGradesAAL <- norm.avgGrades
norm.avgGradesMeltAAL <- NULL
norm.avgGradesMeltAAL <- melt(norm.avgGradesAAL[,-10], id.vars = c("rowID", "Campus"))
norm.avgGradesMeltAAL$variable <- gsub("\\.", " ", norm.avgGradesMeltAAL$variable)
norm.avgGradesMeltAAL['highRisk'] <- ifelse(norm.avgGradesMeltAAL$rowID %in% highRiskStudents$rowID, 1, 0)
norm.avgGradesMeltAAL$variable <- factor(norm.avgGradesMeltAAL$variable, levels = c('Understanding of Medialogy', 'Study and work', 
                                                                              'Growth mindset','Grit','Study habits',
                                                                              'High school habits','Social support for studying'),ordered = TRUE)

dfSSPanswers["rowID"] <- seq(1:nrow(dfSSPanswers))
studyHoursAAL <- dfSSPanswers [, c('rowID', 'Campus', 'Response 113')]
studyHoursAAL['highRisk'] <- ifelse(studyHoursAAL$rowID %in% highRiskStudents$rowID, 1, 0)
names(studyHoursAAL)[3]<-"hours"

#######################################################################

dfSSPanswersNA<-read.csv("SSPanswersTestAALNA.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# Preparing the dataset to the mailmerge in rmarkdown
answersAAL <- data.frame(dfSSPanswers[, c(78,124)], dfSSPanswersNA[,124])
names(answersAAL) <- c("social", "studyHours", "studyHoursNA")

studentData <- sqldf('Select rowID, Campus from dfSSPgrades')
studentData['name'] <- paste(dfSSPgrades[,2],dfSSPgrades[,1])
studentData['email'] <- dfSSPgrades[,5]
studentData['initials'] <-  gsub("@student.aau.dk", "",dfSSPgrades[,5])
#avgPer <- data.frame(norm.avgGrades, apply(norm.avgGrades[, 7:1], 2, function(c) ecdf(c)(c))*100, answers)
# 67-11: since going to uni.. friends -> social integration easy: 67+11
#104-11: study hours -> 113+11
#106-11: study activities + non related -> null and 114+11
#109-11: 3D modelling to 116-11: programming -> 117:123 + 11

#studentData <- merge(studentData, avgPer, by= c("rowID","Campus"))
#studentData$rowID <- as.numeric(levels(studentData$rowID))[studentData$rowID]
studentData <- studentData[with(studentData, order(studentData$rowID)),]
studentDataAAL <- studentData

write.csv(studentData,file = "studentDataAAL.csv")
write.csv(highRiskStudents,file = "highRiskStudentsAAL.csv")
#personalized_info <- read.csv(file = "studentDataAAL.csv")
#highRiskStudents <- read.csv(file = "highRiskStudentsAAL.csv")

#######################################################################

# # Boxplots are the best data representation for understanding the dataset, 
# # but the students might gain more from a simpler (and gamified) graph, 
# # such as the radar/spider web chart with their scores in comparison to the average/median student
# # and the percentile rank for each topic.
# 
# #create a vector with axis names
# labs <- c("Campus","name","Understanding of\n Medialogy", "Time com-\n mitment", "Growth\n mindset", "Grit", "Study habits\n at AAU", "High school\n habits", "Social support\n for studying")
# #use the new vector to change the column names
# #colnames(dfStudentMedian)<- labs
# 
# # import percentiles
# dfSPPscore <- read.csv("studentData.csv", header = T)
# # calculate the median of the scores times 100 to be on the same scale as percentiles, not the percentiles
# SSPmedian <- data.frame(Campus="AAL/CPH",name="Median",t(colMedians(dfSPPscore[,7:13])))
# colnames(SSPmedian)<- labs
# dfSPPscore <- dfSPPscore[,c(3:4,7:13)]
# colnames(dfSPPscore)<- labs
# # median as the last row in the dataset
# dfSPPscoreAddon <- rbind(dfSPPscore, SSPmedian)
# 
# # function to create the coordinates for the radarplot and remove outer line
# coord_radar <- function (theta = "x", start = 0, direction = 1) 
# {
#   theta <- match.arg(theta, c("x", "y"))
#   r <- if (theta == "x")
#     "y"
#   else "x"
#   
#   #dirty
#   rename_data <- function(coord, data) {
#     if (coord$theta == "y") {
#       plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
#     } else {
#       plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
#     }
#   }
#   theta_rescale <- function(coord, x, scale_details) {
#     rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
#     rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
#   }
#   
#   r_rescale <- function(coord, x, scale_details) {
#     scales::rescale(x, c(0, 0.4), scale_details$r.range)
#   }
#   
#   ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
#           direction = sign(direction),
#           is_linear = function(coord) TRUE,
#           render_bg = function(self, scale_details, theme) {
#             scale_details <- rename_data(self, scale_details)
#             
#             theta <- if (length(scale_details$theta.major) > 0)
#               theta_rescale(self, scale_details$theta.major, scale_details)
#             thetamin <- if (length(scale_details$theta.minor) > 0)
#               theta_rescale(self, scale_details$theta.minor, scale_details)
#             thetafine <- seq(0, 2 * pi, length.out = 100)
#             
#             rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
#             
#             # This gets the proper theme element for theta and r grid lines:
#             #   panel.grid.major.x or .y
#             majortheta <- paste("panel.grid.major.", self$theta, sep = "")
#             minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
#             majorr     <- paste("panel.grid.major.", self$r,     sep = "")
#             
#             ggplot2:::ggname("grill", grid::grobTree(
#               ggplot2:::element_render(theme, "panel.background"),
#               if (length(theta) > 0) ggplot2:::element_render(
#                 theme, majortheta, name = "angle",
#                 x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
#                 y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
#                 id.lengths = rep(2, length(theta)),
#                 default.units = "native"
#               ),
#               if (length(thetamin) > 0) ggplot2:::element_render(
#                 theme, minortheta, name = "angle",
#                 x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
#                 y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
#                 id.lengths = rep(2, length(thetamin)),
#                 default.units = "native"
#               ),
#               
#               ggplot2:::element_render(
#                 theme, majorr, name = "radius",
#                 x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
#                 y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
#                 id.lengths = rep(length(thetafine), length(rfine)),
#                 default.units = "native"
#               )
#             ))
#           })
# }
# #define plot theme
# RadarTheme<-theme(panel.background=element_blank(),
#                   plot.title= element_text(size = 25,face=c("bold", "italic")),
#                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
#                   text=element_text(family="serif"), aspect.ratio = 1,
#                   #legend.position="bottom",legend.title=element_blank(),
#                   #legend.direction="horizontal", legend.text = element_text(size = 20),
#                   strip.text.x = element_text(size = rel(0.8)),
#                   axis.text.x = element_text(size = 20),
#                   axis.ticks.y = element_blank(),
#                   axis.text.y = element_blank(),
#                   #axis.line.x=element_line(size=0.5),
#                   panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"),
#                   #legend.key=element_rect(fill=NA),
#                   line = element_blank(),
#                   title = element_blank(),
#                   legend.position="none") 
#                   #text=element_text(size=15, family="Arial")) # changed font
#  
# #define plot theme
# # RadarTheme<-theme(panel.background=element_blank(),
# #                   plot.title= element_text(size = 25,face=c("bold", "italic")),
# #                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
# #                   text=element_text(size=15, family="Arial"), 
# #                   aspect.ratio = 1,
# #                   legend.position="bottom",legend.title=element_blank(),
# #                   legend.direction="horizontal", legend.text = element_text(size = 15),
# #                   strip.text.x = element_text(size = rel(0.8)),
# #                   #strip.text.x = element_text(size=15, face = "bold"),
# #                   axis.text.x = element_text(size = 15, face = "bold"),
# #                   axis.ticks.y = element_blank(),
# #                   axis.text.y = element_blank(),
# #                   axis.line.x=element_line(size=0.5),
# #                   panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"))
                  

#######################################################################

# The for loop renders the student report pdf files
# note: sometimes the loop can't run all students > check output folder or run from last completed student file 
for (i in 1:nrow(personalized_info)){
  rmarkdown::render(input = "C:/Users/BiancaClavio/Documents/PBLstats-on-grades/docs/SSP/SSP-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = gsub(" ", "", paste("SSP_", ifelse(personalized_info$Campus[i] == 'AAL',
                                                     "AAL_Individual-student-feedback_",
                                                     "CPH_Individual-student-feedback_"),
                                                      personalized_info$initials[i], ".pdf", sep='')),
                    output_dir = "handouts2018")
}

############################################################################


# Sends mails with SSP student reports:

# library(sendmailR)
# library(RDCOMClient)

# for (i in 1:nrow(personalized_info)){
#   subject <- ifelse(personalized_info$Campus[i] == 'AAL', "SSP_AAL_Individual-student-feedback_", "SSP_AAL_Individual-student-feedback_")
#   attachmentPath <- gsub(" ", "", paste('C:/Users/BiancaClavio/Documents/stats-on-grades/docs/handouts/', subject, personalized_info$initial[i],'.pdf'))
#   name <- personalized_info$name[i]
#   coordinator <- ifelse(personalized_info$Campus[i] == 'AAL', "Hendrik (hk@create.aau.dk)", "Jon (jpe@create.aau.dk)")
#   mailBody <- paste("Dear",name, "
# 
# In September, you participated in the Study Verification Test (SSP), in which you answered a series of questions on Moodle. We collected this information to better understand your hopes, expectations, and worries about student life at AAU in general and about Medialogy in particular. We use the information to improve your study environment and to reach out and provide individual support to those of you who seek or need it to adjust to university life and master your chosen programme.
# We have analysed your responses in your respective cohort; more specifically within the first semester students in Aalborg and Copenhagen in 2017. One major outcome of this effort is the attached student report. It provides personalized feedback on important factors for finishing a university degree as well as specific recommendations and links to AAU resources that can be of help.
# Currently only the semester coordinator, supervisors, and the study board have access to this information. But you are welcome to share your student reports with peers, study counsellors, teachers, or others - should you desire to do so. Depending on individual results and needs we might approach you again in the future.
# 
# Please contact",coordinator,"via email and add Bianca in CC (bcch@create.aau.dk) if you have any questions.
# 
# Best regards,
# Jon, Bianca, and Hendrik
# 
#   ")
#   ## init com api
#   OutApp <- COMCreate("Outlook.Application")
#   ## create an email
#   outMail = OutApp$CreateItem(0)
#   ## configure  email parameter
#   outMail[["To"]] = as.character(personalized_info$email[i])
#   outMail[["Cc"]] = ifelse(personalized_info$Campus[i] == 'AAL', "hk@create.aau.dk", "jpe@create.aau.dk")
#   outMail[["subject"]] = subject
#     outMail[["body"]] = mailBody
#   outMail[["Attachments"]]$Add(attachmentPath)
#   
#   ####### Uncomment below to send mails:
#   ##### outMail$Send()
# }






######################### IMPORT allGPRO quizzes 

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/GPRO/'} else {"~/SVN/01Projects/GPRO/"}
setwd(SVNData)

# Note: I haven't imported the Midterm exam responses, but they are in SVN

# imports and merges the midterm exam grades from AAL and CPH
dfMidtermGradesCPH<-read.csv("MidtermExamCPH-grades_16-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfMidtermGradesAAL<-read.csv("MidtermExamAAL-grades_16-11.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
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
#    dfGPROquizzesAAL<-read.csv("GPRO-gradebook-quiz.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

# merges GPRO Midterm exam and SSP (not self-assessments)


dfMidtermGradesSelect <- dfMidtermGrades[, c(3,8,33)]
names(dfMidtermGradesSelect)[1]<-"email"
dfMED1data <- NULL
dfMED1data <- merge(dfSSPgradesSum, dfMidtermGradesSelect, by= c("email", "Campus"))
# Note: some data/students disappears in this merge: re-exams in GPRO, MED1 dropouts, merits to the GPRO exam
dfMED1data$`Grade/100.00` <- dfMED1data$`Grade/100.00`/10
dfMED1data['score'] <- rowSums(dfMED1data[4:5])

ggplot(dfMED1data, aes(score)) + geom_density()+
  scale_x_discrete(breaks=seq(0,15,1), name ="Total score", limits = c(0,15))

highRiskStudentsGPRO <- data.frame(score= dfMED1data$score[order(dfMED1data$score)[1:20]])
highRiskStudentsGPRO <- data.frame(dfMED1data[dfMED1data$score %in% highRiskStudentsGPRO$score,])
highRiskStudentsGPRO <- merge(highRiskStudentsGPRO, personalized_info[4:5], by="email")

highRiskStudentsGPROAAL <- subset(highRiskStudentsGPRO, highRiskStudentsGPRO$Campus =="AAL")
highRiskStudentsGPROCPH <- subset(highRiskStudentsGPRO, highRiskStudentsGPRO$Campus =="CPH")

setwd('C:/Users/BiancaClavio/Documents/stats-on-grades/')
write.csv(dfMED1data,file = "DropoutMED1.csv")


############################################################################


# Sends mails to highrisk students (highRiskStudentsGPRO):

# library(sendmailR)
# library(RDCOMClient)
# 
# for (i in 1:nrow(highRiskStudentsGPROAAL)){
#   subject <- "Invitation to talk about your study experience"
#   name <- highRiskStudentsGPROAAL$name[i] 
#   mailBody <- paste("Dear",name, "
# 
# We would like to meet with you on Monday 11th December and talk about your study experience at Medialogy. We want to improve the Medialogy education so that students will have an easier time learning. The students best qualified to help us are those who find the content more difficult than other students. According to your mid-term exams you might be able to help us, and we would like to know from you about your experience so far and how to better help students master the challenges encountered during the first year of study.
# It would help us tremendously if you filled in the questionnaire (https://www.moodle.aau.dk/mod/quiz/view.php?id=697398) to give us some initial indication of how helpful you found the individual (SSP) feedback. 
# We hope you can spare some time on Monday to meet with us and give us some feedback. It should both benefit you in the coming semester and future student generations. 
# 
# Please contact us via email if you want to book a specific timeslot for the meeting on Monday, or if you have any questions.
# 
# 
# Best regards,
# Hendrik and Bianca
# 
#   ")
#   ## init com api
#   OutApp <- COMCreate("Outlook.Application")
#   ## create an email
#   outMail = OutApp$CreateItem(0)
#   ## configure  email parameter
#   outMail[["To"]] = as.character(highRiskStudentsGPROAAL$email[i])
#   outMail[["Cc"]] = "hk@create.aau.dk"
#   outMail[["subject"]] = subject
#   outMail[["body"]] = mailBody
# 
#   ####### Uncomment below to send mails:
#   ###outMail$Send()
# }




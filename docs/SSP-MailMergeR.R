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



dfSSPgrades <- data.frame(lapply(dfSSPgrades, function(x) { gsub("-", 0, x) }))
dfSSPgradesStat <- data.frame( lapply(dfSSPgrades[11:121], function(x) as.numeric(as.character(x))) )

# computes avg grade for each category and for each student
avgGrades <- NULL
# avgGrades['View on Medialogy'] <- list(rowMeans(dfSSPgradesStat[98:111]))
# avgGrades['Study and Work'] <- list(rowMeans(dfSSPgradesStat[c(94:97)]))
# avgGrades['Academic Abilities'] <- list(rowMeans(dfSSPgradesStat[88:92]))
# avgGrades['Personal Trait'] <- list(rowMeans(dfSSPgradesStat[75:87]))
# avgGrades['Self-control'] <- list(rowMeans(dfSSPgradesStat[65:74]))
# avgGrades['Growth Mindset'] <- list(rowMeans(dfSSPgradesStat[62:64]))
# avgGrades['Grit'] <- list(rowMeans(dfSSPgradesStat[57:61]))
# avgGrades['Belonging Uncertainty'] <- list(rowMeans(dfSSPgradesStat[51:56]))
# avgGrades['High School Trust'] <- list(rowMeans(dfSSPgradesStat[46:50]))
# avgGrades['High School Behaviour'] <- list(rowMeans(dfSSPgradesStat[34:45]))
# avgGrades['Education Choice'] <- list(rowMeans(dfSSPgradesStat[24:33]))
# avgGrades['Going to University'] <- list(rowMeans(dfSSPgradesStat[16:23]))
# avgGrades['Education Attitude'] <- list(rowMeans(dfSSPgradesStat[10:15]))
# avgGrades['Demographics'] <- list(rowMeans(dfSSPgradesStat[1:9]))

avgGrades['Understanding of Medialogy'] <- list(rowMeans(dfSSPgradesStat[c(98:106)])) # removed 5
avgGrades['Study and Work'] <- list(rowMeans(dfSSPgradesStat[93:97]))
avgGrades['Growth Mindset'] <- list(rowMeans(dfSSPgradesStat[62:64]))
avgGrades['Grit'] <- list(rowMeans(dfSSPgradesStat[57:61]))
avgGrades['Study habits'] <- list(rowMeans(dfSSPgradesStat[65:68]))
avgGrades['High School Habits'] <- list(rowMeans(dfSSPgradesStat[34:45]))
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
norm.avgGradesMelt <- melt(norm.avgGrades, id.vars = c("rowID", "Campus"))
norm.avgGradesMelt['highRisk'] <- ifelse(norm.avgGradesMelt$rowID %in% highRiskStudents$rowID, 1, 0)

dfSSPanswers["rowID"] <- seq(1:nrow(dfSSPanswers))
studyHours <- dfSSPanswers [, c('rowID', 'Campus', 'Response 93')]
studyHours['highRisk'] <- ifelse(studyHours$rowID %in% highRiskStudents$rowID, 1, 0)
names(studyHours)[3]<-"hours"







# The histogram for each category shows that the scaled averages are not normal distributed:
# ggplot(scaled.avgGradesMelt,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram(binwidth = 0.1)

ggplot(norm.avgGradesMelt,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram(binwidth = 0.1)


# sumGrades <- NULL
# sumGrades['avgDemographics'] <- list(rowSums(dfSSPgradesStat[1:9]))
# sumGrades['avgAttitude'] <- list(rowSums(dfSSPgradesStat[10:15]))
# sumGrades['avgReasons'] <- list(rowSums(dfSSPgradesStat[16:23]))
# sumGrades['avgChoice'] <- list(rowSums(dfSSPgradesStat[24:33]))
# sumGrades['avgHSBehave'] <- list(rowSums(dfSSPgradesStat[34:45]))
# sumGrades['avgHSTrust'] <- list(rowSums(dfSSPgradesStat[46:50]))
# sumGrades['avgBelonging'] <- list(rowSums(dfSSPgradesStat[51:56]))
# sumGrades['avgGrit'] <- list(rowSums(dfSSPgradesStat[57:61]))
# sumGrades['avgGrowth'] <- list(rowSums(dfSSPgradesStat[62:64]))
# sumGrades['avgControl'] <- list(rowSums(dfSSPgradesStat[65:74]))
# sumGrades['avgTraits'] <- list(rowSums(dfSSPgradesStat[75:87]))
# sumGrades['avgAcademic'] <- list(rowSums(dfSSPgradesStat[88:92]))
# sumGrades['avgHours'] <- list(rowSums(dfSSPgradesStat[c(94,97)])) # I have removed 3 questions
# sumGrades['avgMedialogy'] <- list(rowSums(dfSSPgradesStat[98:111]))
# sumGrades <- data.frame( lapply(sumGrades, function(x) as.numeric(as.character(x))) )
# 
# scaled.sumGrades <- sumGrades
# scaled.sumGrades[1:14] <- scale(sumGrades[1:14])
# scaled.sumGrades["Campus"] <- dfSSPgrades$Campus
# scaled.sumGrades["rowID"] <- seq(1:nrow(scaled.sumGrades))
# scaled.sumGradesMelt <- melt(scaled.sumGrades, id.vars = c("rowID", "Campus"))
# scaled.sumGradesMelt['highRisk'] <- ifelse(scaled.sumGradesMelt$rowID %in% highRiskStudents$rowID, 1, 0)
# 
# # The histogram for each category shows that the scaled averages are not normal distributed (the same as the above):
# ggplot(scaled.sumGradesMelt,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram(binwidth = 0.1)




# scaled data with facet of AAL and CPH common for all students
ggplot(data= melt(scaled.avgGrades[1:15], id.var="Campus"), aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Campus)) +
  scale_y_continuous(breaks=seq(-4,4,1)) +
  coord_flip() +
  theme_bw() + 
  facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        #panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
        )

# Campi side-by-side
ggplot(data= melt(scaled.avgGrades[1:15], id.var="Campus"), aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Campus)) +
  scale_y_continuous(breaks=seq(-4,4,1)) +
  labs(title = "Scaled averages of student scores",y = "Standard deviations from the mean") +
  coord_flip() +
  theme_bw() + 
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.title.x=element_blank(),
        #panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #legend.position="none"
  )


# all high risk students with color diff - dots on top of each other
ggplot(norm.avgGradesMelt, aes(x=variable, y=value)) + 
  geom_boxplot() + 
  coord_flip() +
  #facet_grid(Campus ~ .) +
  geom_dotplot(data = subset(norm.avgGradesMelt, norm.avgGradesMelt$highRisk ==1), 
               aes(fill = factor(rowID)), 
               binaxis='y',
               stackdir='center',
               #position = "jitter",
               #alpha = 0.5,
               binwidth = 0.025) +
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )

# all high risk students
ggplot(scaled.avgGradesMelt, aes(x=variable, y=value)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks=seq(-4,4,1)) +
  coord_flip() +
  #facet_grid(Campus ~ .) +
  geom_dotplot(data = subset(scaled.avgGradesMelt, scaled.avgGradesMelt$highRisk ==1), 
               aes(fill = factor(rowID==1)), 
               binaxis='y',
               stackdir='center',
               #position = position_dodge(width = 1),
               binwidth = 0.1) +
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")

# individual high risk student (used in the markdown file)
# ggplot(scaled.avgGradesMelt, aes(x=variable, y=value)) +
#   geom_boxplot() +
#   coord_flip() +
#   geom_dotplot(data = subset(scaled.avgGradesMelt, scaled.avgGradesMelt$highRisk ==1),
#                aes(fill = rowID %in% highRiskStudents$rowID[i],
#                  #fill = ifelse(rowID == highRiskStudents$rowID[i],"blue", "white"),
#                    #color = ifelse(rowID == highRiskStudents$rowID[i], "blue", "white"),
#                    alpha = ifelse(rowID %in% highRiskStudents$rowID[i], 1, 0)),
#                binaxis='y',
#                stackdir='center',
#                binwidth = 0.11) +
#   theme_bw() +
#   #facet_grid(Campus ~ .) +
#   theme(axis.title.y=element_blank(),
#         #axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.title.x=element_blank(),
#         #panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position="none")

i <- 1 # from 1-20
student <- subset(norm.avgGradesMelt, norm.avgGradesMelt$rowID %in% highRiskStudents$rowID[i])
ggplot(norm.avgGradesMelt, aes(x=variable, y=value)) +
  geom_boxplot() + 
  coord_flip() +
  geom_dotplot(data = student,
               aes(x=variable, y=value, fill = "green"),
               binaxis='y',
               stackdir='center',
               binwidth = 0.02) +
  theme_bw() +
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")



# i <- 1 # from 1-20
# # study hours boxplot
# 
# studentHours <- subset(studyHours, studyHours$rowID %in% highRiskStudents$rowID[i])
# studentHours['DotPos'] <- 95 
# ggplot(studyHours, aes(x=rowID,y=hours)) + 
#   geom_boxplot() + 
#   coord_flip() +
#   geom_dotplot(data = studentHours,
#                aes(x=DotPos, y=hours, fill = "green"),
#                binaxis='y',
#                stackdir='center',
#                #position = "dodge",
#                #binpositions="all",
#                binwidth = 2.0) +
#   theme_bw() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         #axis.title.x=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position="none")




#######################################################################

studentData <- sqldf('Select rowID,Campus from dfSSPgrades')
studentData['FirstName'] <- dfSSPgrades[,2]
studentData['SurName'] <- dfSSPgrades[,1]

avgPer <- data.frame(norm.avgGrades, apply(norm.avgGrades[, 7:1], 2, function(c) ecdf(c)(c))*100, dfSSPanswers[, c(67,104,106:107,109:116)])

#studentData1 <- merge(studentData, norm.avgGradesMelt, by= "rowID")
#studentData1 <- studentData1[order(studentData1$variable), ]
studentData <- merge(studentData, avgPer, by= c("rowID","Campus"))
studentData <- subset(studentData, studentData$rowID %in% highRiskStudents$rowID)
studentData$rowID <- as.numeric(levels(studentData$rowID))[studentData$rowID]
studentData <- studentData[with(studentData, order(studentData$rowID)),]

setwd('C:/Users/BiancaClavio/Documents/stats-on-grades/docs')
write.csv(studentData,file = "studentData.csv")
#write.csv(studentData1,file = "studentData1.csv")
write.csv(highRiskStudents,file = "highRiskStudents.csv")
personalized_info <- read.csv(file = "studentData.csv")
#score_info <- read.csv(file = "studentData1.csv")
highRiskStudents <- read.csv(file = "highRiskStudents.csv")

## Loop
for (i in 1:nrow(personalized_info)){
  rmarkdown::render(input = "SSP-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = paste("SSPanalysis_", i, ".pdf", sep=''),
                    output_dir = "handouts/")
}

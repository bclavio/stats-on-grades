
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

dfGPROquestionnnaire <- read.csv("GPRO/GPRO-comparisionQuestionnaire.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE,na.strings=c("","NA"))
names(dfGPROquestionnnaire) <- c("timestamp","study","priorExp","remember","books","slides","online-documentation",
                                 "web-search","Moodle-videos","online-videos","Peergrade.io","other-tools",
                                 "specified-tools","lecture-exercises","lecture-handin","programming-pet",
                                 "semester-project","KhanAcademy","Moodle-SA","ask-teacher","ask-others",
                                 "collaborate","other-activities","specified-activities","confidence-ex",
                                 "confident-concepts","need-help","specified-help","comments")

dfGPROquestionnnaireHelp <- subset(dfGPROquestionnnaire,grepl("Yes", dfGPROquestionnnaire$`need-help`))

length(grep("Yes", dfGPROquestionnnaire$`need-help`)) # 51
length(grep("Copenhagen", dfGPROquestionnnaireHelp$study)) # 25, AAL+PDP=26
sum(is.na(dfGPROquestionnnaireHelp$`specified-help`)) # 5
# specified-help - concrete (excluded general answers, e.g. "I need help with everything") 
# CPH: Easier optional exercises,  solving lecture exercises, understanding assignments, discussion with experienced programmers, help from others, understanding the hand-ins, math and logical thinking, difficulty spikes in the exercises, Conditionals and Arrays, Sessions or extra lectures, Q/A, what concepts to use for the exercises and hand-ins, individual revision + explanations in real life examples, being taught in collaborative coding skills, finding a way to practice, hand-ins doesn't really fit a learning curve,
# AAL: teacher assistants, rerun, more exercises, more videos, self studying, don't understand constructor, Smaller exercises in different concepts
# PDP: more videos, study cafe, study cafe

# self-reported confidence (Mean) - all: 5.317 and 6.151 
# self-reported confidence (Mean) - need help: 4.451 and 5.431
# self-reported confidence (Mean) - dont need help: 5.907 and 6.64


hist(subset(dfGPROquestionnnaire$`confidence-ex`,!grepl("Copenhagen", dfGPROquestionnnaire$study)),
     main="Histogram for self-reported confidence in exercises - AAL", 
     xlab="points", 
     xlim=c(1,10),
     breaks=10)



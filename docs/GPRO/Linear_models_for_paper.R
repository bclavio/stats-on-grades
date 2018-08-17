#require("RPostgreSQL")
library(MASS)
library(leaps)
library(gvlma)
library(car)
library(ggplot2)
library(gclus)

################################################
################## LOAD DATA ###################
################################################
# Set working directory to the path containing Fall 2017 data sources.
# Many of the required Fall 2017 files can be found at Z:\BNC\PBL development project\data\2018_SLERD_Paper_Analysis
# Each semester, data from Khan Academy, Peer Grade and Moodle Activity must be pulled from their respoective places.
SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/'} else {"/Users/Brian/PBL/Paper"}
setwd(SVNData)

# Load SSP scores
dfSSPGrades<-read.csv("SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load Midterm/Final grades
dfAALMidGrades<-read.csv("15-02-2018_GPRO-overview-MT-exam.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load SA grades
# contains Student id number for merging with 
dfSAGrades<-read.csv("15-02-2018_GPRO-gradebook.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load Peer Grade grades
dfPeerGrade<-read.csv("15-02-2018_Peer_Grade_course_overall_stats_data.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load Khan Academy exercises (106 exercises)
# Found on website under "Progress by Skill"
# Waring: Comes from Khan with extra white spaces.
dfKhan<-read.csv("15-02-2018_Khan_data.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
# Load Khan student user name and email pairs
# This file is only necessary if not all students use their student email address as
# their Khan academy user name. If necessary, this file will need to be manually updated every semester.
#dfKhanID<-read.csv("khan_id_new.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load merge file containing, student name, email address and ID
# Manually constructed. This will be necessary each semester if, for example,
# students use a Khan Academy user name which differs from their student name or email address.
dfKhanID<-read.csv("all_merge.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load SSP questions (111 questions into 14 topic categories)
questions<-read.csv("QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
# 1-9, 10-15, 16-23, 24-33, 34-45, 46-50, 51-56, 57-61, 62-64, 65-74, 75-87, 88-92, 93-97, 98-111
# demographics,attitudeEdu,reasonsUni,eduChoice,hsBehav,hsTrust,belongUncert,grit,growth,selfControl,personalTrait,acadAbility,studyHours,viewMedia

# Load Tutoring invitees
dfInvitees<-read.csv("tutoringInvitees.csv", header = FALSE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Load Dropout data
# Currently not enough dropouts for classification.
#dfDropOuts<-read.csv("dropout-Feb2018-Q999.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
#dfDropOuts<-dfDropOuts[dfDropOuts$'Status'!="active",]


################################################
# Load Moodle Activity Data (online)
# Moodle Activity data is located in a PostgreSQL database that must be accessed 
# using a login code (provided below) from Hendrik and an AAU connection.
# (see the next section for loading Moodle Activity Data while offline.)
# For online access of the PostgreSQL database through R:

# # create a connection to the database server
# load the PostgreSQL driver
# drv <- dbDriver("PostgreSQL")
# # create a connection to the PostgreSQL database (must be on AAU network)
# activityDataCon <- dbConnect(drv, dbname = "activity_data",
#                              host = "md-db-test.moodle.aau.dk", port = 5432,
#                              user = "hk", password = "=cD58BEtUE")
# # check for the existence of tables
# dbExistsTable(activityDataCon, "analyse_mdl_course")
# dbExistsTable(activityDataCon, "analyse_mdl_logstore_standard_log")
# dbExistsTable(activityDataCon, "analyse_mdl_user")
# 
# # query the data from PostgreSQL
# dfCourseData <- dbGetQuery(activityDataCon, "SELECT * from analyse_mdl_course")
# dfLogData <- dbGetQuery(activityDataCon, "SELECT * from analyse_mdl_logstore_standard_log")
# dfUserData <- dbGetQuery(activityDataCon, "SELECT * from analyse_mdl_user")

################################################
# Load Moodle Activity data (offline)
# Warning: No headers
# Second column of moodleUserData (Student ID) is used to match SA to Moodle activity data
#moodleCourseData<-read.csv("analyse_mdl_course.csv", header = FALSE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
#moodleLogData<-read.csv("analyse_mdl_logstore_standard_log.csv", header = FALSE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)
#moodleUserData<-read.csv("analyse_mdl_user.csv", header = FALSE, fill=TRUE, sep = ",", check.names=FALSE, stringsAsFactors=FALSE)

# Have not found a way yet to link Moodle activity to all other data,
# so it will be excluded from this analysis.

################################################
################# CLEAN DATA ###################
################################################
# Clean data sources (i.e., remove incomplete entries or irrelevant columns and reshape)
# Retain "Email address" as student ID used for merging
#
# SSP
# Remove students who did not finish the survey
dfSSPGrades<-dfSSPGrades[(dfSSPGrades$"State"=="Finished"), ]
# Rename column
colnames(dfSSPGrades)[10] <- "SSP score"
# Shorten using all SSP questions
alldfSSPGrades<-dfSSPGrades[,c(5,10:121)]

# Midterm grades
# Name columns
dfCPHMidGrades<-dfAALMidGrades[(dfAALMidGrades$"education"=="CPH"), ]
dfAALMidGrades<-dfAALMidGrades[(dfAALMidGrades$"education"!="CPH"), ]
# Shorten
dfCPHMidGrades<-dfCPHMidGrades[,c(1,2,3,5)]
dfAALMidGrades<-dfAALMidGrades[,c(1,2,3,5)]
# Make "education" predictors/groups
# PDP students get removed later because they did not do SSP
index<-dfAALMidGrades$"education" == "MEA"
dfAALMidGrades$"education"[index]<-1
index<-dfAALMidGrades$"education" == "PDP"
dfAALMidGrades$"education"[index]<-0
# Remove AAL invalid entries
loc <- apply(dfAALMidGrades[,c(3,4)],1,function(row) "-" %in% row)
dfAALMidGrades<-dfAALMidGrades[!loc,]
loc <- apply(dfAALMidGrades[,c(3,4)],1,function(row) "" %in% row)
dfAALMidGrades<-dfAALMidGrades[!loc,]
# Remove CPH invalid entries
loc <- apply(dfCPHMidGrades[,c(3,4)],1,function(row) "-" %in% row)
dfCPHMidGrades<-dfCPHMidGrades[!loc,]
loc <- apply(dfCPHMidGrades[,c(3,4)],1,function(row) "" %in% row)
dfCPHMidGrades<-dfCPHMidGrades[!loc,]
# Rename first column
colnames(dfAALMidGrades)[1] <- "ID number"
colnames(dfCPHMidGrades)[1] <- "ID number"

# SA
# Subset only grades
saTest<-dfSAGrades[,c(7,8,9,10,11,12,13,15,16,17,18,19)]
# Make non-attempts NA
saTest[saTest=="-"] <- NA
# Compute SA total average
saTest<-as.data.frame(lapply(saTest, as.numeric))
saRowAvg<-rowMeans(saTest, na.rm = TRUE, dims = 1)
# Create a column of SA attempts
saTest[!is.na(saTest)] <- 1
saTest[is.na(saTest)] <- 0
saNumComp<-rowSums(as.data.frame(lapply(saTest,as.numeric)))
# Subset all relevant columns
dfSAGrades<-dfSAGrades[,c(6,7,8,9,10,11,12,13,15,16,17,18,19)]
# Add column of SA attempts
dfSAGrades<-cbind(dfSAGrades,saRowAvg,saNumComp)
# Remove tmp user
dfSAGrades<-dfSAGrades[!grepl("tmpuser",dfSAGrades$'Email address'),]
# Subset only total average and number of attempts 
# Not doing this would provide more variables (i.e., grades) but would punish non-attempts with zeros.
# Because SA was not mandatory, we use number of attempts as a predictor instead of giving zeros for non-attempts.
dfSAGrades<-dfSAGrades[,c(1,14,15)]
# Give students who did not attempt SA a total average of zero.
# This similarly punishes students who did not do SA (might be students who are re-taking exam?)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
dfSAGrades[is.nan(dfSAGrades)] <- 0

# Peer Grade
# By student email and shorten (Peer Grade changed column name of "hand-in score" to "Submission score")
# Mandatory assignments
AALpeerGrade<-dfPeerGrade[,c('student_email','Submission score','feedback score','combined score')]
# change column name to match others
colnames(AALpeerGrade)[1] <- c("Email address")

# Khan Academy
#Sys.setlocale('LC_ALL','C') # required for reading non-unicode characters
# Format Khan Academy data from Long to Wide and convert student user ID occurrences
# in "Not Started", "Started" and "Completed" columns to numerical 0, 1 and 2, respectively.
# For each column, split list of student user ID names by hidden carriage return inserted from Khan Academy
# Not started column
s <- strsplit(dfKhan$"Not Started", split = "\n")
x<-data.frame(V1 = rep(dfKhan$"Exercise", sapply(s, length)), V2 = unlist(s))
x$"Not Started" <- rep(0,nrow(x))
colnames(x) <- c("Exercise", "Student Name","Completed")
# Started column
s <- strsplit(dfKhan$"Started", split = "\n")
y<-data.frame(V1 = rep(dfKhan$"Exercise", sapply(s, length)), V2 = unlist(s))
y$"Started" <- rep(1,nrow(y))
colnames(y) <- c("Exercise", "Student Name","Completed")
# Completed column
s <- strsplit(dfKhan$"Completed", split = "\n")
z<-data.frame(V1 = rep(dfKhan$"Exercise", sapply(s, length)), V2 = unlist(s))
z$"Completed" <- rep(2,nrow(z))
colnames(z) <- c("Exercise", "Student Name", "Completed")
# Join columns and reshape to wide format
joinedKhan<-rbind(x,y,z)
joinedKhan<-reshape(joinedKhan, idvar = "Student Name", timevar = "Exercise", direction = "wide")
# Remove duplicates
# Not sure how a student user name could occur more than once but it did in Fall 2017
joinedKhan<-joinedKhan[!duplicated(joinedKhan[,1]),]
# Remove Teachers
# In subsequent semesters this may or may not be necessary.
joinedKhan<-joinedKhan[!grepl("hendrik",joinedKhan$'Student Name'),]
# 106 Khan assignments
# Count number of completed Khan assignments for each student and store in "KhanCount" column
# Like SA, Khan Academny assignments were nto mandatory so we consider number of attempts instead of assigning zeros to non-attempts.
joinedKhan$"KhanCount" <- apply(joinedKhan, 1, function(x) sum(x==2))

# Tutoring invitees
# The students below atended at least one tutoring session.
dfAttendees = data.frame(c('vroua17@student.aau.dk','jlnc17@student.aau.dk','avafai17@student.aau.dk','nneuma17@student.aau.dk','jsimon16@student.aau.dk','fnje17@student.aau.dk','mepo17@student.aau.dk','evalbe17@student.aau.dk','nrux17@student.aau.dk')) 
colnames(dfAttendees) <- c("Email address")

################################################
# Merge all data sources created above

# SA and Midterm
saTest<-merge(dfKhanID,dfSAGrades,by="Email address")
# 11 students or staff not considered (check with command below)
# students were not on the list or did not take the final exam (drop outs?)
# 99 total AAL students who did SA and Midterm
dfComplete<-merge(saTest,dfAALMidGrades,by="ID number")

# Midterm and Khan
dfComplete<-merge(dfComplete,joinedKhan,by="Student Name",all.x=TRUE)
# Shorten
dfComplete<-dfComplete[,c(1:8,115)]
# Some students did not create a Khan Academy account (they will not be in the Khan database) 
# so replace those students who have NA in KhanCount with zeros
# These student get removed later because they did not do Peer Grade
dfComplete[is.na(dfComplete)]<-0

# Peer Grade 
dfComplete<-merge(dfComplete,AALpeerGrade,by='Email address',all.x=TRUE)
# Optional: remove students retaking the course. Peer Grade was not required for
# these students and none of them did it
dfComplete<-dfComplete[complete.cases(dfComplete), ]
# If we don't delete these students above, we can replace their PG scores with zeros.
#dfComplete[is.na(dfComplete)]<-0
# Otherwise, all students are now those not retaking the course.

# SSP
dfComplete<-merge(dfComplete,alldfSSPGrades,by='Email address',all.x=TRUE)
# Remove all incomplete entries leaves only 72 students
# This removes all PDP students
dfComplete<-dfComplete[complete.cases(dfComplete), ]
# Alternatively, we can zero all NAs and consider all 99 students who took the final exam
#dfComplete[is.na(dfComplete)]<-0

################################################
# Add factors for pass or fail Final exam 
# Can be used for plotting e.g., how often passing students attempted Khan Academy.
dfComplete$"pass/fail"[dfComplete$"Exam"<64] <- 0
dfComplete$"pass/fail"[dfComplete$"Exam">=64] <- 1

# An additional predictor of programming experience was found to be significant.
# SSP Questions 33 and 78 
avgProgExp<-as.data.frame(cbind(dfComplete$"Q. 33 /0.09",dfComplete$"Q. 78 /0.09"))
avgProgExp<-rowMeans(as.data.frame(lapply(avgProgExp,as.numeric)))
dfComplete[, "Avg. Prog. Exp."] <- avgProgExp

# Add column containing tutoring invitees and attendees
# Can be used for plotting.
# 0 = not invited, 1 = invited, 2 = invited and attended
dfComplete$"Tutoring"<-as.numeric(dfComplete$"Email address" %in% dfInvitees[,])
dfComplete$"Tutoring"[dfComplete$"Email address" %in% dfAttendees[,]]<-2

# Final clean
# Non attempts in SSP set to zero
dfComplete[is.na(dfComplete)]<-0
dfComplete[dfComplete=='-']<-0


################################################
################## ANALYSIS ####################
################################################
# Linear model: Final exam ~ Mid term grade is used as the base model 
# The adjusted r-squared value in the base model is compared to subsequent
# models to determine any improvement.
#
# Extract Final exam
drops <- c("Exam")
examScores<-dfComplete[,drops]
dfComplete<-dfComplete[ , !(names(dfComplete) %in% drops)]

################################################
# Initial Base model - FE ~ MT 
base1<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term'),na.action=na.omit)
# base model adjusted r-squared value
init_result<-summary(base1)$adj.r.squared

################################################
# By predictors excluding SSP single questions
inputData<-as.data.frame(lapply(dfComplete[,c(4,5,8,9,12)], as.numeric))

# Step-wise multiple linear regression
fit<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + .,data=inputData,na.action=na.omit)
step<-stepAIC(fit, direction="both")
step$anova # display results
summary(step)
# Best model is the addition of KhanCount to the base model
# Using AIC, t value and p value as the measure
# Notable absences are Peer Grade, saNumComp, saRowAvg.
# Compare best model to base
fit<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount'),na.action=na.omit)
summary(fit)
anova(base1,fit) 
# Significantly different than base model
# This becomes our new base model.

################################################
# New Base model - FE ~ MT + KhanCount
base2<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount'),na.action=na.omit)
# base model adjusted r-squared value
init_result<-summary(base2)$adj.r.squared

################################################
# By SSP category
# Category question ranges
# 1-9, 10-15, 16-23, 24-33, 34-45, 46-50, 51-56, 57-61, 62-64, 65-74, 75-87, 88-92, 93-97, 98-111
# Column ranges in "dfComplete" for the 14 SSP categories
# 13-21, 22-27, 28-35, 36-45, 46-57, 58-62, 63-68, 69-73, 74-76, 77-86, 87-99, 100-104, 105-109, 110-123
demographics<-rowMeans(as.data.frame(lapply(dfComplete[,13:21], as.numeric)))
attitudeEdu<-rowMeans(as.data.frame(lapply(dfComplete[,22:27], as.numeric)))
reasonsUni<-rowMeans(as.data.frame(lapply(dfComplete[,28:35], as.numeric)))
eduChoice<-rowMeans(as.data.frame(lapply(dfComplete[,36:45], as.numeric)))
hsBehav<-rowMeans(as.data.frame(lapply(dfComplete[,46:57], as.numeric)))
hsTrust<-rowMeans(as.data.frame(lapply(dfComplete[,58:62], as.numeric)))
belongUncert<-rowMeans(as.data.frame(lapply(dfComplete[,63:68], as.numeric)))
grit<-rowMeans(as.data.frame(lapply(dfComplete[,69:73], as.numeric)))
growth<-rowMeans(as.data.frame(lapply(dfComplete[,74:76], as.numeric)))
selfControl<-rowMeans(as.data.frame(lapply(dfComplete[,77:86], as.numeric)))
personalTrait<-rowMeans(as.data.frame(lapply(dfComplete[,87:99], as.numeric)))
acadAbility<-rowMeans(as.data.frame(lapply(dfComplete[,100:104], as.numeric)))
studyHours<-rowMeans(as.data.frame(lapply(dfComplete[,105:109], as.numeric)))
viewMedia<-rowMeans(as.data.frame(lapply(dfComplete[,110:123], as.numeric)))
test<-cbind(demographics,attitudeEdu,reasonsUni,eduChoice,hsBehav,hsTrust,belongUncert,grit,growth,selfControl,personalTrait,acadAbility,studyHours,viewMedia)

# Step-wise multiple linear regression by SSP category
# Use new base model
fit<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + demographics + attitudeEdu + reasonsUni + eduChoice + hsBehav + hsTrust + belongUncert + grit + growth + selfControl + personalTrait + acadAbility + studyHours + viewMedia,na.action=na.omit)
step<-stepAIC(fit, direction="both")
step$anova # display results
summary(step)
# Possibly try regsubsets() (below).

# 4 significant category predictors (in addition to midterm and KhanCount)
# Using AIC, t value and p value as the measure
# personalTrait, hsTrust, reasonsUni, selfControl, 

# Construct new model based on the best 4 categories
fit<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + personalTrait + hsTrust + reasonsUni + selfControl,na.action=na.omit)
summary(fit)
# Compare to new base model
anova(base2,fit)
# Significantly different than new base model.
# Now to test whether by SSP category or SSP question is better... 

################################################
# By SSP single question
# Throwing an infinity error when using stepAIC() (with any direction).
# This is because the number of X variables exceeds the number of observations (i.e., students).
# Possibly use Lasso or regsubsets().
# Using regsubsets, for example, results in a different set of significant predictors.
# Therefore, we have to manually test each SSP question. 
inputData<-as.data.frame(lapply(dfComplete[,c(13:123)], as.numeric))
# Remove columns containing all zeros
#inputData<-inputData[,-(which(colSums(inputData) == 0))] 
#inputData<-inputData*100

# Possibly use regsubset() for model selection
#best.subset <- regsubsets(as.numeric(examScores)~., inputData, nvmax=5, really.big=T)
#best.subset.summary <- summary(best.subset)
#best.subset.summary$outmat
#best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)
#best.subset.by.adjr2
#best.subset.by.cp <- which.min(best.subset.summary$cp)
#best.subset.by.cp
#best.subset.by.bic <- which.min(best.subset.summary$bic)
#best.subset.by.bic

# Manual model selection
results<-numeric(111)
pvalues<-numeric(111)
for (i in 1:111){
  pred<-as.numeric(inputData[,i])
  # Add to Base2 model
  x<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + pred,na.action=na.omit)
  y<-summary(x)$adj.r.squared
  results[i]<-y
  xx<-summary(x)
  pvalues[i]<-pf(xx$fstatistic[1], xx$fstatistic[2], xx$fstatistic[3],
                    lower.tail = FALSE)
}
# Find most significant SSP single questions
x<-order(results, decreasing=TRUE)
top<-which(results[x]>init_result)
best<-x[top]

# 42 SSP individual questions significantly improve the Base 2 model
# Only the top 5 improve the model collectively
# 82  22 107  60  48 (not used, see below)

fit<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$"Q. 82 /0.09") + as.numeric(dfComplete$"Q. 22 /0.09") + as.numeric(dfComplete$"Q. 107 /0.09") + as.numeric(dfComplete$"Q. 60 /0.09") + as.numeric(dfComplete$"Q. 48 /0.09"),na.action=na.omit)
summary(fit)
# Compare to new Base 2 model
anova(base2,fit)

################################################
# Compare SSP category model to SSP question model
fit1<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + personalTrait + hsTrust + reasonsUni + selfControl,na.action=na.omit)
fit2<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$"Q. 82 /0.09") + as.numeric(dfComplete$"Q. 22 /0.09") + as.numeric(dfComplete$"Q. 107 /0.09") + as.numeric(dfComplete$"Q. 60 /0.09") + as.numeric(dfComplete$"Q. 48 /0.09"),na.action=na.omit)
summary(fit1)
summary(fit2)
# Anova probably not appropriate to compare these two models.
#anova(fit1,fit2)
# Model using SSP questions (fit2) is better when considering adjusted r-squared.

################################################
# Does adding Avg. Prog. Exp. improve the SSP questions model?
# Yes, it does.
fit<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount')  + as.numeric(dfComplete$'Avg. Prog. Exp.') + as.numeric(dfComplete$"Q. 82 /0.09") + as.numeric(dfComplete$"Q. 22 /0.09") + as.numeric(dfComplete$"Q. 107 /0.09") + as.numeric(dfComplete$"Q. 60 /0.09") + as.numeric(dfComplete$"Q. 48 /0.09"),na.action=na.omit)
summary(fit)
# Does it change which SSP questions are now significant?
# Yes, and the resulting model is better.

# New base model - FE ~ MT + KhanCount + Avg. Prog. Exp.
base3<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount')  + as.numeric(dfComplete$'Avg. Prog. Exp.'),na.action=na.omit)

################################################
# Re-test SSP questions using new Base 3 model
inputData<-as.data.frame(lapply(dfComplete[,c(13:123)], as.numeric))

# Manual model selection
results<-numeric(111)
pvalues<-numeric(111)
for (i in 1:111){
  pred<-as.numeric(inputData[,i])
  # New Base3 model
  x<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$'Avg. Prog. Exp.') + pred,na.action=na.omit)
  y<-summary(x)$adj.r.squared
  results[i]<-y
  xx<-summary(x)
  pvalues[i]<-pf(xx$fstatistic[1], xx$fstatistic[2], xx$fstatistic[3],
                 lower.tail = FALSE)
}
# Find most significant SSP single questions
x<-order(results, decreasing=TRUE)
top<-which(results[x]>init_result)
best<-x[top]

# New set of significant SSP questions
# 82  60  50  22 107
# These correspond to SSP categories: personalTrait, growth, hsTrust, attitudeEdu and viewMedia.
# This is the final and best model
modelFinal<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$'Avg. Prog. Exp.') + as.numeric(dfComplete$"Q. 82 /0.09") + as.numeric(dfComplete$"Q. 60 /0.09") + as.numeric(dfComplete$"Q. 50 /0.09") + as.numeric(dfComplete$"Q. 22 /0.09") + as.numeric(dfComplete$"Q. 107 /0.09"),na.action=na.omit)
summary(modelFinal)

# Does this change when adding Peer Grade?
# It does slightly improve (adjusted r-squared) but it
# was not originally selected for using stepAIC().

################################################
# Test to confirm the final model is appropriate.
modelFinal<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$'Avg. Prog. Exp.') + as.numeric(dfComplete$"Q. 82 /0.09") + as.numeric(dfComplete$"Q. 60 /0.09") + as.numeric(dfComplete$"Q. 50 /0.09") + as.numeric(dfComplete$"Q. 22 /0.09") + as.numeric(dfComplete$"Q. 107 /0.09"),na.action=na.omit)
summary(modelFinal)
mean(modelFinal$residuals)
plot(density(resid(modelFinal)))
qqnorm(resid(modelFinal))
qqline(resid(modelFinal))
plot(modelFinal)

# Perceived problem with Homoscedasticity
# Looking at the Residuals vs Fitted plot,
# the fitted line is not straight.
# Automated test contradicts this, so okay.
gvlma(modelFinal) 

# Manual check
# Outliers
c<-cooks.distance(modelFinal)
plot(c, pch="*", cex=2, main="Influential Observations by Cooks distance")
abline(h = 4*mean(c, na.rm=T), col="red") 
# No autocorrelation
acf(modelFinal$residuals)
# Correlation of variables to residuals
cor.test(as.numeric(dfComplete$"Q. 107 /0.09"), modelFinal$residuals) 
#crPlots(fit)
#ncvTest(fit)
# Multicollinearity
vif(modelFinal)
sqrt(vif(modelFinal)) > 2
# Normaility of residuals
qqPlot(modelFinal, main="QQ Plot")
#
sresid <- studres(modelFinal)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


################################################
################## PLOTTING ####################
################################################

# Final Model
modelFinal<-lm(as.numeric(examScores) ~ as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$'Avg. Prog. Exp.') + as.numeric(dfComplete$"Q. 82 /0.09") + as.numeric(dfComplete$"Q. 60 /0.09") + as.numeric(dfComplete$"Q. 50 /0.09") + as.numeric(dfComplete$"Q. 22 /0.09") + as.numeric(dfComplete$"Q. 107 /0.09"),na.action=na.omit)
summary(modelFinal)
plot(modelFinal)


# Correlation plot matrix
inputData<-dfComplete[,c('mid-term','KhanCount','Avg. Prog. Exp.',"Q. 82 /0.09","Q. 60 /0.09","Q. 50 /0.09","Q. 22 /0.09","Q. 107 /0.09")]
inputData<-cbind(examScores,inputData)

test<-melt(inputData,id.vars='examScores')
ggplot(test) +
  geom_jitter(aes(value,examScores, colour=variable),) + geom_smooth(aes(value,examScores, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable,scales="free_x") +
  labs(x = "...", y = "...")

# Scatter plot matrix
#pairs(~as.numeric(dfComplete$'mid-term') + as.numeric(dfComplete$'KhanCount') + as.numeric(dfComplete$'Avg. Prog. Exp.'),data=dfComplete,main="")

# Correlation plot matrix
dta<-cbind(as.numeric(examScores),as.numeric(dfComplete$'mid-term'), as.numeric(dfComplete$'KhanCount'),as.numeric(dfComplete$'Avg. Prog. Exp.'))
dta.r <- abs(cor(dta)) # correlations
dta.col <- dmat.color(dta.r) # colors
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="..." )

# AAL and CPH linear models for FE ~ MT
# Use all AAL and all CPH students
inputData<-as.data.frame(cbind(as.numeric(dfAALMidGrades[,2]),as.numeric(dfAALMidGrades[,3]),as.numeric(dfAALMidGrades[,4])))
colnames(inputData)[1]<-"Education"
colnames(inputData)[2]<-"Midterm"
colnames(inputData)[3]<-"FinalExam"

ggplot(inputData, aes(x=Midterm, y=FinalExam,group=1,color=Education)) +
  geom_point(shape=1) +
  #ylim(10.0, 90.0) + # set limits
  geom_smooth(method=lm)

inputData<-as.data.frame(cbind(as.numeric(dfCPHMidGrades[,3]),as.numeric(dfCPHMidGrades[,4])))
colnames(inputData)[1]<-"Midterm"
colnames(inputData)[2]<-"FinalExam"

ggplot(inputData, aes(x=Midterm, y=FinalExam,group=1)) +
  geom_point(shape=1) +
  #xlim(0.0, 100.0) + # set limits
  geom_smooth(method=lm)

# AAL students Final Exam against saNumComp and KhanCount by pass/fail
# Not significant
SAComp<-as.numeric(dfComplete$"saNumComp")
FinalExam<-as.numeric(examScores)
Passing<-as.factor(dfComplete$"pass/fail")
ggplot(dfComplete, aes(x=SAComp, y=FinalExam,group=1,color=Passing)) +
  geom_point(shape=1) +
  #ylim(10.0, 90.0) + # set limits
  geom_smooth(method=lm)
# Significant
KhanAttempt<-as.numeric(dfComplete$"KhanCount")
FinalExam<-as.numeric(examScores)
Passing<-as.factor(dfComplete$"pass/fail")
ggplot(dfComplete, aes(x=KhanAttempt, y=FinalExam,group=1,color=Passing)) +
  geom_point(shape=1) +
  #ylim(10.0, 90.0) + # set limits
  geom_smooth(method=lm)


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
library(CTT)
#library(gsubfn)
library(Hmisc)
library(psych)
library(ggplot2)
library(reshape2)

#library(ecdfHT)


### Import SSP data and get in shape  

setwd('Z:/BNC/PBL development project/data/analysis_data/SSP/')

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

dfSSPgrades <- rbind(dfSSPgradesCPH,dfSSPgradesAAL) 
dfSSPgrades <- dfSSPgrades[grepl("Finished", dfSSPgrades$State),]
dfSSPgrades["rowID"] <- seq(1:nrow(dfSSPgrades))

# get hours not score 
dfSSPgrades$`Q. 93 /0.09`<- dfSSPanswers$`Response 93`
dfSSPgrades$`Q. 95 /0.09`<- dfSSPanswers$`Response 95`
dfSSPgrades$`Q. 96 /0.09`<- dfSSPanswers$`Response 96`


# melting the data 
dfSSPgradesMelt <- dfSSPgrades[,-c(1:4,6:9,122:123)]
dfSSPgradesMelt1 <- data.frame(lapply(dfSSPgradesMelt, function(x) { gsub("-", 0, x) }))
dfSSPgradesMelt1 <- data.frame(dfSSPgradesMelt1[1], lapply(dfSSPgradesMelt1[2:113], function(x) as.numeric(as.character(x))) )
colnames(dfSSPgradesMelt) <- gsub(" ","",colnames(dfSSPgradesMelt))
names(dfSSPgradesMelt1) <- names(dfSSPgradesMelt)
names(dfSSPgradesMelt1)[1] <- "email"
dfSSPgradesMelt1 <-melt(dfSSPgradesMelt1, by = c("email"))

#############################
## write to database (uncomment below)
library(RMysSQL)
libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep='')) # MAC and Windows
mydb = dbConnect(MySQL(), user=LAuserID, password=LAuserpass, dbname=LAdb, host=LAserver)

#dbWriteTable(mydb, value = dfSSPgradesMelt1, name = "tbl_SSPQmelt1", append = TRUE, row.names=FALSE)
#write.csv(dfSSPgradesMelt1,file = "dfSSPgradesMelt1DB.csv", row.names = FALSE)


# writing more data to database 

#setwd('Z:/BNC/PBL development project/data/analysis_data/dropOut/data_2017cohortCPHAAL')
#dfOptag<-read.csv("Optag_2017_bac_medialogi_adggru2.csv", header = TRUE, sep = ",", encoding="utf8")
#dbWriteTable(mydb, value = dfOptag, name = "tbl_dfOptag1", temporary = TRUE, row.names=FALSE)

#dfAAUgradesDB<-read.csv("AAUgrades.csv", header = TRUE, sep = ",", encoding="utf8", check.names=FALSE, stringsAsFactors=FALSE)
#row.names(dfAAUgradesDB) <- NULL
#dbWriteTable(mydb, value = dfAAUgradesDB, name = "tbl_AAUgrades", temporary = TRUE, row.names=FALSE)

#dffrafaldAAUmedDB<-read.csv("frafaldAAUmed.csv", header = TRUE, sep = ",", encoding="utf8")
#dbWriteTable(mydb, value = dffrafaldAAUmedDB, name = "tbl_frafaldAAUmed2", temporary = TRUE, row.names=FALSE)

#dffrafaldAAUallDB<-read.csv("frafaldAAUalls.csv", header = TRUE, sep = ",", encoding="utf8")
#dbWriteTable(mydb, value = dffrafaldAAUallDB, name = "tbl_frafaldAAUall1", temporary = TRUE, row.names=FALSE)

dbClearResult(dbListResults(mydb)[[1]]) # after each fetch
dbDisconnect(mydb) # after working with the database
detach("package:RMySQL", unload=TRUE)

#######################
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



### Import SSP data

SVNData<-if(grepl("BiancaClavio", getwd())){'C:/Users/BiancaClavio/Documents/SVN/01Projects/SSP/'} else {"~/SVN/01Projects/SSP/"}
setwd(SVNData)

dfQAGrades<-read.csv("QuestionsOverview.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE)
dfSSPgradesCPH<-read.csv("SSPgradesTestCPH 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPgradesAAL<-read.csv("SSPgradesTestAAL 02-10.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)

####################################
###### the answers are not in use
dfSSPanswersCPH<-read.csv("SSPanswersTestCPH 28-09.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPanswersAAL<-read.csv("SSPanswersTestAAL 28-09.csv", header = TRUE, fill=TRUE, sep = ",", check.names=FALSE, encoding="UTF-8", stringsAsFactors=FALSE)
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("No influence","Not at all true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Limited influence","Slightly true",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Some influence","Some influence",dfSSPanswersAAL))
dfSSPanswersAAL[35:43] <- apply(dfSSPanswersAAL[35:43],2, function(dfSSPanswersAAL) gsub("Decisive influence","Completely true",dfSSPanswersAAL))
dfSSPanswersCPH["Campus"]<-"CPH"
dfSSPanswersAAL["Campus"]<-"AAL"
dfSSPanswers <- rbind(dfSSPanswersCPH,dfSSPanswersAAL) 
###################################

dfSSPgradesCPH["Campus"]<-"CPH"
dfSSPgradesAAL["Campus"]<-"AAL"
dfSSPgradesAAL <- dfSSPgradesAAL[-nrow(dfSSPgradesAAL),]
dfSSPgradesCPH <- dfSSPgradesCPH[-nrow(dfSSPgradesCPH),]

dfSSPgrades <- rbind(dfSSPgradesCPH,dfSSPgradesAAL) 
dfSSPgrades <- dfSSPgrades[!grepl("In progress", dfSSPgrades$State),]
dfSSPgrades <- data.frame(lapply(dfSSPgrades, function(x) { gsub("-", 0, x) }))
dfSSPgradesStat <- data.frame( lapply(dfSSPgrades[11:121], function(x) as.numeric(as.character(x))) )

# computes avg grade for each category and for each student
avgGrades <- NULL
avgGrades['avgDemographics'] <- list(rowMeans(dfSSPgradesStat[1:9]))
avgGrades['avgAttitude'] <- list(rowMeans(dfSSPgradesStat[10:15]))
avgGrades['avgReasons'] <- list(rowMeans(dfSSPgradesStat[16:23]))
avgGrades['avgChoice'] <- list(rowMeans(dfSSPgradesStat[24:33]))
avgGrades['avgHSBehave'] <- list(rowMeans(dfSSPgradesStat[34:45]))
avgGrades['avgHSTrust'] <- list(rowMeans(dfSSPgradesStat[46:50]))
avgGrades['avgBelonging'] <- list(rowMeans(dfSSPgradesStat[51:56]))
avgGrades['avgGrit'] <- list(rowMeans(dfSSPgradesStat[57:61]))
avgGrades['avgGrowth'] <- list(rowMeans(dfSSPgradesStat[62:64]))
avgGrades['avgControl'] <- list(rowMeans(dfSSPgradesStat[65:74]))
avgGrades['avgTraits'] <- list(rowMeans(dfSSPgradesStat[75:87]))
avgGrades['avgAcademic'] <- list(rowMeans(dfSSPgradesStat[88:92]))
avgGrades['avgHours'] <- list(rowMeans(dfSSPgradesStat[c(94,97)])) # I have removed 3 questions
avgGrades['avgMedialogy'] <- list(rowMeans(dfSSPgradesStat[98:111]))
avgGrades <- data.frame( lapply(avgGrades, function(x) as.numeric(as.character(x))) )

# normalizes avg grades
# Students with averages above 1 are most likely to continue Medialogy while
# students with averages below -1 are most likely to dropout.
scaled.avgGrades <- avgGrades
scaled.avgGrades[1:14] <- scale(avgGrades[1:14])

# checks that we get mean of 0 and sd of 1
colMeans(scaled.avgGrades)
apply(scaled.avgGrades, 2, sd)

# TODO: find the lowest 10% students for each category: https://statistics.laerd.com/statistical-guides/standard-score-3.php







scaled.avgGrades["Campus"] <- dfSSPgrades$Campus
scaled.avgGrades["rowID"] <- seq(1:nrow(scaled.avgGrades))

# scaled data with facet of AAL and CPH common for all students
ggplot(data= melt(scaled.avgGrades[1:15], id.var="Campus"), aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Campus)) +
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

ggplot(dfSSPgradesStat, aes(rowSums(dfSSPgradesStat))) + geom_density()
dfSSPgradesStat





# TODO: reshape the dataset 
i <- 1
scaled.avgGrades1 <- scaled.avgGrades



# scaled data common for all students
ggplot(melt(scaled.avgGrades, id.var="rowID"), aes(x=variable, y=value)) + 
  geom_boxplot(data= melt(scaled.avgGrades[1:14])) +
  coord_flip() +
  theme_bw() + 
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        #panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none") +
geom_point(scaled.avgGrades, aes(x=rowID[1], y=scaled.avgGrades), 
           color=ifelse(scaled.avgGrades$rowID==1, "blue","white"),
           cex=ifelse(scaled.avgGrades$rowID==1, 5, 0),
           alpha=ifelse(scaled.avgGrades$rowID==1, 1, 0)) 

#geom_point(avgGrades, aes(x=rowID[i], y=avgGrades), 
#           color=ifelse(avgGrades$rowID==i, "blue","white"),
#           cex=ifelse(avgGrades$rowID==i, 5, 0),
#           alpha=ifelse(avgGrades$rowID==i, 1, 0)) 






scaled.avgGrades[2]< -2 |ggplot(avgGrades, aes(x=variable, y=value)) + 
  geom_boxplot(data= melt(avgGrades[1:14])) +
  coord_flip() +
  theme_bw() + 
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        #panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none"
  )


# Data points for each individual student
p2 +  geom_point(aes(x=rowID[i], y=avgGrades[2]), 
                 color=ifelse(avgGrades$rowID==i, "blue","white"),
                 cex=ifelse(avgGrades$rowID==i, 5, 0),
                 alpha=ifelse(avgGrades$rowID==i, 1, 0)) 


i <- 3 # rowID
j <- 1 # category

# plots the boxplot of a category
p <- ggplot(data = avgGrades,aes(x=rowID[i], y=avgGrades[j])) +
  geom_boxplot() + 
  coord_flip() +
  ggtitle(paste0("Category ",j)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=2) + 
  theme(axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank(),
  axis.title.x=element_blank())
p
# plots the data point of the student
p +  geom_point(color=ifelse(avgGrades$rowID==i, "blue","red"),
                cex=ifelse(avgGrades$rowID==i, 7, 0),
                alpha=ifelse(avgGrades$rowID==i, 1, 0)) 
 

avgGrades.melted <- melt(avgGrades, id='rowID')

#then plot
p2 <- ggplot(avgGrades.melted, aes(x=factor(avgGrades),y=rowID))+
  geom_boxplot() + labs(title="CMP") +facet_wrap(~variable)
p2




# plots the boxplot of a category
p <- ggplot(avgGrades,aes(x=rowID[i], y=avgGrades)) +
  geom_boxplot() + 
  coord_flip() +
  ggtitle(paste0("Category ",j)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=2) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())
p
# plots the data point of the student
p +  geom_point(color=ifelse(avgGrades$rowID==i, "blue","red"),
                cex=ifelse(avgGrades$rowID==i, 7, 0),
                alpha=ifelse(avgGrades$rowID==i, 1, 0)) 





# mm = melt(df, id=c('id','factor.col'))
# ggplot(mm)+geom_boxplot(aes(x=paste(variable,factor.col,sep="_"), y=value))


avgGrades.melted <- melt(avgGrades, id='rowID')
ggplot(avgGrades.melted)+geom_boxplot(aes(x=paste(variable,sep="_"), y=avgGrades))

p <- ggplot(data=avgGrades,aes(x=rowID[i], y=avgGrades[c(1)])) +
  geom_boxplot() + 
  coord_flip() +
  ggtitle(paste0("Category ",j)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=2) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank())
p

p +  geom_point(color=ifelse(avgGrades$rowID==i, "blue","red"),
                cex=ifelse(avgGrades$rowID==i, 7, 0),
                alpha=ifelse(avgGrades$rowID==i, 1, 0)) 





# perhaps we want to show the specific analysis of the lowest score of each student?



# TODO percentile for each category and for each student??
perc.rank <- function(x) trunc(rank(x))/length(x)
# my.df <- data.frame(x=rnorm(200))
# my.df <- within(my.df, xr <- perc.rank(x))

i <- 1
j <- 1
percGrades <- 0
percGrades <- dfSSPgradesStat[1:9]
percGrades <- within(percGrades, xr1 <- prop.table(perc.rank(dfSSPgradesStat))*100) #prop.table?
j <- i+1

percGrades <- within(percGrades, xr1 <- prop.table(perc.rank(dfSSPgradesStat[1]))*100)


percGrades['Q1'] <- dfSSPgradesStat[1]
percGrades['Q2'] <- within(percGrades$Q1, xr <- prop.table(perc.rank(dfSSPgradesStat[1]))*100)


perc.rank(dfSSPgradesStat[1])

percGrades <- NULL
for (i in 1:nrow(dfSSPgradesStat)){
    percGrades[i] <- within(percGrades, xr <- prop.table(perc.rank(dfSSPgradesStat[i]))*100)
}


####################################

percGrades <- 0
for (i in 1:nrow(dfSSPgradesStat)){
    percGrades[,i] <- dfSSPgradesStat[,i]
    percGrades <- within(percGrades, xr <- prop.table(perc.rank(dfSSPgradesStat[,i]))*100)
}



# find out how many students are under -2 SD for each question
highRisk <- subset(scaled.avgGrades, scaled.avgGrades[1]< -2 |
                     scaled.avgGrades[2]< -2 |
                     scaled.avgGrades[3]< -2 |
                     scaled.avgGrades[4]< -2 |
                     scaled.avgGrades[5]< -2 |
                     scaled.avgGrades[6]< -2 |
                     scaled.avgGrades[7]< -2 |
                     scaled.avgGrades[8]< -2 |
                     scaled.avgGrades[9]< -2 |
                     scaled.avgGrades[10]< -2 |
                     scaled.avgGrades[11]< -2 |
                     scaled.avgGrades[12]< -2 |
                     scaled.avgGrades[13]< -2 |
                     scaled.avgGrades[14]< -2 )





studentData <- sqldf('Select rowId,Campus from dfSSPgrades')
studentData['FirstName'] <- dfSSPgrades[,2]
studentData['SurName'] <- dfSSPgrades[,1]
studentData <- merge(studentData, scaled.avgGrades, by= "rowID")

setwd('C:/Users/BiancaClavio/Documents/stats-on-grades/docs')
write.csv(studentData,file = "studentData.csv")
personalized_info <- read.csv(file = "studentData.csv")

## Loop
for (i in 1:nrow(personalized_info)){
  rmarkdown::render(input = "SSP-MailMerge.Rmd",
                    output_format = "pdf_document",
                    output_file = paste("SSPanalysis_", i, ".pdf", sep=''),
                    output_dir = "handouts/")
}

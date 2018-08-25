

library(ggplot2)
library(reshape2)

setwd("Z:/BNC/PBL development project/Data/analysis_data/SSP")

dfSSPreflectAAL <- read.csv("Reflection_SSP_AAL_gradesNEW.csv", header = TRUE, fill=TRUE, sep = ",")
dfSSPreflectCPH <- read.csv("Reflection_SSP_CPH_gradesNEW.csv", header = TRUE, fill=TRUE, sep = ",")

# overwrite column names in CPH to match AAL 
names(dfSSPreflectCPH) <- names(dfSSPreflectAAL)

# combining AAL and CPH
dfSSPreflect <- rbind(dfSSPreflectAAL, dfSSPreflectCPH)

# remove uncompleted attempts and overall average
dfSSPreflect <- dfSSPreflect[- grep("-", dfSSPreflect$Grade.10.00),]


################################################
# no groupings of students

# select the topic scores:
dfSSPreflectScores <- dfSSPreflect[, c(12, 15, 18, 21, 24, 27, 30)] 

# convert to numeric
dfSSPreflectScores <- data.frame( lapply(dfSSPreflectScores, function(x) as.numeric(as.character(x))) )

#create a vector with topic names
labs <- c("Social support for studying",  "High school habits",  "Study habits at AAU", "Grit", "Growth mindset", "Time commitments", "Understanding of Medialogy")
#use the new vector to change the column names
colnames(dfSSPreflectScores)<- labs

# melt to do graphs
dfSSPreflectScoresMelt <- melt(dfSSPreflectScores)

# facet histograms
ggplot(data = dfSSPreflectScoresMelt, aes(x = value)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_wrap(~variable)

# boxplots
ggplot(dfSSPreflectScoresMelt, aes(x=variable, y=value)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw() +
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")

summary(dfSSPreflectScores)



##############################
## could group student answers based on the recommendation answer for each topic.
dfSSPreflectRecommend <- dfSSPreflect

# convert from factor to char
dfSSPreflectRecommend[, c(11, 14, 17, 20, 23, 26, 29)]  <- data.frame(lapply(dfSSPreflectRecommend[, c(11, 14, 17, 20, 23, 26, 29)], as.character), stringsAsFactors=FALSE)

# substitute with Yes and No
dfSSPreflectRecommend[c(11, 14, 17, 20, 23, 26, 29)] <-  apply(dfSSPreflectRecommend[c(11, 14, 17, 20, 23, 26, 29)],2, function(dfSSPreflectRecommend) gsub("0.42","Yes",dfSSPreflectRecommend))
dfSSPreflectRecommend[c(11, 14, 17, 20, 23, 26, 29)] <-  apply(dfSSPreflectRecommend[c(11, 14, 17, 20, 23, 26, 29)],2, function(dfSSPreflectRecommend) gsub("0.00","No",dfSSPreflectRecommend))

# select recommendation score and ratings
dfSSPreflectRecommend <- dfSSPreflectRecommend[, c(11,12, 14,15, 17,18, 20,21, 23,24, 26,27, 29,30)] 


#create a vector with topic names
labels <- c("A", "1B", "A",  "2B", "A", "3B", "A", "4B", "A", "5B", "A", "6B", "A", "7B")
#use the new vector to change the column names
colnames(dfSSPreflectRecommend)<- labels

# melt to do graphs
dfSSPreflectRecommendMelt <- melt(dfSSPreflectRecommend, id.vars = "A")


##################################
# subset the recommendations
dfSSPreflectRecommendMeltNo <- subset(dfSSPreflectRecommendMelt, dfSSPreflectRecommendMelt$A == "No")
dfSSPreflectRecommendMeltYes <- subset(dfSSPreflectRecommendMelt, dfSSPreflectRecommendMelt$A == "Yes")

# prepare data for Yes group, remove the extra column
dfSSPreflectRecommendMeltYes <- dfSSPreflectRecommendMeltYes[,-1]
dfSSPreflectRecommendMeltYes$value <- as.numeric(dfSSPreflectRecommendMeltYes$value)

# facet histograms for Yes group
ggplot(data = dfSSPreflectRecommendMeltYes, aes(x = value)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_wrap(~variable)

# boxplots for Yes group
ggplot(dfSSPreflectRecommendMeltYes, aes(x=variable, y=value)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw() +
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")


# prepare data for No group, remove the extra column
dfSSPreflectRecommendMeltNo <- dfSSPreflectRecommendMeltNo[,-1]
dfSSPreflectRecommendMeltNo$value <- as.numeric(dfSSPreflectRecommendMeltNo$value)

# facet histograms for No group
ggplot(data = dfSSPreflectRecommendMeltNo, aes(x = value)) + 
  geom_histogram(binwidth = 0.1) + 
  facet_wrap(~variable)

# boxplots for No group
ggplot(dfSSPreflectRecommendMeltNo, aes(x=variable, y=value)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw() +
  #facet_grid(Campus ~ .) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        #axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")


##################################
# wanted to make a hist and boxplot illustration difference between the two groups, but too much work and doesn't look interesting



##########
# Conclusion: nothing interesting in this analysis and few student responses, so it's better to look at the comment sections.

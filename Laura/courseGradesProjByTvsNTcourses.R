setwd("~/Desktop/")

#load data
data <- read.csv("dfAAUMarriedGrades.csv", header = T)

#some checking
summary(data)
dim(data)

#try with ggplot only to improve the graphic
#by year across campus

#subset to only bachelor
bac <- subset(data, type == "bachelor" & SPV != "BSc" & startaar <= '2014')
#and check
summary(bac)
bac[1:5, 1:5]

#load package for pivoting
library(reshape)
#melt
bac.m<-melt(bac, id= c("bctdf", "takenInSem", "enrolID", "CourseLocation"), measure = "gradeNum")
#check
summary(bac.m)
bac.m[1:5, 1:6]
#mean by student
bacmeanbyStud <- cast(bac.m, enrolID+CourseLocation ~ variable+bctdf, mean)
#check
summary(bacmeanbyStud)
bacmeanbyStud[1:5, 1:6]
attach(bacmeanbyStud)

#plot 
library(ggplot2)
library(ggpubr)

#Keep Project on the y-axis -OK
#try to add R2
#stat_smotth_func is not recognised
#stat_poly_eq asks for x, provding the variable name doesn't work
pacrossSCy <- ggplot(bacmeanbyStud, aes(y=gradeNum_project)) + 
  geom_jitter(aes(x= gradeNum_T, colour = "gradeNum_T"), alpha = 0.4, size = 5)+
  stat_cor(aes(y=gradeNum_project, x= gradeNum_T), method = "pearson", color= "red", 
           label.x = -6, label.y = 12)+
  geom_smooth(aes(y=gradeNum_project, x= gradeNum_T), method = "lm", color= "red")+
  geom_jitter(aes(x= gradeNum_NT, color= "gradeNum_NT"), data=bacmeanbyStud, 
              alpha = 0.4, size = 5)+
  stat_cor(aes(y=gradeNum_project, x= gradeNum_NT), bacmeanbyStud,  
           method = "pearson", color= "blue", label.x = -6, label.y = 11)+
  geom_smooth(aes(y=gradeNum_project, x= gradeNum_NT), bacmeanbyStud,  
              method = "lm", color= "blue")+
  geom_jitter(aes(x= gradeNum_elective, color= "gradeNum_elective"), 
              data=bacmeanbyStud,  alpha = 0.4, size = 5)+
  stat_cor(aes(y=gradeNum_project, x= gradeNum_elective), bacmeanbyStud,  
           method = "pearson", color= "green", label.x = -6, label.y = 10)+
  geom_smooth(aes(y=gradeNum_project, x= gradeNum_elective), bacmeanbyStud,  
              method = "lm", color= "green")+
  scale_color_manual(name="Courses:", breaks=c("gradeNum_T", "gradeNum_NT", "gradeNum_elective"),
                     values = c("gradeNum_T" = "red", "gradeNum_NT"="blue", "gradeNum_elective"="green"),
                     labels= c("Technical", "Non-technical", "Electives"))+
  labs(y = "Average project grade", x = "Average course grade", 
       title= "Across campus, across semesters")+
  xlim(-8, 12)+ ylim(-8,12)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        panel.grid.minor = element_line(colour = 'light grey'),
        axis.text.x = element_text(size=15, colour="black"), 
        axis.text.y = element_text(size=15, colour="black"), 
        axis.title.x = element_text(size=15, colour="black", face = "bold"), 
        axis.title.y = element_text(size=15, colour="black", face = "bold"),
        legend.title= element_text(size=15, colour="black", face = "bold"),
        legend.text =  element_text(size=15, colour="black"), 
        legend.position="bottom")
pacrossSCy

#export as png
png("Bachelor grade correlation plot across semester and campus.png", height= 6, width=8, units = 'in', res= 300)
pacrossSCy
dev.off()

#across semester, wrapped by campus
library(tidyr)
WbyC<- bacmeanbyStud %>%
  gather(-enrolID, -CourseLocation, -gradeNum_project, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = gradeNum_project, color = var)) +
  geom_jitter(alpha = 0.4, size = 3) +
  stat_cor(method = "pearson", show.legend = F, size= 3, fontface = "bold")+
  geom_smooth(method = "lm")+
  facet_wrap(~ CourseLocation, scales = "free") +
  labs(y = "Average project grade", x = "Average course grade", 
       title= "Across semester, by Campus")+
  xlim(-8, 12)+ ylim(-8,12)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        panel.grid.minor = element_line(colour = 'light grey'),
        axis.text.x = element_text(size=15, colour="black"), 
        axis.text.y = element_text(size=15, colour="black"), 
        axis.title.x = element_text(size=15, colour="black", face = "bold"), 
        axis.title.y = element_text(size=15, colour="black", face = "bold"),
        legend.title= element_text(size=15, colour="black", face = "bold"),
        legend.text =  element_text(size=15, colour="black"), 
        legend.position="bottom")
WbyC

#export as png
png("Bachelor grade correlation plot across semester, wrapped by Campus2.png", height= 6, width=10, units = 'in', res= 300)
WbyC
dev.off()

#representation by semester
#mean by student
bacmeanbySem <- cast(bac.m, enrolID+CourseLocation+takenInSem ~ variable+bctdf, mean)
#check
summary(bacmeanbySem)
bacmeanbySem[1:5, 1:6]
attach(bacmeanbySem)
bac1to6<- bacmeanbySem[which(takenInSem <7),]
summary(bac1to6)
attach(bac1to6)

WbyS<- bac1to6 %>%
  gather(-enrolID, -CourseLocation, -takenInSem, -gradeNum_project, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = gradeNum_project, color = var)) +
  geom_jitter(alpha = 0.4, size = 4) +
  stat_cor(method = "pearson", show.legend = F, size= 5, fontface = "bold", label.x.npc = "right", label.y.npc = "bottom")+
  geom_smooth(method = "lm")+
  facet_wrap(~ takenInSem, ncol=2) +
  labs(y = "Average project grade", x = "Average course grade", 
       title= "Across Campus, by semester")+
  xlim(-8, 12)+ ylim(-8,12)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        panel.grid.minor = element_line(colour = 'light grey'),
        plot.title = element_text(size=20, colour="black", face = "bold"),
        strip.text = element_text(size=15, colour="black", face = "bold"),
        axis.text.x = element_text(size=15, colour="black"), 
        axis.text.y = element_text(size=15, colour="black"), 
        axis.title.x = element_text(size=15, colour="black", face = "bold"), 
        axis.title.y = element_text(size=15, colour="black", face = "bold"),
        legend.title= element_text(size=15, colour="black", face = "bold"),
        legend.text =  element_text(size=15, colour="black"), 
        legend.position="bottom")
WbyS

#export as png
png("Bachelor grade correlation plot across Campus, wrapped by semester 1 to 8.png", height= 15, width=15, units = 'in', res= 300)
WbyS
dev.off()

#by semester AND Campus
bac1to6<- bacmeanbySem[which(bacmeanbySem$takenInSem <7),]
bac1to6$CourseLocation <- factor(bac1to6$CourseLocation, levels = c('A', 'K', 'E'))
WbySC<- bac1to6 %>%
  gather(-enrolID, -CourseLocation, -takenInSem, -gradeNum_project, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = gradeNum_project, color = var)) +
  geom_jitter(alpha = 0.4, size = 3) + geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  stat_cor(method = "pearson", show.legend = F, size= 5, fontface = "bold", label.x.npc = "right", label.y.npc = "bottom")+
  geom_smooth(method = "lm")+coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE)+
  facet_grid(takenInSem~CourseLocation) +
  labs(y = "Average project grade", x = "Average course grade", 
       title= "By Campus and semester")+
  xlim(-8, 12)+ ylim(-8,12)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        panel.grid.minor = element_line(colour = 'light grey'),
        plot.title = element_text(size=25, colour="black", face = "bold"),
        strip.text = element_text(size=20, colour="black", face = "bold"),
        axis.text.x = element_text(size=15, colour="black"), 
        axis.text.y = element_text(size=15, colour="black"), 
        axis.title.x = element_text(size=20, colour="black", face = "bold"), 
        axis.title.y = element_text(size=20, colour="black", face = "bold"),
        legend.title= element_text(size=20, colour="black", face = "bold"),
        legend.text =  element_text(size=20, colour="black"), 
        legend.position="bottom")
WbySC
#export as png
png("Bachelor grade correlation plot, wrapped by Campus and semester 1 to 6.png", height= 21, width=12, units = 'in', res= 300)
WbySC
dev.off()


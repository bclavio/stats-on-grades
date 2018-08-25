# Boxplots are the best data representation for understanding the dataset, 
# but the students might gain more from a simpler (and gamified) graph, 
# such as the radar/spider web chart with their scores in comparison to the average/median student
# and the percentile rank for each topic.




#radarchart w/ggplot2 
#load library
library(ggplot2)
library(miscTools)
library(extrafont)
library(ggrepel)
library(cowplot)
#font_import(pattern="[A/a]rial")
#loadfonts(device="win")



setwd("Z:/BNC/PBL development project/Data/analysis_data/SSP")

#create a vector with axis names
labs <- c("Campus","name","Understanding of\n Medialogy", "Time com-\n mitment", "Growth\n mindset", "Grit", "Study habits\n at AAU", "High school\n habits", "Social support\n for studying")
#use the new vector to change the column names
#colnames(dfStudentMedian)<- labs

# import percentiles
dfSPPscore <- read.csv("studentData.csv", header = T)
# calculate the median of the scores times 100 to be on the same scale as percentiles, not the percentiles
SSPmedian <- data.frame(Campus="AAL/CPH",name="Median",t(colMedians(dfSPPscore[,7:13])))
colnames(SSPmedian)<- labs

#SSPmean <- data.frame(Campus="AAL/CPH",name="Mean",t(colMeans(dfSPPscore[,7:13])*100))
#colnames(SSPmean)<- labs

dfSPPscore <- dfSPPscore[,c(3:4,7:13)]
colnames(dfSPPscore)<- labs

# median as the last row in the dataset
dfSPPscoreAddon <- rbind(dfSPPscore, SSPmedian)



#some checking
summary(dfSPPscoreAddon)
dim(dfSPPscoreAddon)
#dfStudentMedian <-NULL
# selecting current student and median
dfStudentMedian<- dfSPPscoreAddon[c(1,nrow(dfSPPscoreAddon)), 3:9]

#check
dfStudentMedian
#add rownames to identify median and individual in the plot
rownames(dfStudentMedian) <- c("You", "Median")
#convert rownames to column -needed for melt function
#Must be done AFTER setting column names, otherwise it creates problems
dfStudentMedian$ID <- rownames(dfStudentMedian)

# function to create the coordinates for the radarplot and remove outer line
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}



#define plot theme
RadarTheme<-theme(panel.background=element_blank(),
                  plot.title= element_text(size = 25,face=c("bold", "italic")),
                  plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                  text=element_text(family="serif"), aspect.ratio = 1,
                  #legend.position="bottom",legend.title=element_blank(),
                  #legend.direction="horizontal", legend.text = element_text(size = 13),
                  strip.text.x = element_text(size = rel(0.8)),
                  axis.text.x = element_text(size = 15, face = "bold"),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.line.x=element_line(size=0.5),
                  panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"),
                  line = element_blank(),
                  title = element_blank(),
                  legend.position="none",
                  panel.border=element_blank())

#melt data
dfmelt<- reshape2::melt(dfStudentMedian)
dfmelt['axis'] <- NA
#dfmelt$axis <- list(seq(0.0, 1.0, 0.20),seq(0.0, 1.0, 0.20),seq(0.0, 1.0, 0.20),seq(0.0, 1.0, 0.20),
#                    0,0,0,0,0,0,0,0,0,0)
dfmelt$axis1 <- 0.2
dfmelt$axis2 <- 0.4
dfmelt$axis3 <- 0.6
dfmelt$axis4 <- 0.8
dfmelt$axis5 <- 1.0
dfmelt$axis6 <- 0.5

#plot
p<- ggplot(dfmelt, aes(x=variable, y= value))+
  geom_text(aes(y = axis1,label = axis1)) +
  geom_text(aes(y = axis2,label = axis2)) +
  geom_text(aes(y = axis3,label = axis3)) +
  geom_text(aes(y = axis4,label = axis4)) +
  #geom_text(aes(y = axis6,label = axis6)) +
  geom_text(aes(y = axis5,label = "1.0")) +
  annotate("text", x= 0, y= 0, label = "0")+
  geom_polygon(aes(group=ID, color= ID, fill= ID), alpha = 0.4, size = 0.1, show.legend = T)+
  RadarTheme+
  xlab("")+ ylab("")+
  scale_y_continuous(limits = c(0, 1.20), breaks = seq(0, 1.0, 0.1))+
  #annotate("text", x= 0.0, y= seq(0.0, 1.0, 0.20), label = seq(0.0, 1.0, 0.20))+
  #geom_label_repel(aes(color=factor(ID), y = value,label = round(value, digits = 1))) +
  #geom_label(data=subset(dfmelt, dfmelt$ID=="Median"), aes(color=factor(ID), y = value,label = round(value, digits = 1))) +
  geom_label(data=subset(dfmelt, dfmelt$ID=="You"), aes(color=factor(ID), y = value,label = round(value, digits = 2), size=7)) +
  guides(size=FALSE) +
  coord_radar()

p2 <- add_sub(p, "Figure 3: Radar chart of the median self-ratings of all students in seven topics (red) in comparison to your score (blue).", x = 0, hjust = 0)
ggdraw(p2)

#export as jpg
jpeg("Radarplot_9.jpeg", height= 12, width=15, units = 'in', quality= 100, res= 300)
ggdraw(p2)
dev.off()










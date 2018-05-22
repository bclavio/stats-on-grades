#radarchart w/ggplot2 
#load library
library(ggplot2)
library(miscTools)

setwd("Z:/BNC/PBL development project/Data/analysis_data/SSP")


#load point data with pre-calculated median at the end
dfSPPpercentile <- read.csv("studentDataToLauraPercentile.csv", header = T)

# median as the last row in the dataset
#dfSPPpercentile[nrow(df)+1,] <-NA
#dfSPPpercentileMedian <- NULL
#dfSPPpercentileMedian <- c("Median", "None", colMedians(dfSPPpercentile[,3:9]))

#rbind(dfSPPpercentile, data.frame(rowID="Median", Campus="AAL/CPH",t(colMedians(dfSPPpercentile[,3:9]))))

#dfSPPpercentile <-rbind( dfSPPpercentile, dfSPPpercentileMedian)

  



#some checking
summary(data)
dim(data)

#try to create a suitable input, must be a data frame!
#5 starting from file with median at the end - works!
df<- data[c(1,191), 3:9]
#create a vector with axis names
labs <- c("Understanding of Medialogy", "Time commitments", "Growth mindset", "Grit", "Study habits at AAU", "High school habits", "Social support for studying")
#use the new vector to change the column names
colnames(df)<- labs
#check
df
#add rownames to identify median and individual in the plot
rownames(df) <- c("You", "Median")
#convert rownames to column -needed for melt function
#Must be done AFTER setting column names, otherwise it creates problems
df$ID <- rownames(df)
#check
df 



#function to create the coordinates for the radarplot
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") 
        "y"
    else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
}

#define plot theme
RadarTheme<-theme(panel.background=element_blank(),
                  plot.title= element_text(size = 25,face=c("bold", "italic")),
                  plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                  text=element_text(family="serif"), aspect.ratio = 1,
                  legend.position="bottom",legend.title=element_blank(),legend.direction="horizontal", legend.text = element_text(size = 13),
                  strip.text.x = element_text(size = rel(0.8)),
                  axis.text.x = element_text(size = 12, face = "bold"),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.line.x=element_line(size=0.5),
                  panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"))

#melt data
dfmelt<- reshape2::melt(df)
dfmelt

#plot
p<- ggplot(dfmelt, aes(x=variable, y= value))+
    geom_polygon(aes(group=ID, color= ID, fill= ID), alpha = 0.4, size = 1, show.legend = T)+
    RadarTheme+
    xlab("")+ ylab("")+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, 0.20))+
    annotate("text", x=0.0, y= seq(0.0, 1.0, 0.20), label = seq(0.0, 1.0, 0.20))+
    coord_radar()

#check
p

#export as jpg
jpeg("Radarplot_stud1c.jpeg", height= 12, width=12, units = 'in', quality= 100, res= 300)
p
dev.off()

setwd("~/Documents/DataScience/Course 4/Programming Assignment 2/DATA")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#-------------------------------------------------------------------------
# Quesion 3: Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point,
#            nonpoint, onroad, nonroad) variable, which of these four 
#            sources have seen decreases in emissions from 1999–2008 for 
#            Baltimore City? Which have seen increases in emissions from 
#            1999–2008? Use the ggplot2 plotting system to make a plot 
#            answer this question.   
#-------------------------------------------------------------------------
# Keep Only Baltimore City, MD (fips == "24510")
BALTIMORE <- NEI[NEI$fips=="24510",]
head(BALTIMORE)

#-------------------------------------------------------------------------------
# SOLUTION
# Generally, Non-Road, NonPoint, and On-Road have seen decreases; 
# Point appears to be trending up, with improvement shown in 2008
# On-Road has consistently produced the lowest totals
#-------------------------------------------------------------------------------

library(ggplot2)

setwd('..')
# plot3.png
png("plot3.png", width=480, height=480, res=120)


p<-ggplot(data = BALTIMORE,
       aes(x=year,
           y=Emissions,
           fill=type)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(.~type)+
  labs(x = "Year", y = "Emissions")  +
  guides(fill=FALSE)+
  scale_x_continuous(breaks=unique(BALTIMORE$year)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0))+
  ggtitle("Baltimore, MD Emissions 1999-2008
                    (by Type)")
p+geom_smooth(method = "lm",se=TRUE, aes(group=type)) 


dev.off()

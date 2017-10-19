setwd("~/Documents/DataScience/Course 4/Programming Assignment 2/DATA")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(SCC)
#------------------------------------------------------------------------
# Quesion 6: Compare emissions from motor vehicle sources in 
#            Baltimore City with emissions from motor vehicle 
#            sources in Los Angeles County, California 
#            (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city
#            has seen greater changes over time in motor vehicle 
#            emissions?
#-------------------------------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)

# Keep Only Baltimore City, MD (fips == "24510")
BALTIMORE <- NEI[NEI$fips=="24510",]
# Keep Only Los Angeles, CA (fips == "06037")
LA <- NEI[NEI$fips=="06037",]
LA$city <- "Los Angeles"
BALTIMORE$city <- "Baltimore"

TWO_CITY <-rbind(BALTIMORE,LA)
head(TWO_CITY)
# Must identify motor vehicles in the SCC dataframe
# Keep all unique lines with Vehicle in the EI.Sector field
motor_ids <- unique(grep("vehicle",SCC$EI.Sector,value=TRUE, ignore.case=TRUE))
head(motor_ids)
#Gas/Diesal; Heavy Duty/Light Duty
motor_ref <- subset(SCC,EI.Sector %in% motor_ids)
  head(motor_ref)

motor_EI <- inner_join(TWO_CITY, motor_ref) %>% select(SCC,EI.Sector,year,type,Emissions, city)
head(motor_EI)

#Aggregate Data 
Emission_Counts <- aggregate(Emissions ~ year+city, data=motor_EI, length)
Emission_Sum <-aggregate(Emissions ~ year+city, data=motor_EI, sum)
Emission_Mean<-aggregate(Emissions~ year+city, data = motor_EI,mean)
Motor_EI_Temp <- merge(Emission_Sum, Emission_Counts,
                       by=c("year","city"))
Motor_EI2 <- merge(Motor_EI_Temp,Emission_Mean,
                   by=c("year","city"))
Motor_EI2 <- rename(Motor_EI2,Emissions.Sum=Emissions.x,
                    Emissions.Count=Emissions.y,
                    Emissions.Mean=Emissions 
)
(Motor_EI2)

#Plot Overall Emissions
p<-ggplot(data = Motor_EI2,
          aes(x=year, y=Emissions.Sum,
              fill = city)) +
  facet_grid(.~city) +
  guides(fill=FALSE) +
  labs(x = "", y = "Emissions Sum")+
  scale_x_continuous(breaks=unique(Motor_EI2$year)) +
  geom_bar(stat="identity", position = "dodge")+ 
  ggtitle("Motor Vehicle Comparison: 1999-2008",
          "Baltimore, MD vs. Los Angeles, CA")

p1<-p+geom_smooth(method = "lm",se=TRUE, aes(group=city)) 

#Plot Average Emissions
p<-ggplot(data = Motor_EI2,
          aes(x=year, y=Emissions.Mean,
              fill = city)) +
  facet_grid(.~city) +
  guides(fill=FALSE) +
  labs(x = "Year", y = "Emissions Mean")+
  scale_x_continuous(breaks=unique(Motor_EI2$year)) +
  geom_bar(stat="identity", position = "dodge")

p2<-p+geom_smooth(method = "lm",se=TRUE, aes(group=city)) 

# Create PNG File
setwd('..')
png("plot6.png", width=500, height=522,res=120)
grid.arrange(p1,p2,nrow=2)
dev.off()

#----------------------------------------------------
# Solution: Over time, LA has seen greater changes in
#           Emissions per SCC, trending up to almost
#           100 per SCC in 2008.
#           However, overall emission are relatively
#           flat for LA and trend slightly upward
#           over the measurement period.
#           Baltimore emissions per SCC are near 0
#           for each measurement year.
#           Overall emissions (and average emissions)
#           for Baltimore are trending lower over time
#------------------------------------------------------
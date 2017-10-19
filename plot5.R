setwd("~/Documents/DataScience/Course 4/Programming Assignment 2/DATA")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(SCC)
#------------------------------------------------------------------------
# Quesion 5: How have emissions from motor vehicle sources changed from 
#            1999–2008 in Baltimore City?
#------------------------------------------------------------------------
# Keep Only Baltimore City, MD (fips == "24510")
BALTIMORE <- NEI[NEI$fips=="24510",]

# Must identify motor vehicles in the SCC dataframe
# Keep all unique lines with Vehicle in the EI.Sector field
motor_ids <- unique(grep("vehicle",SCC$EI.Sector,value=TRUE, ignore.case=TRUE))
head(motor_ids)
Gas/Diesal; Heavy Duty/Light Duty
motor_ref <- subset(SCC,EI.Sector %in% motor_ids):
head(motor_ref)
library(dplyr)
motor_EI <- inner_join(BALTIMORE, motor_ref) %>% select(SCC,EI.Sector,year,type,Emissions)
head(motor_EI)
#Shorten Name of Vehicle Types
motor_EI$Vehicle_Type <- motor_EI$EI.Sector
motor_EI$Vehicle_Type <-
  gsub("Mobile - On-Road Diesel Light Duty Vehicles", "Light Diesal",
       motor_EI$Vehicle_Type)

motor_EI$Vehicle_Type <-
  gsub("Mobile - On-Road Gasoline Light Duty Vehicles", "Light Gas",
       motor_EI$Vehicle_Type)

motor_EI$Vehicle_Type <-
  gsub("Mobile - On-Road Diesel Heavy Duty Vehicles", "Heavy Diesal",
       motor_EI$Vehicle_Type)

motor_EI$Vehicle_Type <-
  gsub("Mobile - On-Road Gasoline Heavy Duty Vehicles", "Heavy Gas",
       motor_EI$Vehicle_Type)

#-------------------------------------------------------------------------------
# SOLUTION
# Motor vehicle emissions have reduced from 1999-2008; 
# Especially from Heavy Duty Diesal, which have seen
# the greatest improvment amongs the four vehicle type. 
# Measurment counts in each year have generally increased.
# However, the average emmission rate has 
# decreased significantly from 1999, and have remained
# relatively stable each measure year after.
# Light Diesal shows lowest average emissions.
#-------------------------------------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)

#install.packages("gridExtra")

#Aggregate Data
Emission_Counts <- aggregate(Emissions ~ year+Vehicle_Type, data=motor_EI, length)
Emission_Sum <-aggregate(Emissions ~ year+Vehicle_Type, data=motor_EI, sum)
Emission_Mean <-aggregate(Emissions ~ year+Vehicle_Type, data=motor_EI, mean)

Motor_EI_Temp <- merge(Emission_Sum, Emission_Counts,
                   by=c("year","Vehicle_Type"))

Motor_EI2 <- merge(Motor_EI_Temp, Emission_Mean,
                       by=c("year","Vehicle_Type"))
head(Motor_EI2)


# plot5.png
p<-ggplot(data = Motor_EI2,
       aes(x=year,
           y=Emissions.x, #Sum
           fill=Vehicle_Type)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(.~Vehicle_Type)+
  labs(x = "", y = "Emissions Sum")  +
  guides(fill=FALSE)+
  scale_x_continuous(breaks=unique(Motor_EI2$year)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0))+
  ggtitle("Baltimore, MD Vehicle Emissions 1999-2008
          (by Vehicle Type)")
p1<-p+geom_smooth(method = "lm",se=TRUE, aes(group=Vehicle_Type)) 

p<-ggplot(data = Motor_EI2,
       aes(x=year,
           y=Emissions.y, #Count
           fill=Vehicle_Type)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(.~Vehicle_Type)+
  labs(x = "", y = "Emissions Count")  +
  guides(fill=FALSE)+
  scale_x_continuous(breaks=unique(Motor_EI2$year)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0))
p2<-p+geom_smooth(method = "lm",se=TRUE, aes(group=Vehicle_Type)) 


p<-ggplot(data = Motor_EI2,
       aes(x=year,
           y=Emissions, #Mean
           fill=Vehicle_Type)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(.~Vehicle_Type)+
  labs(x = "Year", y = "Emissions Mean")  +
  guides(fill=FALSE)+
  scale_x_continuous(breaks=unique(Motor_EI2$year)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0))
p3<-p+geom_smooth(method = "lm",se=TRUE, aes(group=Vehicle_Type)) 

# Create PNG File
setwd('..')
png("plot5.png", width=600, height=800,res=120)
grid.arrange(p1,p2,p3,ncol=1, nrow=3)
dev.off()



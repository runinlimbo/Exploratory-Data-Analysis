#-----------
# Get Data--
#-----------
URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
DEST <-"~/Documents/DataScience/Course 4/Programming Assignment 2/DATA"
if(!file.exists(DEST))
{dir.create(DEST)}
setwd(DEST)
download.file(URL,destfile = "PM25_Dataset.zip")
unzip(zipfile= "PM25_Dataset.zip"
      ,exdir=DEST)
list.files(DEST)
# "PM25_Dataset.zip"               "Source_Classification_Code.rds" "summarySCC_PM25.rds" 

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI)
head(SCC)


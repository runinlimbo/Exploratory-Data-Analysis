setwd("~/Documents/DataScience/Course 4/Programming Assignment 2/DATA")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#-------------------------------------------------------------------------
# Quesion 2: Have total emissions from PM2.5 decreased in the Baltimore 
#            City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008?
#            Use the base plotting system to make a plot answering this
#            question.   
#-------------------------------------------------------------------------
# Keep Only Baltimore City, MD (fips == "24510")
BALTIMORE <- NEI[NEI$fips=="24510",]
head(BALTIMORE)

#------------------
# Exploring Data --
#------------------
tapply(BALTIMORE$Emissions,BALTIMORE$year,FUN=str)
# Numeric, Approx 300 to 700 entries per year
# PM2.5 Emission count by Year
hist(BALTIMORE$year)
# Data collection has increased each year
tapply(BALTIMORE$Emissions,BALTIMORE$year,FUN=summary)
# Non-Negative Emissions
# No Missing Values
# Average appears emmissions have improved
#     1999     2002     2005     2008 
#    10.23     4.58     5.70     2.66 

# Split into vectors by Year
TT <- split(BALTIMORE$Emissions,BALTIMORE$year,drop=FALSE)
boxplot(TT)
# A few outliers, largest in 1999

boxplot(log10(TT$`1999`),log10(TT$`2002`),log10(TT$`2005`),log10(TT$`2008`))
# More Extreme Values in Later Years, greatest in 2005


#-------------------------------------------------------------------------------
# SOLUTION
# Although data collection is increasing, overall emissions by year is decreasing
# However, 2005 data shows a slight uptrend, which could be attributed towards 
# data anomolies in the year (2005 shows greatest extreme values using log boxplot)
#-------------------------------------------------------------------------------
setwd('..')
# plot2.png
SS <- tapply(BALTIMORE$Emissions,BALTIMORE$year,FUN=sum)
RR <- tapply(BALTIMORE$Emissions,BALTIMORE$year,FUN=mean)
x <-names(SS)

# Create PNG File
png("plot2.png", width=480, height=480, res=120)

# Make room in right margin for secondary axis
par(mar=c(5,4,4,6)+0.1)

# Plot first set of data and draw axis
barplot(SS/(1000), axes = FALSE, ylim=c(0,max(SS/(1000))+1),
               col = 'grey', 
               main = 'Emissions by Year',
               xlab = 'Year',
               las = 2
)
axis(side = 2,ylim=c(0,max(SS/(1000)+1)),col="black",las=1)
mtext("Total Emissions (thousands)",side=2,line=2.5)
box()

# Allow a second plot on same graph
par(new=T)

# Plot the second plot (add lines, points, and right axis)
plot(x,RR,xaxt="n", col='red',yaxt="n",xlab="",ylab="",
     ylim=c(0,max(RR)+1))
lines(x=x,y=RR, cex=1.2, col='red')
points(x=x,y=RR, pch=20, col='red')
text(x,RR,labels=round(RR,1),cex=0.5,pos=3, col = 'red')
axis(side = 4, ylim=c(0,max(RR)+1), las =1)
mtext("Average Emissions", side=4, line=2.5)

#Add Legend
legend("topright", legend=c("Sum", "Mean"),
       text.col=c("grey","red"),pch=c(15,16),
       col=c("grey","red"))
dev.off()


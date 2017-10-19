setwd("~/Documents/DataScience/Course 4/Programming Assignment 2/DATA")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#-------------------------------------------------------------------------
# Quesion 1: Have total emissions from PM2.5 decreased in the United 
#            States from 1999 to 2008? Using the base plotting system, 
#            make a plot showing the total PM2.5 emission from all 
#            sources for each of the years 1999, 2002, 2005, and 2008.
#-------------------------------------------------------------------------

#------------------
# Exploring Data --
#------------------
tapply(NEI$Emissions,NEI$year,FUN=str)
# Numeric, Approx 1M to 2M entries per year
# PM2.5 Emission count by Year
hist(NEI$year)
# Data collection has increased each year
tapply(NEI$Emissions,NEI$year,FUN=summary)
# Non-Negative Emissions
# No Missing Values
# Average appears emmissions have improved
#     1999     2002     2005     2008 
#     6.62      3.3     3.18    1.753 

# Split into vectors by Year
TT <- split(NEI$Emissions,NEI$year,drop=FALSE)
boxplot(TT)
# A few outliers, mainly in 2002

boxplot(log10(TT$`1999`),log10(TT$`2002`),log10(TT$`2005`),log10(TT$`2008`))
# More Extreme Values in Later Years, No Negative extreme values in 1999

#-------------------------------------------------------------------------------
# SOLUTION
# Although data collection is increasing, overall emissions by year is decreasing
#-------------------------------------------------------------------------------
setwd('..')
# plot1.png
SS <- tapply(NEI$Emissions,NEI$year,FUN=sum)
RR <- tapply(NEI$Emissions,NEI$year,FUN=mean)
x=names(SS)

# Create PNG File
png("plot1.png", width=480, height=480, res=120)

# Make room in right margin for secondary axis
par(mar=c(5,4,4,6)+0.1)

# Plot first set of data and draw axis
barplot(SS/(10^6), axes = FALSE, ylim=c(0,max(SS/(10^6))+1),
        col = 'grey', 
        main = 'Emissions by Year',
        xlab = 'Year',
        las = 2
)
axis(side = 2,ylim=c(0,max(SS/(10^6)+1)),col="black",las=1)
mtext("Total Emissions (millions)",side=2,line=2.5)
box()

# Allow a second plot on same graph
par(new=T)

# Plot the second plot (add lines, points, and right axis)
plot(x,RR,xaxt="n", col='red',yaxt="n",xlab="",ylab="",
     ylim=c(0,max(RR)+1))
lines(x=x,y=RR, cex=1.2, col='red')
points(x=x,y=RR, pch=20, col='red')
text(x,RR,labels=round(RR,1),cex=0.65,pos=3, col='red')
axis(side = 4, ylim=c(0,max(RR)+1), las =1)
mtext("Average Emissions", side=4, line=2.5)

#Add Legend
legend("topright", legend=c("Sum", "Mean"),
       text.col=c("grey","red"),pch=c(15,16),
       col=c("grey","red"))
dev.off()

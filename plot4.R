setwd("~/Documents/DataScience/Course 4/Programming Assignment 2/DATA")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(SCC)
#------------------------------------------------------------------------
# Quesion 4: Across the United States, how have emissions from coal 
#            combustion-related sources changed from 1999–2008?
#------------------------------------------------------------------------

# Must identify coal emissions in the SCC dataframe
coal_ids <- unique(grep("coal",SCC$EI.Sector,value=TRUE, ignore.case=TRUE))
head(coal_ids)
coal_ref <- subset(SCC,EI.Sector %in% coal_ids)
head(coal_ref)

coal_EI <- subset(NEI, SCC %in% coal_ref$SCC)

head(coal_EI)
#-------------------------------------------------------------------------------
# SOLUTION
# Overall coal emissions have reduced from 1999-2008; 
# However, the average emmission rate has remained
# relatively stable over the past three measurment periods.
#-------------------------------------------------------------------------------
setwd('..')
# plot4.png
ySum <- tapply(coal_EI$Emissions,coal_EI$year,FUN=sum)
yMean <- tapply(coal_EI$Emissions,coal_EI$year,FUN=mean)
x=names(ySum)

# Create PNG File
png("plot4.png", width=480, height=480, res=120)

# Make room in right margin for secondary axis
par(mar=c(5,4,4,6)+0.1)

# Plot first set of data and draw axis
barplot(ySum/(10^5), axes = FALSE, ylim=c(0,max(ySum/(10^5))+1),
        col = 'grey', 
        main = 'Coal Emissions by Year',
        xlab = 'Year',
        las = 2
)
axis(side = 2,ylim=c(0,max(ySum/(10^5)+1)),col="black",las=1)
mtext("Total Emissions (Ten Thousands)",side=2,line=2.5)
box()

# Allow a second plot on same graph
par(new=T)

# Plot the second plot (add lines, points, and right axis)
plot(x,yMean,xaxt="n", col='red',yaxt="n",xlab="",ylab="",
     ylim=c(0,max(yMean)+10))
lines(x=x,y=yMean, cex=1.2, col='red')
points(x=x,y=yMean, pch=20, col='red')
text(x,yMean,labels=round(yMean,1),cex=0.5,pos=3, col='red')
axis(side = 4, ylim=c(0,max(yMean)+1), las =1)
mtext("Average Emissions", side=4, line=2.5)

#Add Legend
legend("topright", legend=c("Sum", "Mean"),
       text.col=c("grey","red"),pch=c(15,16),
       col=c("grey","red"))
dev.off()

dir.create("./Data")
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "./Data/FNEI_data.zip")
download.date<-Sys.time()
unzip("./data/FNEI_data.zip",exdir="./Data")
NEI <- readRDS("./Data/summarySCC_PM25.rds")
SCC <- readRDS("./Data/Source_Classification_Code.rds")

#Is any data missing in this database?
nrow(NEI[is.na(NEI)])

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from 
#all sources for each of the years 1999, 2002, 2005, and 2008.

##PLOT1

#Pretreatment of the Data, we have to merge all the columns of the same year
PollbyYear<-aggregate(Emissions~year,data=NEI,FUN=sum)

#Plot itself correcting the X-axis to only show the relevant years
png("Plot1.png")
plot(PollbyYear[,"year"],PollbyYear[,"Emissions"],xaxt="n",
     t="l",ylab="Tons of PM2.5",xlab="Year")
    axis(side=1,at=c(1999,2002,2005,2008), 
         labels=c(1999,2002,2005,2008))
    title(main="PM2.5 Emissions in the USA")
dev.off()

#Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

##PLOT2

#Subsetting and formatting the data by year
Baltimore25<-NEI[NEI[,"fips"]=="24510",]
Baltimore25byYear<-aggregate(Emissions~year,data=Baltimore25,FUN=sum)

#Plot itself correcting the X-axis to only show the relevant years
png("Plot2.png")
plot(Baltimore25[,"year"],Baltimore25[,"Emissions"],xaxt="n",
     t="l",ylab="Tons of PM2.5",xlab="Year")
    axis(side=1,at=c(1999,2002,2005,2008), 
         labels=c(1999,2002,2005,2008))
    title(main="PM2.5 Emissions in Baltimore")

##PLOT3

#Of the four types of sources indicated by the type (point, nonpoint, onroad, 
#nonroad) variable, which of these four sources have seen decreases in emissions
#from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

#Load the required packages
require(ggplot2)

Baltimore25<-NEI[NEI[,"fips"]=="24510",]
Baltimore25<-aggregate(Emissions~type,data=Baltimore25,FUN=sum)

?ggplot2

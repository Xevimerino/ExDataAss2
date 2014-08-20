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
plot(Baltimore25byYear[,"year"],Baltimore25byYear[,"Emissions"],xaxt="n",
     t="l",ylab="Tons of PM2.5",xlab="Year")
    axis(side=1,at=c(1999,2002,2005,2008), 
         labels=c(1999,2002,2005,2008))
    title(main="PM2.5 Emissions in Baltimore")
dev.off()

##PLOT3

#Of the four types of sources indicated by the type (point, nonpoint, onroad, 
#nonroad) variable, which of these four sources have seen decreases in emissions
#from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

#Load the required packages
require("ggplot2")
require("plyr")

Baltimore25<-NEI[NEI[,"fips"]=="24510",c("Emissions","type","year")]
Baltimore25[,"type"]<-as.factor(Baltimore25[,"type"])
Baltimore25[,"year"]<-as.factor(Baltimore25[,"year"])

Baltimore26 <- ddply(Baltimore25, c("type", "year"), summarise,
               mean = mean(Emissions))

png("Plot3.png")
p <- ggplot(Baltimore26, aes(x = year, y = mean, color = type))
p + geom_line(aes(group=type))
dev.off()

##PLOT4

#Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999–2008?

#Load the required packages
require("plyr")

# Getting all the coal-combustion sources with grep from SCC dataframe
Coal<-grep("Coal",SCC[,"Short.Name"],fixed=T,useBytes=T)
CombCoal<-grep("Comb",SCC[,"Short.Name"],fixed=T,useBytes=T)
SCCCoalComb<-SCC[which(CombCoal %in% Coal),"SCC"]
NEICoalComb<-which(NEI[,"SCC"]%in%SCCCoalComb)
NEICC<-NEI[NEICoalComb,]

#Summarising by mean
NEICCSum <- ddply(NEICC, c("year"), summarise,
                     mean = mean(Emissions))

#Inspect this. Why there aren't 91 different SCC codes?
unique(NEICC[,"SCC"])

#It maybe should be sorted by source?
png("Plot4.png")
plot(NEICCSum[,"year"],NEICCSum[,"mean"],
     t="l",ylab="Tons of PM2.5",xlab="Year",xaxt="n")
axis(side=1,at=c(1999,2002,2005,2008), 
     labels=c(1999,2002,2005,2008))
title(main="Coal Combustion Emissions in the USA")
dev.off()

##PLOT5

#How have emissions from motor vehicle sources 
#changed from 1999–2008 in Baltimore City(fips == "24510")?

#Load the required packages
require("plyr")

#Selecting the city of Baltimore
Baltimore25<-NEI[NEI[,"fips"]=="24510",]

#Selecting motor vehicle sources
Vehicle<-grep("Vehicle",SCC[,"Short.Name"],fixed=T,useBytes=T)
SCCVehicle<-SCC[Vehicle,"SCC"]
NEIVehicle<-which(Baltimore25[,"SCC"]%in%SCCVehicle)
NEIBV<-Baltimore25[NEIVehicle,]

#Summarising by mean
NEIBVSum <- ddply(NEIBV, c("year"), summarise,
                  mean = mean(Emissions))

png("Plot5.png")
plot(NEIBVSum[,"year"],NEIBVSum[,"mean"],
     t="l",ylab="Tons of PM2.5",xlab="Year",xaxt="n")
axis(side=1,at=c(1999,2002,2005,2008), 
     labels=c(1999,2002,2005,2008))
title(main="Vehicle emissions in Baltimore")
dev.off()

##PLOT6

#Compare emissions from motor vehicle sources in Baltimore City with 
#emissions from motor vehicle sources in Los Angeles County, California 
#(fips == "06037"). Which city has seen greater changes over time in motor
#vehicle emissions?

#Load the required packages
require("plyr")
require("ggplot2")

#Selecting the city of Baltimore
Baltimore25<-NEI[NEI[,"fips"]=="24510",]
Baltimore25[,"city"]<-"Baltimore"
LA25<-NEI[NEI[,"fips"]=="06037",]
LA25[,"city"]<-'Los Angeles'
BaltvsLA<-rbind(Baltimore25,LA25)

#Selecting motor vehicle sources
Vehicle<-grep('Vehicle',SCC[,"Short.Name"],fixed=T,useBytes=T)
SCCVehicle<-SCC[Vehicle,"SCC"]
NEIVehicle<-which(BaltvsLA[,"SCC"]%in%SCCVehicle)
NEIBvsLA<-BaltvsLA[NEIVehicle,]

#Summarising by mean
NEIBvsLASum <- ddply(NEIBvsLA, c("year","city"), summarise,
                  mean = mean(Emissions))

png("Plot6.png")
p <- ggplot(NEIBvsLASum, aes(x = year, y = mean, color = city))
p + geom_line(aes(group=city))
dev.off()

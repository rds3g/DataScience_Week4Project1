library(dplyr)
library(lubridate)


## This first line will likely take a few seconds. Be patient!
if(! exists("NEI") ){ 
  NEI <- readRDS("summarySCC_PM25.rds")
}
if( ! exists("SCC")){
  SCC <- readRDS("Source_Classification_Code.rds") 
}

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot 
# howing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

NEI  <- tbl_df(NEI)  #make dplyr more efficient
NEI2pt5byYear <- NEI %>% group_by(year) %>% summarize(total2pt5byYear=sum(Emissions)) %>% arrange(year)
obsCountsbyYear <-tally(group_by(NEI, year)) %>% arrange(year)

# The total number of observations increase over the years, even while the total emissions decrease!
# > obsCountsbyYear
# # A tibble: 4 × 2
# year       n
# <int>   <int>
#   1  1999 1108469
# 2  2002 1698677
# 3  2005 1713850
# 4  2008 1976655

dev.set(2)
barplot(height=(NEI2pt5byYear$total2pt5byYear/1e6),names.arg=NEI2pt5byYear$year, 
        main="Total Fine Particle Emissions in US",ylab = "Emissions (Million Tons)", xlab="Year")


#
pngFileOutput <- png(filename = "plot1.png",
     width = 480, height = 480, units = "px", pointsize = 12,
     bg = "white")
barplot(height=(NEI2pt5byYear$total2pt5byYear/1e6),names.arg=NEI2pt5byYear$year, 
        main="Total Fine Particle Emissions in US",ylab = "Emissions (Million Tons)", xlab="Year")

dev.off() # close the PNG file


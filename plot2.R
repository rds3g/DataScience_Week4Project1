library(dplyr)
library(lubridate)


## This first line will likely take a few seconds. Be patient!
if(! exists("NEI") ){ 
  NEI <- readRDS("summarySCC_PM25.rds")
}
if( ! exists("SCC")){
  SCC <- readRDS("Source_Classification_Code.rds") 
}


#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
# 

NEI  <- tbl_df(NEI)  #make dplyr more efficient
NEIBaltimore <- NEI %>% filter(fips=="24510")
NEIBaltimore2pt5byYear <- NEIBaltimore %>%  group_by(year) %>% 
  summarize(total2pt5byYear=sum(Emissions)) %>% arrange(year)
BaltimoreObsCountsbyYear <-tally(group_by(NEIBaltimore, year)) %>% arrange(year)

# The total number of observations increase over the years, even while the total emissions decrease!


dev.set(2)
barplot(height=(NEIBaltimore2pt5byYear$total2pt5byYear/1e3),names.arg=NEIBaltimore2pt5byYear$year, 
        main="Total Fine Particle Emissions in City of Baltimore (Maryland)",ylab = "Emissions (Thousand Tons)", 
        xlab="Year")

#
pngFileOutput <- png(filename = "plot2.png",
     width = 480, height = 480, units = "px", pointsize = 12,
     bg = "white")

barplot(height=(NEIBaltimore2pt5byYear$total2pt5byYear/1e3),names.arg=NEIBaltimore2pt5byYear$year, 
        main="Total Fine Particle Emissions in City of Baltimore (Maryland)",ylab = "Emissions (Thousand Tons)", xlab="Year")

dev.off() # close the PNG file



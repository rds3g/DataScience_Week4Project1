library(dplyr)
library(lubridate)
library(ggplot2)


## This first line will likely take a few seconds. Be patient!
if(! exists("NEI") ){ 
  NEI <- readRDS("summarySCC_PM25.rds")
}
if( ! exists("SCC")){
  SCC <- readRDS("Source_Classification_Code.rds") 
}

# Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–200
#? Use the ggplot2 plotting system to make a plot answer this question.

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
# 
NEI  <- tbl_df(NEI)  #make dplyr more efficient
NEI <- NEI %>% mutate(year=as.factor(year))
NEIBaltimore <- NEI %>% filter(fips=="24510")
NEIBaltimore2pt5byYear <- NEIBaltimore %>%  group_by(year, type) %>% 
  summarize(total2pt5byYear=sum(Emissions)) %>% arrange(type,year)

makePlot3 <- function(){

  g1 <- ggplot(data=NEIBaltimore2pt5byYear, aes(x=year, y=total2pt5byYear, fill=year)) 
  g2 <- g1 + geom_bar(stat="identity") + facet_grid( .~ type)
  return (g2)
}

dev.set(2)
plt3 <- makePlot3()
print(plt3)

 pngFileOutput <- png(filename = "plot2.png",
      width = 480, height = 480, units = "px", pointsize = 12,
     bg = "white")
 print(plt3)
 
# barplot(height=(NEIBaltimore2pt5byYear$total2pt5byYear/1e3),names.arg=NEIBaltimore2pt5byYear$year, 
#         main="Total Fine Particle Emissions in City of Baltimore (Maryland)",ylab = "Emissions (Thousand Tons)", xlab="Year")
# 
# dev.off() # close the PNG file
# 
# 

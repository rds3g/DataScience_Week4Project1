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

NEI  <- tbl_df(NEI)  #make dplyr more efficient
SCC <- tbl_df(SCC)



NEI <- NEI %>% mutate(year=as.factor(year))
NEIBaltimore <- NEI %>% filter(fips=="24510")
NEIBaltimore2pt5byYear <- NEIBaltimore %>%  group_by(year, type) %>% 
  summarize(total2pt5byYear=sum(Emissions)) %>% arrange(type,year)

makePlot3 <- function(){

  g1 <- ggplot(data=NEIBaltimore2pt5byYear, aes(x=year, y=total2pt5byYear, fill=type)) 
  g2 <- g1 + geom_bar(stat="identity") + facet_grid( .~ type)
  g2 <- g2 + xlab("Year") + ylab("Emissions (Tons)") 
  g2 <- g2    + ggtitle("Baltimore City Fine Particulate Emissions")
  g2 <- g2 + theme(legend.position="none")
  return (g2)
}

dev.set(2)
plt3 <- makePlot3()
print(plt3)

 pngFileOutput <- png(filename = "plot3.png",
      width = 480, height = 480, units = "px", pointsize = 12,
     bg = "white")
 print(plt3)
  dev.off()
  
# barplot(height=(NEIBaltimore2pt5byYear$total2pt5byYear/1e3),names.arg=NEIBaltimore2pt5byYear$year, 
#         main="Total Fine Particle Emissions in City of Baltimore (Maryland)",ylab = "Emissions (Thousand Tons)", xlab="Year")
# 
# dev.off() # close the PNG file
# 
# 

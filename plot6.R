library(dplyr)
library(lubridate)
library(ggplot2)


if(! exists("NEI") ){ 
  NEI <- readRDS("summarySCC_PM25.rds")
}
if( ! exists("SCC")){
  SCC <- readRDS("Source_Classification_Code.rds") 
}

NEI  <- tbl_df(NEI)  #make dplyr more efficient
SCC  <- tbl_df(SCC)

NEI <- mutate(NEI, year=as.factor(year))

# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
# Which city has seen greater changes over time in motor vehicle emissions?


# I have interest in seeing how gasoline vs. diesel emissions change
# make a new column for designating Gasoline, Diesel
# then subset SCC to include only the codes of interest (for followup Join to NEI)
MVGasolineOrDiesel <- rep(NA,nrow(SCC))
match_ii <- grep("Highway Vehicles.{1,}Gasoline",SCC$SCC.Level.Two)
MVGasolineOrDiesel[match_ii]<-"Gasoline"
match_ii <- grep("Highway Vehicles.{1,}Diesel",SCC$SCC.Level.Two)
MVGasolineOrDiesel[match_ii]<-"Diesel"
SCC_MotorVehicles <- cbind(SCC,MVGasolineOrDiesel)
SCC_MotorVehicles <- filter(SCC_MotorVehicles,!is.na(MVGasolineOrDiesel))
SCC_MotorVehicles <- mutate(SCC_MotorVehicles,SCC = as.character(SCC))



# inner join on the NEI and SCC_CoalCombusionOnly tables
NEI_BaltAndLACounty <- NEI %>% filter(fips=="24510" | fips=="06037")
NEI_BaltAndLACounty <- mutate(NEI_BaltAndLACounty,year=as.factor(year))
NEI_BaltAndLACounty <- inner_join(NEI_BaltAndLACounty,SCC_MotorVehicles, by = "SCC")
Locality <- rep(NA,nrow(NEI_BaltAndLACounty))
match_Balt <- which(NEI_BaltAndLACounty$fips=="24510")
Locality[match_Balt]<-"Baltimore City"
match_LACounty <- which(NEI_BaltAndLACounty$fips=="06037")
Locality[match_LACounty]<-"Los Angeles County"
NEI_BaltAndLACounty <- cbind(NEI_BaltAndLACounty,Locality)

 NEI_BaltAndLACounty_2pt5Totals <- NEI_BaltAndLACounty %>% group_by(year, MVGasolineOrDiesel, Locality) %>%
     summarize(totalsOf2pt5Tons=sum(Emissions)) %>% arrange(year,MVGasolineOrDiesel)

names(NEI_BaltAndLACounty_2pt5Totals)[names(NEI_BaltAndLACounty_2pt5Totals)=="MVGasolineOrDiesel"] <- "Fuel Type"


# 
g1 <- ggplot(NEI_BaltAndLACounty_2pt5Totals, aes(x=year,y=totalsOf2pt5Tons,fill=`Fuel Type`)) + geom_bar(stat="identity")
g1 <- g1 + facet_grid(Locality ~ ., scales = "free")
g1 <- g1 + xlab("Year") + ylab("Emissions (Tons)") 
g1 <- g1    + ggtitle("Fine Particulate Emissions\n From Motor Vehicles by Fuel Type")
# 
print (g1)

# Now print to the png file 
pngFileOutput <- png(filename = "plot6.png",
                   width = 480, height = 480, units = "px", pointsize = 12,
                   bg = "white")

print(g1)
dev.off()


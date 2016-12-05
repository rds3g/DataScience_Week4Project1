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

# Q5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?


# Look through the SCC dataframe and find sources matching "Coal" and "Comb"
#grep("Comb.{1,}[Cc]oal",SCC$Short.Name,value=TRUE)>match_ii <- grep("Comb.{1,}[Cc]oal",SCC$Short.Name)

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
NEI_BaltOnly <- NEI %>% filter(fips=="24510")
NEI_BaltOnly <- mutate(NEI_BaltOnly,year=as.factor(year))
NEI_BaltOnly <- inner_join(NEI_BaltOnly,SCC_MotorVehicles, by = "SCC")
#NEI_BaltOnly <- mutate(NEI_BaltOnly, EmissionsInThousandsTons = Emissions)


 NEI_BaltOnly_2pt5Totals <- NEI_BaltOnly %>% group_by(year, MVGasolineOrDiesel) %>%
     summarize(totalsOf2pt5Tons=sum(Emissions)) %>% arrange(year,MVGasolineOrDiesel)

names(NEI_BaltOnly_2pt5Totals)[names(NEI_BaltOnly_2pt5Totals)=="MVGasolineOrDiesel"] <- "Fuel Type"


# 
g1 <- ggplot(NEI_BaltOnly_2pt5Totals, aes(x=year,y=totalsOf2pt5Tons,fill=`Fuel Type`)) + geom_bar(stat="identity") 
g1 <- g1 + xlab("Year") + ylab("Emissions (Tons)") 
g1 <- g1    + ggtitle("Baltimore City Fine Particulate Emissions\n From Motor Vehicles by Fuel Type")
# 
print (g1)

# Now print to the png file 
pngFileOutput <- png(filename = "plot5.png",
                   width = 480, height = 480, units = "px", pointsize = 12,
                   bg = "white")

print(g1)
dev.off()


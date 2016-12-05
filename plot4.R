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

# Q4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# Look through the SCC dataframe and find sources matching "Coal" and "Comb"
#grep("Comb.{1,}[Cc]oal",SCC$Short.Name,value=TRUE)>match_ii <- grep("Comb.{1,}[Cc]oal",SCC$Short.Name)
match_ii <- grep("Comb.{1,}[Cc]oal",SCC$Short.Name)
SCC_CConly <- SCC[match_ii,]
SCC_CConly <- mutate(SCC_CConly, SCC = as.character(SCC))
SCC_CConly <- mutate(SCC_CConly, myLevelThree = as.character(SCC.Level.Three))
# I am interested personally in seeing how the major types of coal are sub-components of
# the bigger category of Coal Combusion, lots of subcategories, but make my own
# with types Anthracite, Bituminous, and Catch All Other (Lignite, etc)
MyCoalType <- rep("Other",nrow(SCC_CConly))
match_ii <- grep("Anthracite",SCC_CConly$myLevelThree)
MyCoalType[match_ii] <- "Anthracite"
match_ii <- grep("[bB]ituminous",SCC_CConly$myLevelThree)
MyCoalType[match_ii] <- "Bituminous"
match_ii <- grep("[Ll]ignite",SCC_CConly$myLevelThree)
MyCoalType[match_ii] <- "Lignite"
SCC_CConly <- cbind(SCC_CConly, MyCoalType)
SCC_CConly <- filter(SCC_CConly, MyCoalType!="Other")

# inner join on the NEI and SCC_CoalCombusionOnly tables
NEI_CConly <- inner_join(NEI,SCC_CConly,by="SCC")
NEI_CConly <- mutate(NEI_CConly,as.factor(year))
NEI_CConly <- mutate(NEI_CConly, EmissionsInThousandsTons = Emissions/1e3)

NEI_CConly_2pt5Totals <- NEI_CConly %>% group_by(year, MyCoalType) %>%
    summarize(totalsOf2pt5ThousandTons=sum(EmissionsInThousandsTons)) %>% arrange(year,MyCoalType)

names(NEI_CConly_2pt5Totals)[names(NEI_CConly_2pt5Totals)=="MyCoalType"] <- "Coal Type"

# Others is so small that it does not even show up in the stacked bar graphs,
# take others out
#NEI_CConly_2pt5Totals <- filter(NEI_CConly_2pt5Totals, totalsOf2pt5ThousandTons > 0.001)


g1 <- ggplot(NEI_CConly_2pt5Totals, aes(x=year,y=totalsOf2pt5ThousandTons,fill=`Coal Type`)) + geom_bar(stat="identity") 
g1 <- g1 + xlab("Year") + ylab("Emissions (Thousand Tons)") 
g1 <- g1    + ggtitle("United States Fine Particulate Emissions from Coal Combustion ")

print (g1)

# Now print to the png file 
pngFileOutput <- png(filename = "plot4.png",
                    width = 480, height = 480, units = "px", pointsize = 12,
                    bg = "white")

print(g1)
dev.off()


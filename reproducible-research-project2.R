
library(plyr)
library(ggplot2)
library(reshape2)

readData <- function()
{
  dir.create("temp")
  dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  if(!file.exists("./temp/weather.bz2")) 
  {
    download.file(dataURL,"./temp/weather.bz2")
  }
  
  # check if reducedStormData variable already exists
  dataSet <- NULL
  if(file.exists("./temp/weathercache.RData"))
  {
    #load("./temp/weathercache.RData")
    #print(paste("Dataset loaded from cache. Rows:  ", nrow(dataSet)))
    #don't return dataset if it cache exists
  }
  else
  {
    dataSet <- read.csv("weather.bz2")
    print(paste("Dataset loaded from start. Rows:  ", nrow(dataSet)))
  }
  dataSet
}

reduceSize <- function()
{
  if(file.exists("./temp/weathercache.RData"))
  {
    print("Here")
    load("./temp/weathercache.RData")
    print(paste("slimSet loaded from cache. Rows:  ", nrow(slimSet)))
    return(slimSet)
  }
  dataSet$Date = as.Date(dataSet$BGN_DATE, format = "%m/%d/%Y")
  tempDS <- subset(dataSet,
                   Date > as.Date("01/01/2000",format = "%m/%d/%Y") &
                     EVTYPE != "?" & 
                     (INJURIES > 0 | FATALITIES > 0 | PROPDMG > 0 | PROPDMG > 0)
  )
  
  keepCols <- c("EVTYPE", "FATALITIES", "INJURIES", 
                "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP","Date")
  
  slimSet <- tempDS[,keepCols]
  
  
  slimSet$PropertyDamage <- mapply(fixAmount, as.numeric(slimSet$PROPDMG), slimSet$PROPDMGEXP)
  slimSet$CropDamage <- mapply(fixAmount, as.numeric(slimSet$CROPDMG), slimSet$CROPDMGEXP)
  save(slimSet, file="./temp/weathercache.RData")
  slimSet
}

fixAmount <- function(tempAmt, powerInd)
{
  amount <- NULL
  if (powerInd == "" | powerInd == " ")
  {
    amount <- tempAmt
  }
  else if (powerInd == "K")
  {
    amount <- tempAmt * 10^3
  }
  else if (powerInd == "M")
  {
    amount <- tempAmt * 10^6
  }
  else if (powerInd == "B")
  {
    amount <- tempAmt * 10^9
  }
  else 
  {
    print(paste(c("Unexpected power: ",powerInd)))
  }
  amount
}

analyzeHealth <- function()
{
  eventFatalities <- aggregate(FATALITIES ~ EVTYPE, slimSet, sum)
  eventInjuries <- aggregate(INJURIES ~ EVTYPE, slimSet, sum)
  merged <- arrange(merge(eventFatalities, eventInjuries, by.x = "EVTYPE"), desc(FATALITIES))
  print(merged[1:20,])
  melted <- melt(merged[1:20,], c("EVTYPE"))
  
  g <- ggplot(melted, aes(x = reorder(factor(EVTYPE),-value, value))) +
                geom_bar(stat="identity", aes(fill = variable)) +
                xlab("Event type") +
                ylab("Health damage") +
                ggtitle('Extreme weather effects on public health year 2000+') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
              print(g)
}

analyzeEconomy <- function()
{
  propDamage <- aggregate(PropertyDamage ~ EVTYPE, slimSet, sum)
  cropDamage <- aggregate(CropDamage ~ EVTYPE, slimSet, sum)
  mergedEco <- arrange(merge(propDamage, cropDamage, by.x = "EVTYPE"), desc(PropertyDamage))
  print(mergedEco[1:20,])
  meltedEco <- melt(mergedEco[1:20,], c("EVTYPE"))
  
  
  g <- ggplot(meltedEco, aes(x = reorder(factor(EVTYPE),-value, value))) +
                geom_bar(stat="identity", aes(fill = variable)) +
                xlab("Event type") +
                ylab("Economic damage") +
                ggtitle('Extreme weather damage to economy year 2000+') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
              print(g)
}


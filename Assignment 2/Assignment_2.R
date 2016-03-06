# Assignment_2

# Define image sizes
# img.width <- 450
# img.height <- 300
# options(RCHART_HEIGHT = img.height, RCHART_WIDTH = img.width)
# opts_chunk$set(fig.width=6, fig.height=4)

library(R.utils)
library(knitr)
library(plyr)
library(dplyr)
# library(ggplot2)

# Title 
# Author: Roland Ferrao

# Synopsis
# This report aims to analyze the impact of different weather events on public health and economy based on the storm database 
# collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) from 1950 - 2011.
# The report aims to find What weather events are the most destructive to the population's health? and  
# What weather events are the most destructive to the economy?
# From the data it was depicted that excessive heat and tornado are most harmful with respect to population health, 
# while flood, drought, and hurricane/typhoon have the greatest economic consequences.

# Stage 1: Data processing and loading
if (!file.exists("data")) {dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "./data/stormdata.csv.bz2"
download.file(fileUrl, destfile = destfile)

storm_data <- bunzip2("./data/stormdata.csv.bz2", "./data/storm_data.csv", overwrite = TRUE, remove = FALSE)
csv_storm_data <-read.csv("./data/storm_data.csv")
dim(csv_storm_data) # 902,297 *37

# subset the data with only the relevant rows
reduced_csv_storm_data <- subset(csv_storm_data, select = c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))
dim(reduced_csv_storm_data) # 902,297 * 8
colnames(reduced_csv_storm_data)
str(reduced_csv_storm_data)
# EVTYPE: Factor with 985 levels
# FATALITIES: Number
# INJURIES: Number
# PROPDMG: Number
# PROPDMGEXP: Factor with 19 levels
# CROPDMG: Number
# CROPDMGEXP: Factor with 9 levels

# Stage 2: Creating fatalities & injuries datasets
fatalities_dataset <- aggregate(FATALITIES ~ EVTYPE, FUN = sum, data = reduced_csv_storm_data) # 985 * 2
top_fatalities <- fatalities_dataset[order(fatalities_dataset$FATALITIES, decreasing=TRUE),]
top_ten_fatalities <- top_fatalities[1:10,]

injuries_dataset <- aggregate(INJURIES ~ EVTYPE, FUN = sum, data = reduced_csv_storm_data) 
top_injuries <- injuries_dataset[order(injuries_dataset$INJURIES, decreasing = TRUE),]
top_ten_injuries <- top_injuries[1:10,]

# Stage 3: Creating property damage and crop damage datasets
# Convertig the PROPDMGEXP variable to numeric
reduced_csv_storm_data$PROPDMGEXP <- as.character(reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP <- gsub("B", 10^9, reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP <- gsub("K", 1000, reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP <- gsub("H", 100, reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP <- gsub("h", 100, reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP <- gsub("M", 10^6, reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP <- gsub("m", 10^6, reduced_csv_storm_data$PROPDMGEXP)
reduced_csv_storm_data$PROPDMGEXP[reduced_csv_storm_data$PROPDMGEXP %in% c("?","-","+","1","2","3","4","5","6","7","8")]<-NA
reduced_csv_storm_data$PROPDMGEXP[reduced_csv_storm_data$PROPDMGEXP %in% c("","0")]<-0
reduced_csv_storm_data$PROPDMGEXP <- as.numeric(reduced_csv_storm_data$PROPDMGEXP)
# Aggregating the property damage by event type
reduced_csv_storm_data$PROPDMG <- reduced_csv_storm_data$PROPDMGEXP*reduced_csv_storm_data$PROPDMG
propert_damages_dataset <- aggregate(PROPDMG ~ EVTYPE,FUN = sum,reduced_csv_storm_data)
top_property_damages <- propert_damages_dataset[order(propert_damages_dataset$PROPDMG,decreasing = TRUE),]
top_ten_property_damages <- top_property_damages[1:10,]

# Convertig the CROPDMGEXP variables to numeric
reduced_csv_storm_data$CROPDMGEXP <- as.character(reduced_csv_storm_data$CROPDMGEXP)
reduced_csv_storm_data$CROPDMGEXP <- gsub("B", 10^9, reduced_csv_storm_data$CROPDMGEXP)
reduced_csv_storm_data$CROPDMGEXP <- gsub("K", 1000, reduced_csv_storm_data$CROPDMGEXP)
reduced_csv_storm_data$CROPDMGEXP <- gsub("k", 1000, reduced_csv_storm_data$CROPDMGEXP)
reduced_csv_storm_data$CROPDMGEXP <- gsub("M", 10^6, reduced_csv_storm_data$CROPDMGEXP)
reduced_csv_storm_data$CROPDMGEXP <- gsub("m", 10^6, reduced_csv_storm_data$CROPDMGEXP)
reduced_csv_storm_data$CROPDMGEXP[reduced_csv_storm_data$CROPDMGEXP %in% c("?","2")] <- NA
reduced_csv_storm_data$CROPDMGEXP[reduced_csv_storm_data$CROPDMGEXP %in% c("","0")] <- 0
reduced_csv_storm_data$CROPDMGEXP <- as.numeric(reduced_csv_storm_data$CROPDMGEXP)
# Aggregating the crop damage by event type
reduced_csv_storm_data$CROPDMG <- reduced_csv_storm_data$CROPDMGEXP*reduced_csv_storm_data$CROPDMG
crop_damages_dataset <- aggregate(CROPDMG ~ EVTYPE, FUN = sum, reduced_csv_storm_data)
top_crop_damages <- crop_damages_dataset[order(crop_damages_dataset$CROPDMG,decreasing = TRUE),]
top_ten_crop_damages <- top_crop_damages[1:10,]

# Stage 4: Results
# Plotting data to show impact on population health
par(mfrow=c(1,2),
    mar = c(12, 4, 3, 2),
    cex = 0.7)

# png("plot_1.png", width = 480, height = 600, res = 120)
# png("Top_ten_fatalities.png")
barplot(top_ten_fatalities$FATALITIES,
        las = 3, 
        names.arg = top_ten_fatalities$EVTYPE, 
        main = "Highest fatalities by Event",
        ylab = "Number of Fatalities", 
        col = "green")
# dev.off()

# png("Top_ten_injuries.png")
barplot(top_ten_injuries$INJURIES,
        las = 3, 
        names.arg = top_ten_injuries$EVTYPE, 
        main = "Highest injuries by Event",
        ylab = "Number of injuries", 
        col = "darkblue")
# dev.off()

# Plotting data to show impact on economic damage
par(mfrow=c(1,2),
    mar = c(12, 4, 3, 2),
    cex = 0.7)
    
# png("Top_ten_property_damages.png")
barplot(top_ten_property_damages$PROPDMG/(10^9), 
    las = 3, 
    names.arg = top_ten_property_damages$EVTYPE, 
    main = "Greatest property damages by Event", 
    ylab = "Damage caused ($ billions)", 
    col = "green")
# dev.off()

# png("Top_ten_crop_damages.png")
barplot(top_ten_crop_damages$CROPDMG/(10^9), 
    las = 3, 
    names.arg = top_ten_crop_damages$EVTYPE, 
    main = "Greatest crop damages by Event", 
    ylab = "Damage caused ($ billions)", 
    col = "darkblue")
# dev.off()
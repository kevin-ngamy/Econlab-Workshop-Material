library(tidyverse)
library(ggplot2)

#Read in csv dataset
HousingData <- read.csv("~/PersonalProj-Kevin/Econlab/BostonHousingData.csv")
str(HousingData)


### Data Wrangling ###
HousingData$X <- NULL #Remove useless column

#Change variables (or columns names)
colnames(HousingData)[1] <- "CrimeRate" 
colnames(HousingData)[2] <- "ResidZone" 
colnames(HousingData)[3] <- "IndustryProp" 
colnames(HousingData)[6] <- "AvgRoom" 
colnames(HousingData)[7] <- "HouseAge" 
colnames(HousingData)[8] <- "DistToCityCenter" 

#Check any missing values in the data
any(is.na(HousingData))

any(is.na(HousingData$CrimeRate))  #or by variable

#Let's create new variable to express another thing, i.e: variable to indicate if HouseAge > 40
HousingData$OldHouse <- ifelse(HousingData$HouseAge >= 40, 1, 0)

#Let's try filtering dataset based on the variable (one condition)
HouseOldData <- filter(HousingData, HouseAge >= 40) #method #1
HouseOldData <- HouseOldData[HouseOldData$HouseAge >= 40,] #method #2

#Filtering dataset on multiple conditions
#Suppose we want to filter datapoints for houses who are non-old and has high Industrial acres
summary(HousingData) #Aggregate summary
summary(HousingData$IndustryProp) #variable summary

HousingDataHighIndusAndRoom <- HousingData[HousingData$IndustryProp > 11 & HousingData$AvgRoom > 6,]

### Exploration Data Analysis ###

#Basic vizualision (histogram based)
par(mfrow = c(2, 2))
hist(HousingData$AvgRoom)
hist(HousingData$HouseAge)
hist(HousingData$DistToCityCenter)
hist(HousingData$IndustryProp)
plot(HousingData$medv)

#Cute vizualisasion using ggplot

#Suppose we want vizualise Avg room with respect to House age
RoomToAgePlot <- ggplot(HousingData, aes(x = DistToCityCenter, y = CrimeRate, color = as.factor(OldHouse))) + geom_point() + scale_y_log10()
RoomToAgePlot




















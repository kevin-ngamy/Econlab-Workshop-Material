rm(list = ls())
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
HousingData$OldHouse <- ifelse(HousingData$HouseAge >= 35, "Old", "Young")

#Let's try filtering dataset based on the variable (one condition)
HouseOldData <- filter(HousingData, HouseAge >= 35) #method #1
HouseOldData <- HouseOldData[HouseOldData$HouseAge >= 35,] #method #2

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
RoomToAgePlot <- ggplot(HousingData, aes(x = DistToCityCenter, y = CrimeRate, color = as.factor(OldHouse))) + geom_point() + 
  scale_y_log10() + scale_x_log10() + 
  ggtitle("Distance to City to Crime Rate plot") + xlab("Distance to City Center") + ylab("Crime rate") + facet_wrap(~chas)
RoomToAgePlot

RoomToAgePlot <- ggplot(HousingData, aes(x = DistToCityCenter, y = CrimeRate)) + geom_point(aes(color = AvgRoom)) + 
  scale_y_log10() + scale_x_log10() + stat_smooth(method = "lm") +
  ggtitle("Distance to City to Crime Rate plot") + xlab("Distance to City Center") + ylab("Crime rate") + facet_wrap(~ as.factor(OldHouse))
RoomToAgePlot

#histograms
AgePlot <- ggplot(HousingData, aes(x = AvgRoom, fill = as.factor(OldHouse))) + geom_histogram(binwidth = 0.5, position = "dodge") + 
  ggtitle("Average number of Rooms")
AgePlot

FacetPlot <- ggplot(HousingData, aes(x = AvgRoom, fill = as.factor(OldHouse))) + geom_histogram(binwidth = 0.5, position = "dodge") + 
  ggtitle("Average number of Rooms") + facet_wrap(~as.factor(OldHouse))
FacetPlot

#Create Correlation Matrix
#compute correlation matrix
CorMat <- cor(HousingData[,1:14], HousingData[,1:14])

#Correlation matrix viz
ggcorrplot(CorMat)
corrplot(CorMat)
ggcorrplot(CorMat, hc.order = TRUE, type = "lower", lab = TRUE)

### Statistical Analysis ###

#Running linear model
RegressionMod <- lm(medv ~ ., data = HousingData)
summary(RegressionMod)

RegressionMod <- lm(medv ~ CrimeRate + IndustryProp + chas + nox + AvgRoom + DistToCityCenter + rad + 
                      tax + ptratio + black + lstat, data = HousingData)
summary(RegressionMod)

#Model diagnostics
hist(residuals(RegressionMod))
plot(residuals(RegressionMod))

#Aggregate diagnostic plot
plot(RegressionMod)

#Analyze multicollinearity
car::vif(RegressionMod)

RegressionMod <- lm(medv ~ CrimeRate + IndustryProp + chas + nox + AvgRoom + DistToCityCenter + rad:tax + 
                      ptratio + black + lstat, data = HousingData)
summary(RegressionMod)

#Run model prediction
ModelPrediction <- predict(object = RegressionMod, newdata = HousingData, type = "response")

#Attach predictions output from the model to the main data
HousingData$PredictedMedv <- ModelPrediction

#Compare the results between prediction and observed medv value
MedvPredObsPlot <- ggplot(HousingData, aes(x = medv, y = PredictedMedv)) + geom_point() + stat_smooth(method = "lm") + ggtitle("Predicted Vs Observed")
MedvPredObsPlot









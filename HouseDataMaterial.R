rm(list = ls())
library(tidyverse)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(caTools)

#Read in csv dataset
HousingData <- read.csv("~/PersonalProj-Kevin/Econlab/BostonHousingData.csv")
str(HousingData)
#see first rows
head(HousingData)

### Data Wrangling ###
HousingData$X <- NULL #Remove useless column

#Change variables (or columns names)
colnames(HousingData)[1] <- "CrimeRate" 
colnames(HousingData)[2] <- "ResidZone" 
colnames(HousingData)[3] <- "IndustryProp" 
colnames(HousingData)[6] <- "AvgRoom" 
colnames(HousingData)[7] <- "HouseAge" 
colnames(HousingData)[8] <- "DistToCityCenter" 
colnames(HousingData)[14] <- "HousePrice"

#Check any missing values in the data
any(is.na(HousingData))

any(is.na(HousingData$CrimeRate))  #or by variable
any(is.na(HousingData$HousePrice))  #or by variable

#Let's create new variable to express another thing, i.e: variable to indicate if HouseAge > 40
HousingData$OldHouse <- ifelse(HousingData$HouseAge >= 35, "Old", "Young")

#Let's try filtering dataset based on the variable (one condition)
HouseOldData <- filter(HousingData, HouseAge >= 35) #method #1
HouseOldData <- HouseOldData[HouseOldData$HouseAge >= 35,] #method #2

#Filtering dataset on multiple conditions
#Suppose we want to filter datapoints for houses who are non-old and has high Industrial acres
summary(HousingData) #Aggregate summary
summary(HousingData$HousePrice) #variable summary
summary(HousingData$IndustryProp) #variable summary

#let's create another new variable in the data using Dplyr
HousingData <- HousingData %>%
  mutate(PriceCategory = ifelse(HousePrice > 21.20, "Expensive", "Cheap"))

#See the price category ratio
table(HousingData$PriceCategory)


### Exploration Data Analysis ###

#Basic vizualision (histogram based)
par(mfrow = c(2, 2))
hist(HousingData$AvgRoom)
hist(HousingData$HouseAge)
hist(HousingData$DistToCityCenter)
hist(HousingData$HousePrice)

#Cute vizualisasion using ggplot

#Suppose we want vizualise Avg room with respect to House age
RoomToAgePlot <- ggplot(HousingData, aes(x = DistToCityCenter, y = CrimeRate, color = as.factor(OldHouse))) + geom_point() + 
  scale_y_log10() + scale_x_log10() + 
  ggtitle("Distance to City to Crime Rate plot") + xlab("Distance to City Center") + ylab("Crime rate")
RoomToAgePlot

RoomToAgePlot <- ggplot(HousingData, aes(x = DistToCityCenter, y = CrimeRate)) + geom_point(aes(color = AvgRoom)) + 
  scale_y_log10() + scale_x_log10() + stat_smooth(method = "lm") +
  ggtitle("Distance to City to Crime Rate plot") + xlab("Distance to City Center") + ylab("Crime rate") + facet_wrap(~ as.factor(OldHouse))
RoomToAgePlot

#histograms
AgePlot <- ggplot(HousingData, aes(x = AvgRoom, fill = as.factor(OldHouse))) + geom_histogram(binwidth = 0.5, position = "dodge") + 
  ggtitle("Average number of Rooms")
AgePlot

PriceAgePlot <- ggplot(HousingData, aes(x = HouseAge, y = HousePrice, color = as.factor(OldHouse))) + geom_point() +
  scale_y_log10() + ggtitle("Average number of Rooms")
PriceAgePlot

#Create Correlation Matrix
#compute correlation matrix
CorMat <- cor(HousingData[,1:14], HousingData[,1:14])

#Correlation matrix viz
ggcorrplot(CorMat)
corrplot(CorMat)
ggcorrplot(CorMat, hc.order = TRUE, type = "lower", lab = TRUE)

### Statistical Analysis ###

#Split dataset into Training dataset and Testing dataset!
SampleToSplit <- sample.split(HousingData$HousePrice, SplitRatio = 0.70)
HousingTrainData <- subset(HousingData, SampleToSplit == T) #this partition is for TRAINING the model!
HousingTestData <- subset(HousingData, SampleToSplit == F) #this partition is for TESTING the model!

#Running linear model
RegressionMod <- lm(HousePrice ~ ., data = HousingTrainData)
summary(RegressionMod)

RegressionMod <- lm(HousePrice ~ CrimeRate + chas + nox + AvgRoom + DistToCityCenter + rad + 
                      tax + ptratio + black + lstat, data = HousingTrainData)
summary(RegressionMod)

#Model diagnostics
hist(residuals(RegressionMod))
plot(residuals(RegressionMod))

#Aggregate diagnostic plot
plot(RegressionMod)

#Analyze multicollinearity
car::vif(RegressionMod)

#Final regression model
RegressionMod <- lm(HousePrice ~ CrimeRate + chas + nox + AvgRoom + DistToCityCenter + rad:tax + 
                      ptratio + black + lstat, data = HousingTrainData)
summary(RegressionMod)

### Alright! at this point we've trained our regression model, what's next? TEST THE MODEL WITH UNSEEN DATA aka TESTING DATASET ###
str(HousingTestData) #check the testing data first!

#Run model prediction (this is the defining moment!!)
HousingTestData$PredictedHousePrice <- predict(object = RegressionMod, newdata = HousingTestData[-14], type = "response") 

#Round to 2 decimal points for easy review
HousingTestData$PredictedHousePrice <- round(HousingTestData$PredictedHousePrice, 2)

#Compare the results between prediction and observed HousePrice value
HousePricePredObsPlot <- ggplot(HousingTestData, aes(x = HousePrice, y = PredictedHousePrice)) + geom_point() + stat_smooth(method = "lm") + ggtitle("Predicted Vs Observed")
HousePricePredObsPlot

#FINALLY, let's compute the model's accuracy using Root Mean Squared Error (RMSE)

Rmse_diag <- sqrt(mean((HousingTestData$PredictedHousePrice - HousingTestData$HousePrice)^2))
Rmse_diag # Summary: On average, our model's prediction is different by USD 5.3k from the actual price!

#congratulations you're now the next data scientist!!!

#Feel free to contact me for questions or anything related to R
# Email: kevintongam98@gmail.com


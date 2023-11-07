# About this file
#1. longitude: A measure of how far west a house is; a higher value is farther west
#2. latitude: A measure of how far north a house is; a higher value is farther north
#3. housingMedianAge: Median age of a house within a block; a lower number is a newer building
#4. totalRooms: Total number of rooms within a block
#5. totalBedrooms: Total number of bedrooms within a block
#6. population: Total number of people residing within a block
#7. households: Total number of households, a group of people residing within a home unit, for a block
#8. medianIncome: Median income for households within a block of houses (measured in tens of thousands of US Dollars)
#9. medianHouseValue: Median house value for households within a block (measured in US Dollars)
#10. oceanProximity: Location of the house w.r.t ocean/sea

# Simple linear regression on California Housing
library(tidyverse)
ds =read.csv('housing.csv')
length(ds)
names(ds)
head(ds)
unique(ds$total_rooms)
unique(ds$total_bedrooms)
unique(ds$ocean_proximity)
unique(ds$longitude)
unique(ds$latitude)
summary(ds[sapply(ds, is.numeric)])

#explore 
head(ds)
glimpse(ds)
names(ds)
summary(ds)

#Missing data?
colSums(is.na(ds))

#bedrooms
ggplot(data=ds,aes(total_bedrooms))+geom_density()

#impute the missing value using median
bedrooms_median=median(ds$total_bedrooms,na.rm=TRUE)
ds$total_bedrooms=ifelse(is.na(ds$total_bedrooms),
                bedrooms_median,
                ds$total_bedrooms)

colSums(is.na(ds))

#Q1. Identify blocks of houses of median_house_value above 350000
ds1=ds %>% 
  filter(median_house_value>350000) %>% 
  group_by(longitude, latitude, ocean_proximity) %>% 
  # create new columns to make graphs of them
  summarise(count=n(),
            median_households=round(median(households)),
            median_population=round(median(population)),
            median_total_bedrooms=round(median(total_bedrooms)),
            median_total_rooms=round(median(total_rooms)))

head(ds1)
unique(ds1$ocean_proximity)
unique(ds1$longitude)
unique(ds1$latitude)

#Q2. Identify blocks of houses of median_house_value>500000 and median_income>8
ds2=ds %>% 
  filter(median_house_value>500000,median_income>8) %>% 
  group_by(longitude, latitude, ocean_proximity) %>% 
  # create new columns to make graphs of them
  summarise(count=n(),
            median_households=round(median(households)),
            median_population=round(median(population)),
            median_total_bedrooms=round(median(total_bedrooms)),
            median_total_rooms=round(median(total_rooms)))

head(ds2)
unique(ds2$ocean_proximity)
unique(ds2$longitude)
unique(ds2$latitude)

#Median households
ds2 %>% 
  ggplot(aes(x=ocean_proximity,y=median_households, color=ocean_proximity,fill=ocean_proximity))+ geom_col(position='dodge')+
  labs(y='median households')

#Median population
ds2 %>% 
  ggplot(aes(x=ocean_proximity,y=median_population, color=ocean_proximity,fill=ocean_proximity))+ geom_col(position='dodge')+
  labs(y='median population')

#Median total bedrooms
ds2 %>% 
  ggplot(aes(x=ocean_proximity,y=median_total_bedrooms, color=ocean_proximity,fill=ocean_proximity))+ geom_col(position='dodge')+
  labs(y='#Median total bedrooms')

#Median total rooms
ds2 %>% 
  ggplot(aes(x=ocean_proximity,y=median_total_rooms, color=ocean_proximity,fill=ocean_proximity))+ geom_col(position='dodge')+
  labs(y='median total rooms')

#MLR of median_house_value
names(ds)
dataset=ds[sapply(ds,is.numeric)]

names(dataset)
library(caTools)
split=sample.split(dataset$median_house_value,SplitRatio=.80)

training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
MLR=lm(formula=median_house_value~.,
       data=training_set)
summary(MLR)
#prediction on test set (median_house_value)
y_pred=predict(MLR,newdata=test_set)
library(Metrics)

rmse(test_set$median_house_value, y_pred)
mae(test_set$pmedian_house_value, y_pred)

saved=data.frame(test_set$median_house_value, y_pred)
head(saved)

#MLR of median_income
names(ds)
dataset=ds[sapply(ds,is.numeric)]

names(dataset)
library(caTools)
split=sample.split(dataset$median_income,SplitRatio=.80)

training_set=subset(dataset,split==TRUE)
test_set1=subset(dataset,split==FALSE)
MLR1=lm(formula=median_income~.,
       data=training_set)
summary(MLR1)
#prediction on test set (median_income)
y_pred=predict(MLR,newdata=test_set)
library(Metrics)

rmse(test_set1$median_income, y_pred)
mae(test_set1$median_income, y_pred)

saved1=data.frame(test_set1$median_income, y_pred)
head(saved1)
summary(ds[sapply(ds,is.numeric)])

#plot the test results based on median house value
ggplot()+geom_point(aes(x=test_set$total_rooms,y=test_set$median_house_value),color='darkgreen')+
  geom_line(aes(x=test_set$total_rooms,y=y_pred),color='darkred')+
  xlab('total rooms')+ylab('median house value')

#plot the test results based on median house value
ggplot()+geom_point(aes(x=test_set$total_bedrooms,y=test_set$median_house_value),color='darkgreen')+
  geom_line(aes(x=test_set$total_bedrooms,y=y_pred),color='darkred')+
  xlab('total bedrooms')+ylab('median house value')

#plot the test results based on median house value
ggplot()+geom_point(aes(x=test_set$population,y=test_set$median_house_value),color='darkgreen')+
  geom_line(aes(x=test_set$population,y=y_pred),color='darkred')+
  xlab('population')+ylab('median house value')

# plot median house value based on longitude and latitude

#plot the test results based on median income
ggplot()+geom_point(aes(x=test_set$total_rooms,y=test_set$median_house_value),color='darkgreen')+
  geom_line(aes(x=test_set$total_rooms,y=y_pred),color='darkred')+
  xlab('total rooms')+ylab('median house value')

#plot the test results based on median income
ggplot()+geom_point(aes(x=test_set$total_bedrooms,y=test_set$median_house_value),color='darkgreen')+
  geom_line(aes(x=test_set$total_bedrooms,y=y_pred),color='darkred')+
  xlab('total bedrooms')+ylab('median house value')

#plot the test results based on median income
ggplot()+geom_point(aes(x=test_set$population,y=test_set$median_house_value),color='darkgreen')+
  geom_line(aes(x=test_set$population,y=y_pred),color='darkred')+
  xlab('population')+ylab('median house value')


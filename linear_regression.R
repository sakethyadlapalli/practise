#Bike Rental Project 
#SET he working directory
setwd("C:\\Users\\Saketh\\Documents\\rscripts\\bike-sharing-demand")
getwd()

#install packages required
install.packages("dplyr")
install.packages("ggpplot2")
install.packages("ggthemes")
install.packages("corrgram")
install.packages("corrplot")
install.packages("caTools")


#loading the data given for training (The data given is already divided into train and test data)

df_train <- read.csv("train.csv")
head(df_train)
str(df_train)

#Coverting the required columns to Factors(Season, Holiday, Woring day, Weather)

df_train$season <- as.factor(df_train$season)
df_train$holiday <- as.factor(df_train$holiday)
df_train$workingday <- as.factor(df_train$workingday)
df_train$weather <- as.factor((df_train$weather))

str(df_train)

# Understanding the data (EDA)
library(ggplot2)
graph_1 <- ggplot(df_train,aes( x = temp, y = count)) + geom_point(aes(color = temp) , alpha = 0.2)
print(graph_1) +ggtitle("Bike Rentals vs Temparature")

#converting datetime 
df_train$datetime <- as.POSIXct(df_train$datetime)

graph_2 <- ggplot(df_train, aes(datetime,count)) + geom_point(aes(color=temp), alpha= 0.3) + scale_color_continuous(low= "blue", high= "red") + ggtitle("Bike Rentals Variation with seasons")
print(graph_2) 

#finding the correlationrrelation btwn temp and count

print(cor(df_train[,c("temp","count")]))

graph_3 <- ggplot(df_train, aes(x= season , y = count)) + geom_boxplot(aes(color=season)) + ggtitle("Season vs Count")

#with timestamps 
library(dplyr)

#crating a new column hour
df_train$hour <- sapply(df_train$datetime,function(x){format(x,"%H")})

graph_4 <- ggplot(filter(df_train, workingday== 1), aes(hour,count)) + geom_point(aes(color= temp), position=position_jitter(w=1, h=0),alpha=0.3) + scale_color_continuous(low= "green", high="red") +ggtitle("working days rentals")

graph_5 <- ggplot(filter(df_train, workingday== 0), aes(hour,count)) +  geom_point(aes(color= temp),position=position_jitter(w=1, h=0), alpha=0.3) + scale_color_continuous(low= "green", high="red") + ggtitle("non working days rentals")


#Linear Regression

train_model <- lm(count~temp , df_train) 







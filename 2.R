#We open the readr package and the csv file we'll work on --> with import data set 
#it's done automatically
library(readr)
library(ggplot2)
cars <- read_csv("C:/Users/user/Downloads/R Tutorial Data/R Tutorial Data Sets/cars.csv")

#data understanding

#We have a 1st overlook on the data to get to know it
summary(cars)

#We'll rework data to visualize it properly:
#first of all we'll rename columns to save time when coding:
names(cars) <- c("name","speed","distance") 

#now we'll get to know how each variable is distributed:
#used table and barplot to do a histogram of a categorical variable

table(cars$name)
barplot(table(cars$name))

#used breaks for the no of columns
hist(cars$speed, breaks = 25)
hist(cars$distance, breaks = 120)
boxplot(cars$speed, cars$distance)
boxplot(cars$distance)

#preprocessing

#how to spot and extract and outlier
which(cars$speed < 5)
box <- boxplot(cars$distance)
which(cars$distance == box$out)

cars[c(which(cars$speed < 5), which(cars$distance == box$out)), ]
wo_outlier <- cars[-c(which(cars$speed < 5), which(cars$distance == box$out)), ]
wo_outlier


#now we'll plot both combined to see the relationship between them, with and without
#the outlier

ggplot(cars, aes(x = cars$speed, y=cars$distance)) + geom_point() + geom_smooth(method = lm, se = F) + xlim(0, 25) + ylim(0, 120)
ggplot(wo_outlier, aes(x = wo_outlier$speed, y = wo_outlier$distance)) + geom_point() + geom_smooth(method = lm, se = F) + xlim(0, 25) + ylim(0, 120)


#now we have the 2 datasets we want to use for the modeling, we'll start with a 
#linear regression to see how it performs:

#for cars

set.seed(123)
car_trainSize <- round(nrow(cars) * 0.7) 
car_testSize <- nrow(cars) -car_trainSize
car_training_indices <- sample(seq_len(nrow(cars)), size = car_trainSize)
car_trainSet <- [car_training_indices,]
car_testSet <- cars[-car_training_indices,] 
car_model <- lm(distance~ speed, car_trainSet)
summary(car_model)
car_Prediction <- predict(car_model, car_testSet)
car_Prediction

#for cars wo outliers

set.seed(123)
wo_trainSize <- round(nrow(wo_outlier) * 0.7) 
wo_testSize <- nrow(wo_outlier) - wo_trainSize
wo_training_indices <- sample(seq_len(nrow(wo_outlier)), size = wo_trainSize)
wo_trainSet <- wo_outlier[wo_training_indices, ]
wo_testSet <- wo_outlier[-wo_training_indices, ] 
wo_model <- lm(distance~speed, wo_trainSet)
summary(wo_model)
wo_Prediction <- predict(wo_model, wo_testSet)
wo_Prediction


#now we'll have a look at both models' performances:
summary(car_model)
summary(wo_model)
cars_wprediction<-cbind(car_testSet, car_Prediction)
car_error <- (cars_wprediction$distance-cars_wprediction$car_Prediction)
cars_werror <- cbind(cars_wprediction, car_error)
wo_wprediction <- cbind(wo_testSet, wo_Prediction)
wo_error <- (wo_wprediction$distance-wo_wprediction$wo_Prediction)
wo_werror <- cbind(wo_wprediction, wo_error)
sum(abs(car_error)) / 15
sum(abs(wo_error)) / 14


sum(sqrt((car_error^2) / 15))
sum(sqrt((wo_error^2) / 14))

ggplot(cars_wprediction, aes(x = cars_wprediction$distance, y = cars_wprediction$car_Prediction))+ geom_point()+ xlim(0, 100) + ylim(0, 100)+ geom_abline()
ggplot(wo_wprediction, aes(x = wo_wprediction$distance, y = wo_wprediction$wo_Prediction))+ geom_point()+ xlim(0, 100) + ylim(0, 100)+ geom_abline()


#Now let's test it with a quadratic function:

speed2 <- car_trainSet$speed^2
car_model2 <- lm(distance ~ speed2, car_trainSet)
summary(car_model2)
car_Prediction2 <- predict(car_model2, car_trainSet)
car_Prediction2

car2_error <- (car_trainSet$distance-car_Prediction2)
tabla_final <- cbind(car_trainSet, car_Prediction2, car2_error)
sum(abs(car2_error)) / 35
sum(sqrt((car2_error ^ 2) / 35))

ggplot(tabla_final,aes(x = tabla_final$speed,y = tabla_final$car_Prediction2)) + geom_point()+ xlim(0, 25) + ylim(0, 125) 
ggplot(tabla_final,aes(x = tabla_final$distance,y = tabla_final$car_Prediction2)) + geom_point() + geom_abline()



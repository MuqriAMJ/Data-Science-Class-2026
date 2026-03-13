#Multiple Linear Regression
#1. Mtcars Dataset
data(mtcars)
head(mtcars)
str(mtcars)

#model the MLR
model <- lm(mpg ~ hp + wt + cyl, data = mtcars)

#invesitigate the properties of the model
summary(model)

#split data into train and test sets
data.train<- mtcars[1:22,]
data.test<- mtcars[23:32,]

#modelling
relation <-lm(mpg ~ hp +wt+cyl, data = data.train)
summary(relation)

# Prediction
a <- data.frame(hp = data.test$hp, wt = data.test$wt, cyl = data.test$cyl)
result <- predict(relation, a)
print(round(result, digits = 2))

#Performance Measurement
mape <- mean(abs((data.test$mpg - result)/ data.test$mpg )*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#2. Class Activity
#Multiple Linear Regression
#Ozone Dataset
data(airquality)
head(airquality)
str(airquality)

#Remove NA values
data1 <- na.omit(airquality)

#Model the MLR
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = data1)

#Investigate the properties of the model
summary(model)

#Split data into train and test sets (70% train, 30% test)
set.seed(123)
train_index <- sample(1:nrow(data1), size = 0.7 * nrow(data1))
data.train <- data1[train_index, ]
data.test <- data1[-train_index, ]

#Modelling
relation <- lm(Ozone ~ Solar.R + Wind + Temp, data = data.train)
summary(relation)

#Prediction
a <- data.frame(
  Solar.R = data.test$Solar.R,
  Wind = data.test$Wind,
  Temp = data.test$Temp
)

result <- predict(relation, a)
print(round(result, digits = 2))

#Performance Measurement
mape <- mean(abs((data.test$Ozone - result)/data.test$Ozone)*100)

paste("The error - MAPE is:", round(mape, digits = 2), "%")
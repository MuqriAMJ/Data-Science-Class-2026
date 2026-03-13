#Simple Linear Regression
#1
#data of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

#data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function
relation <- lm(y~x)
print(relation)

#2
# Find weight of a person with height 170
X_test <- data.frame(x = 170)
result <- predict(relation,X_test)
print(round(result, digit=2))

# Find weight of a person with height 189
X_test <- data.frame(x = 189)
result <- predict(relation,X_test)
print(round(result, digit=2))

#3. Plot Graph
plot(x, y,
     col = "blue",
     main = "Height & Weight Regression",
     pch = 16,
     xlab = "Height in cm",
     ylab = "Weight in Kg")

abline(lm(y ~ x))

#4. Training, Testing & Performance Analysis
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

#data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

#Creating data frame
data1= data.frame(x,y)

#splitting data into training and testing
data1_train<-data1[1:7,]
data1_test<-data1[8:10,]

#Apply the lm() function
relation <- lm(y~x, data1_train)
print(relation)

#Make prediction
x_text <- data.frame(x= data1_test$x)
result <- predict(relation,x_text)
print(result)

#Performance measurement [Option 1]
mape <- mean(abs((data1_test$y -result)/data1_test$y)*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#Performance measurement [Option 2]
actuals_preds <- data.frame(cbind(actuals=data1_test$y, predicteds=result)
mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicteds )/ actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")
                            
#Case study: Predicting Income Happiness
df <- read_excel("C:/Users/User/OneDrive/Desktop/income_happiness.xlsx")
View(df)
                            
#Split data into training (80%) and testing (20%) sets
#Randomly select row indices for training
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]
                            
# Apply the lm() function
relation <- lm(happiness~income, data=train_data)
print(relation)
                            
# Prediction
a <- data.frame(x=test_data$income)
colnames(a) <- "income"
result <- predict(relation,a)
                            
#Plot
plot(test_data$income, test_data$happiness,
col = "red",
pch = 16,
xlab = "income",
ylab = "happiness")
                            
abline(lm(happiness ~ income, data = train_data))
                            
#5. Class Activity
experience <- c(1,2,3,4,5,6,7,8,9,10)
salary <- c(2500,2700,3000,3400,3900,4400,5000,5600,6200,6900)
                            
# Create dataframe
data1 <- data.frame(experience, salary)
                            
# Split data (70% train, 30% test)
set.seed(123)   # for reproducibility
train_indices <- sample(1:nrow(data1), size = 0.7*nrow(data1))
train_data <- data1[train_indices, ]
test_data <- data1[-train_indices, ]
                            
# Build Linear Regression Model
relation <- lm(salary ~ experience, data=train_data)
print(relation)
                            
# Prediction
result <- predict(relation, test_data)
print(result)
                            
# Scatter Plot with Regression Line
plot(data1$experience, data1$salary,
col = "blue",
pch = 16,
main = "Experience vs Monthly Salary Regression",
xlab = "Years of Experience",
ylab = "Monthly Salary (RM)")
                            
abline(relation, col="red", lwd=2)
                            
# Performance Measurement (MAPE)
mape <- mean(abs((test_data$salary - result)/test_data$salary)*100)
paste("The error - MAPE is:", round(mape,2), "%")
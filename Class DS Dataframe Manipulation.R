#intro 
#Student_data
library(dplyr)
library(readr)
library(readxl)
library(readxl)
student_data <- read_excel("C:/Users/User/OneDrive/Desktop/student_data.xlsx")
View(student_data)
summary(student_data)
tail(student_data)

#filter
#1 student_fail
Student_fail<- student_data %>% filter(final_exam_mark< 40)
View(Student_fail)

#2 Arrange student data
mydata<- student_data%>% filter(final_exam_mark > 40) %>% arrange(final_exam_mark)
View(mydata)

#3 Select
mydata<- student_data%>% filter(final_exam_mark > 40) %>% arrange(final_exam_mark)
View(mydata)
glimpse(mydata)

#4 Mutate
mydata2 = student_data%>% mutate(Total_Mark=(coursework_mark + final_exam_mark/200*100))
View(mydata2)

mydata3 <- cbind(student_data , Total_Mark = (student_data $coursework_mark + student_data$final_exam_mark/200*100))
View(mydata3)

#Descriptive Analysis
data <- iris
View(iris)
str(iris)
summary(iris)

A<-c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort(A)
IQR(A) #iqr = iq - 3q

#Histogram
hist(iris$Sepal.Length, main = "Histogram of Sepal Length", xlab = "Sepal Length (cm)", ylab = "Frequency", col = "lightblue", border = "black")

#Boxplot
boxplot(Sepal.Length ~ Species, data = iris, main = "Sepal Length by Species", xlab = "Species", ylab = "Sepal Length (cm)", col = c("lightgreen", "lightpink", "lightyellow"))

#Scatter Plot
plot(iris$Sepal.Length, iris$Petal.Length, main = "Sepal Length vs Petal Length", xlab = "Sepal Length (cm)", ylab = "Petal Length(cm)", col = as.numeric(iris$Species), pch = 19)
legend ("topleft", legend = levels(iris$Species), col = 1:3, pch = 19)

#dfplayers
median_age <- median(dfplayers$Age, na.rm =TRUE)
dfplayers$Age[dfplayers$Age<18 | dfplayers$Age>38]<-median_age
View(dfplayers)

#Outliers
data2<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data2, main = "Boxplot")

#Handling Outliers
#drop the values
first_q<-quantile(data2,0.25)
third_q<-quantile(data2,0.75)
iqr<-IQR(data2)
le<-first_q - 1.5 * iqr
ue<-third_q + 1.5 * iqr
data_new<-data2
data_new <- data_new[!data_new<le]
data_new <- data_new[!data_new>ue]
data_new
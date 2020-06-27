setwd("D:\\excelr_DS\\assignment\\Support vector machines")

salary<- read.csv(file.choose()) #salarydata_test.csv
View(salary)
colnames(salary)
str(salary)
salary$educationno <- as.factor(salary$educationno)
#salary$age <-as.factor(salary$age)
#salary$capitalgain <- as.factor(salary$capitalgain)
#salary$capitalloss <- as.factor(salary$capitalloss)
#salary$hoursperweek <- as.factor(salary$hoursperweek)
class(salary)
summary(salary)

salary_data <- read.csv(file.choose()) #salary_data_train.csv
View(salary_data)
colnames(salary_data)
str(salary_data)
salary_data$educationno <- as.factor(salary_data$educationno)
#salary_data$age <-as.factor(salary_data$age)
#salary_data$capitalgain <- as.factor(salary_data$capitalgain)
#salary_data$capitalloss <- as.factor(salary_data$capitalloss)
#salary_data$hoursperweek <- as.factor(salary_data$hoursperweek)
summary(salary_data)
class(salary_data)

salary_train <- salary_data

salary_test<-salary
str(salary_test)


# Building model 

library(kernlab)
library(caret)
model1<-ksvm(salary_train$Salary ~.,data = salary_train,kernel = "vanilladot")
model1

?ksvm

# kernel = rfdot 
model_rfdot<-ksvm(salary_train$Salary ~.,data = salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary) #85.20

# kernel = vanilladot
model_vanilla<-ksvm(salary_train$Salary ~.,data = salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary) # 84.64


# kernal = besseldot
model_besseldot<-ksvm(salary_train$Salary ~.,data = salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=salary_test)
mean(pred_bessel==salary_test$Salary) #78.97

# kernel = polydot

model_poly<-ksvm(salary_train$Salary ~.,data = salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = salary_test)
mean(pred_poly==salary_test$Salary) # 84.61


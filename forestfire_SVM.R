
setwd("D:\\excelr_DS\\assignment\\Support vector machines")

forest_fire<- read.csv(file.choose())
View(forest_fire)
colnames(forest_fire)
str(forest_fire)

#match(forest_fire$month, month.abb)
x <- sapply(forest_fire$month,function(x) grep(paste("(?i)",x,sep=""),month.abb))
forest_fire$month <- x

y <- factor(forest_fire$day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"),ordered = TRUE)
forest_fire$day <- as.integer(y)

View(forest_fire)
str(forest_fire)

forest_fire$FFMC <- as.integer(forest_fire$FFMC)
forest_fire$DMC <- as.integer(forest_fire$DMC)
forest_fire$DC <- as.integer(forest_fire$DC)
forest_fire$ISI <- as.integer(forest_fire$ISI)
forest_fire$temp <- as.integer(forest_fire$temp)
forest_fire$wind <- as.integer(forest_fire$wind)
forest_fire$rain <- as.integer(forest_fire$rain)
forest_fire$area <- as.integer(forest_fire$area)

str(forest_fire)

forest_train <- forest_fire[1:300,]
View(forest_train)

forest_test <- forest_fire[301:517,]
View(forest_test)

##z<- forest_fire$size_category
##z <- ifelse(z=="small", "S" , "L")

##forest_fire$size_category <- z

summary(forest_fire)
# column 10 to column 30 has median zero hence omitted
forest_train <- forest_train[,-c(10:30)]
forest_test <- forest_test[,-c(10:30)]

library(kernlab)
library(caret)
model1<-ksvm(size_category ~.,data = forest_train,kernel = "vanilladot")
model1

# kernel = rfdot 
model_rfdot<-ksvm(size_category ~.,data = forest_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forest_test)
mean(pred_rfdot==forest_test$size_category) # 70.50

# kernel = vanilladot
model_vanilla<-ksvm(size_category ~.,data = forest_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=forest_test)
mean(pred_vanilla==forest_test$size_category) # 70.04


# kernal = besseldot
model_besseldot<-ksvm(size_category ~.,data = forest_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forest_test)
mean(pred_bessel==forest_test$size_category) #70.50

# kernel = polydot

model_poly<-ksvm(size_category ~.,data = forest_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = forest_test)
mean(pred_poly==forest_test$size_category) #70.04

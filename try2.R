#install.packages('RTextTools', dependencies = TRUE)
library(caTools)
library(e1071)
library(SparseM)
library(RTextTools)
library(caret)
library(ggplot2)

mydata <- read.csv(file = "Datafiniti_Hotel_Reviews.csv", header = TRUE, sep = ",")
View(mydata)
totalRecord <- nrow(mydata)

split_data <- sample.split(mydata,SplitRatio = 0.7)
train <- subset(mydata,split_data==TRUE)
test <- subset(mydata,split_data==FALSE)
View(train)
trainSize = nrow(train)
View(test)
testSize = nrow(test)

dtMatrix <- create_matrix(train$reviews.text)
View(dtMatrix)

container <- create_container(dtMatrix, train$primaryCategories, trainSize=1:trainSize, virgin=FALSE)
View(container)

testMatrix <- create_matrix(test$reviews.text, originalMatrix=dtMatrix)
#trace("create_matrix",edit=T)
testContainer <- create_container(testMatrix, labels=rep(0,testSize), testSize=1:testSize, virgin=FALSE)

model <- train_model(container, "SVM", kernel="linear", cost=1, scale=FALSE)
View(model)

pred <- classify_model(testContainer, model)
View(pred)

output <- cbind(test$reviews.text,test$primaryCategories,pred)
View(output)

sumry<- table(output$`test$primaryCategories`, output$SVM_LABEL, dnn = c("A","P"))
View(sumry)
sumry

checkAccuracyFunc <- function(){
  count <- 0
  for (i in c(1:testSize)) {
    if(output$`test$primaryCategories`[i]==output$SVM_LABEL[i])
      count = count+1
  }
  print((count/testSize)*100)  
}

checkAccuracyFunc()

cm <- confusionMatrix(output$SVM_LABEL, output$`test$primaryCategories`, dnn = c("P", "A"))
cm

plot(data.frame(c(1:testSize),matrix(output$SVM_PROB)), type="p")
plot(data.frame(c(1:testSize),matrix(output$SVM_PROB)), type="o")
plot(output$SVM_PROB~output$SVM_LABEL)
plot(output$SVM_LABEL)
#plot(output$SVM_PROB,output$SVM_LABEL)



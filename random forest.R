t <-read.csv("smoking_dataset.csv")
str(t)
t$gender<- as.factor(t$gender)
t$hearing.left.<- as.factor(t$hearing.left.)
t$hearing.right.<-as.factor(t$hearing.right.)
t$Urine.protein<-as.factor(t$Urine.protein)
t$oral<-as.factor(t$oral)
t$tartar<-as.factor(t$tartar)
t$dental.caries<-as.factor(t$dental.caries)
t$smoking<-as.factor(t$smoking)
rfNews()
library(randomForest)
set.seed(123)
sample_indices <- sample(nrow(x), 0.8 * nrow(x))  # 80% for training, 20% for testing

train_data <- x[sample_indices, ]
test_data <- x[-sample_indices, ]
library(ISLR)

head(iris)

std.iris <- scale(iris[1:4])
var(std.iris)

final.iris <- cbind(std.iris,iris[5])

library(caTools)
set.seed(101)
sample <- sample.split(final.iris$Species, SplitRatio=0.7)
train <- subset(final.iris, sample==T)
test <- subset(final.iris, sample==F)

library(class)
predicted.species <- knn(train[1:4], test[1:4], train$Species, k=1)

head(predicted.species)

misclassrate <- mean(test$Species != predicted.species)
misclassrate

predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
    set.seed(101)
    predicted.species <- knn(train[1:4], test[1:4], train$Species, k=i)
    error.rate[i] <- mean(test$Species != predicted.species)
}

library(ggplot2)

k.values <- 1:10
error.df <- data.frame(error.rate, k.values)

pl1 <- ggplot(error.df, aes(k.values, error.rate)) + geom_point()
pl2 <- pl1 + geom_line(lty='dotted', color='red', size=1)
pl2



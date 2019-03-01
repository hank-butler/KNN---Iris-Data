# get data

library(ISLR)

head(iris)

str(iris)

# Standardize the data

stand.features <- scale(iris[1:4])

# Check if standardization worked

var(stand.features[,1])

# > var(stand.features[,1])
# [1] 1

# var = 1, good to go

final.data <- cbind(stand.features, iris[5])

head(final.data)

# train the test splits

set.seed(101)
library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = 0.70)

train <- subset(final.data, sample == T)
test <- subset(final.data, sample == F)

# Build KNN model

library(class)

predicted.species <- knn(train[1:4], test[1:4], train$Species, k =1)

predicted.species

mean(test$Species != predicted.species)

# Choosing a K Value

predicted.species = NULL
error.rate = NULL

for (i in 1:20){
  set.seed(101)
  predicted.species = knn(train[1:4], test[1:4], train$Species, k = i)
  error.rate[i] = mean(test$Species != predicted.species)
}

print(error.rate)

# Elbow method

library(ggplot2)

k.values <- 1:20

error.df <- data.frame(error.rate, k.values)

error.df

ggplot(error.df, aes(x = k.values, y = error.rate))+
  geom_point()+
  geom_line(lty = "dotted", color = "red")

# K drops between 2-6 then spikes. Primarily due to small data set.
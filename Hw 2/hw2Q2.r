class_data <- read.csv("/class_data.csv", header = TRUE)
class1 <- na.omit(class_data$class1) # remove NA values since class 1 has fewer observations
class2 <- class_data$class2
plot(class1)
plot(class2)
mean1 <- mean(class1)
mean2 <- mean(class2)

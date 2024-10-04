setwd("C:/Users/momot/OneDrive/Documents/GitHub/Data-Analysis-HW/Hw 2")
class_data <- read.csv("class_data.csv", header = TRUE)
class1 <- na.omit(class_data$class1)
class2 <- class_data$class2

png(filename = "Class1Data.png")
plot(class1, main = "Class 1 Scores", xlab = "Scores", ylab = "Student Number")
mean1 <- mean(class1)
dev.off()
png(filename = "Class2Data.png")

plot(class2, main = "Class 1 Scores", xlab = "Scores", ylab = "Student Number")
mean2 <- mean(class2)
dev.off()
print(paste("mean for class 1,", mean1))
print(paste("mean for class 2,", mean2))
# We will use a t-test

spooled <- ((length(class1) - 1) * sd(class1)^2 + (length(class2) - 1) * sd(class2)^2) / (length(class1) + length(class2) - 2)

t <- (mean1 - mean2) / (sqrt(spooled) * sqrt(1 / length(class1) + 1 / length(class2)))
ta <- qt(1 - .05 / 2, length(class1) + length(class2) - 2)
print(paste("our test statistic:", abs(t), ">", ta, " which is t_.925,n_x+x_y-2"))

ta <- qt(1 - .01 / 2, length(class1) + length(class2) - 2)
print(paste("our test statistic:", abs(t), "<", ta, " which is t_.995,n_x+x_y-2"))
# Thus we reject H_0 = m_class1 - m_class2 = 0 and H_A = m_class1 - m_class2 != 0
# We make the same conclusion as our test stat is still slightly larger

diabetes <- read.csv("diabetes.csv")
# Bernoulli (yes/no)
glm.logit <- glm(diabetes ~ glucose + pressure + bmi + age,
    data = diabetes, family = "binomial"
)
summary(glm.logit)
exp(glm.logit$coefficients)

beta.var <- vcov(glm.logit)
# fisher.info <- solve(vcov(glm.logit))
var.b1 <- beta.var[5, 5]
var.b1
beta1.hat <- glm.logit$coefficients[5]
# Note that the standard error for beta1, sqrt(var.b1) is exactly the same
# as the Std. Error value listed for gre in the second column of the model summary

zstar <- (beta1.hat - 0.05) / (sqrt(var.b1))

z.critical <- qnorm(0.95, mean = 0, sd = 1)
zstar
z.critical
zstar > z.critical
# Reject?

for (i in names(diabetes)) {
    print(i)
    print(confint(glm.logit, i, level = 0.90))
}


newdata <- data.frame(
    glucose = 150, pressure = 100, bmi = 20, age = 45
)

#
# (e)
#
predict(glm.logit, newdata, type = "response")

newdata2 <- data.frame(
    glucose = 150, pressure = 50, bmi = 20, age = 45
)

predict(glm.logit, newdata2, type = "response")


newdata3 <- data.frame(
    glucose = 150, pressure = 90, bmi = 20, age = 45
)

predict(glm.logit, newdata3, type = "response")
# Not much change

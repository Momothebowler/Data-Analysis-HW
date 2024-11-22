crime_rate <- read.csv("Test2/crime_rate.csv")

mod <- lm(crime ~ home.value + income +
    open.space + no.plumb + dist.bd + north.south, data = crime_rate)

#
# (a)
#
# Summary Here
summary(mod)

north <- crime_rate[crime_rate$north.south == 1, ]
south <- crime_rate[crime_rate$north.south == 0, ]

mean_north <- mean(north$crime)
mean_south <- mean(south$crime)

north_crime <- north$crime
south_crime <- south$crime

print(paste("mean for north,", mean_north))
print(paste("mean for south,", mean_south))

spooled <- ((length(north_crime) - 1) *
    sd(north_crime)^2 + (length(south_crime) - 1) *
        sd(south_crime)^2) / (length(north_crime) + length(south_crime) - 2)

t <- (mean_north - mean_south) / (sqrt(spooled) *
    sqrt(1 / length(north_crime) + 1 / length(south_crime)))

ta <- qt(1 - .01 / 2, length(north_crime) + length(south_crime) - 2)

#
# (b)
#
print(paste(
    "our test statistic:", abs(t), "<",
    ta, " which is t_.995,n_x+x_y-2"
))
# Used t-test
# Thus we do not reject null
# where null states X_1 - X_2 = 0
# Where X_1 is average crime rate of North
# Where X_2 is average crime rate of South

home_value <- crime_rate$home.value
mean_value <- mean(home_value)
# wish to show for hyp test H_0: mean_value - 0.3 = 0

val <- -0.3

an <- anova(mod)
MSE <- an["Mean Sq"]["Residuals", ]
Sxx <- sum((home_value - mean_value)^2)


tstar <- (mod$coefficients["home.value"] - val) /
    sqrt(MSE / Sxx)


ta2 <- qt(1 - .05 / 2, length(home_value) - (6 - 1))

#
# (c)
#
print(paste(
    "our test statistic:", abs(tstar), "<",
    ta2, " which is t_.95,n_x+x_y-2"
))

# Thus we don't reject H_0


#
# (d)
#
for (i in names(crime_rate)) {
    print(confint(mod, i, level = 0.85))
}



newdata <- data.frame(
    home.value = 20, income = 19, open.space = 10,
    no.plumb = 5, dist.bd = 2, north.south = 1
)

#
# (e)
#
predict(mod, newdata, interval = "confidence", level = .9)
predict(mod, newdata, level = .9)

pairs(crime_rate)

crime_rate2 <- crime_rate
crime_rate2$income <- log(crime_rate$income)
pairs(crime_rate2)

mod3 <- lm(crime ~ ., data = crime_rate2)

crime_rate3 <- crime_rate2
crime_rate3$no.plumb <- log(crime_rate2$no.plumb)
pairs(crime_rate3)

mod3 <- lm(crime ~ ., data = crime_rate3)

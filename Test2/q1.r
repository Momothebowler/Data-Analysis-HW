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

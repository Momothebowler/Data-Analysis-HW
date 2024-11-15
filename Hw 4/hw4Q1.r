library(DAAG)
data("ais")
plot(ais$sport, ais$lbm,
    pch = 16,
    ylab = "Lean Body Mass",
    xlab = "Sport",
    main = "Plot and linear fit of car weight by mileage"
)
mod <- lm(ais$lbm ~ ais$sport, data = ais)

summary(mod)
anova(mod)
# BasketBall, not baseball
# Thus we can see also p-value: 3.562e-15 that there is a
# a high probability that the Beta estimates are 0 or correct
# ??????

# For Field the coefficient is 8.2014 so those who do track tend
# to have a higher lean body mass than those in baseball

# For Tennis the coefficient is -12.0173 so thos who do Tennis
# tend to have a lower lean body mass than those in baseball

Z <- model.matrix(mod)
a <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1, -1), ncol = 1)
ZTZi <- solve(t(Z) %*% Z)
ZTZi

atZTZia <- t(a) %*% ZTZi %*% a
atZTZia

est <- t(a) %*% summary(mod)$coef[, 1]
est
# Point estimate for hw
MSE <- anova(mod)[2, 3]
MSE
# t statistic for hypothesis test of H_0:alpha_3=alpha_2=0
t.stat <- (est - 0) / sqrt(MSE * atZTZia)
# p-value for that test
n <- length(ais$lbm)
p <- ncol(Z)
# 2 * (1 - pt(t.stat, (n - p)))
lower <- est - qt(.95, 192) * sqrt(MSE * atZTZia)
upper <- est + qt(.95, 192) * sqrt(MSE * atZTZia)
c(lower, upper)


mod2 <- lm(ais$lbm ~ ais$sport + ht, data = ais)
summary(mod2)
# Lost on interpretation if correct so far

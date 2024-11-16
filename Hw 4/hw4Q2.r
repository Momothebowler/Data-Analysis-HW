library(alr4)
data("UN11")

# Fit a model with fertility as the response variable and ppgdp, lifeExpF, and pctUrban as predictor
un11_sub <- UN11[, c("fertility", "ppgdp", "lifeExpF", "pctUrban")]
# fit the regression model
mod <- lm(fertility ~ ppgdp + lifeExpF + pctUrban, data = UN11)
png(filename = "residuals.png")
plot(mod)
dev.off()
summary(mod)

X <- model.matrix(mod)
rownames(X) <- NULL # remove row (car) names
attr(X, "assign") <- NULL # remove attributes
attr(X, "contrasts") <- NULL # remove attributes

png(filename = "scatterofpairs.png")
pairs(un11_sub)
dev.off()

# log trends, I do not believe linear will work here

un11_sub$ppgdp <- log(UN11$ppgdp)
# names(un11_sub)[names(un11_sub) == "ppgdb"] <- "ppgdb"
# par(mfrow = c(1, 2))
# plot(UN11$fertility, UN11$ppgdp)
# plot(log(UN11$fertility), UN11$ppgdp)

# definitely see a log relationship?
# un11_sub
mod2 <- lm(fertility ~ ., data = un11_sub)
summary(mod2)
png(filename = "residuals2.png")
plot(mod2)
dev.off()
png(filename = "logpp.png")
pairs(un11_sub)
dev.off()

un11_sub$fertility <- log(UN11$fertility)
png(filename = "multilog.png")
pairs(un11_sub)

mod3 <- lm(fertility ~ ., data = un11_sub)
summary(mod3)
png(filename = "residuals3.png")
plot(mod3)
dev.off()
# I believe now a linear model is appropriate

dat.heights <- read.table("Hw 3/family_heights.txt", header = TRUE)

son.heights <- dat.heights[dat.heights$Gender == "M", ]

png(filename = "SonsData.png")
plot(son.heights$Father, son.heights$Height,
    pch = 16,
    ylab = "Son's Heights",
    xlab = "Father's Height",
    main = "Plot and linear fit of Parent to Child Height"
)

slr.fit <- lm(son.heights$Height ~ son.heights$Father, data = son.heights)
abline(slr.fit, lwd = 2, col = "red")
dev.off()

residuals <- resid(slr.fit)

png(filename = "ResidualsofSons.png")
plot(son.heights$Father, residuals,
    pch = 16,
    ylab = "Residual",
    xlab = "Father's Height",
    main = "Plot of Residual"
)
dev.off()

png(filename = "QQPlot.png")
qqnorm(residuals, main = "QQPlot of Residuals")
qqline(residuals)
dev.off()

anova.table <- anova(slr.fit)
MSE <- anova.table$"Mean Sq"[2]
xn <- 70
n <- length(son.heights$Height)
beta0 <- coef(slr.fit)[1]
beta1 <- coef(slr.fit)[2]
x <- son.heights$Father
yn <- beta0 + beta1 * xn
names(yn) <- NULL
tv <- qt(0.975, n - 2)
varey <- MSE * ((1 / n) + ((xn - mean(x))^2) / sum((x - mean(x))^2))
cilow.ey <- yn - tv * sqrt(varey)
ciupp.ey <- yn + tv * sqrt(varey)

xn <- 70
yn <- beta0 + beta1 * xn
names(yn) <- NULL
tv <- qt(0.975, n - 2)
varynew <- MSE * (1 + (1 / n) + ((xn - mean(x))^2) / sum((x - mean(x))^2))
pilow <- yn - tv * sqrt(varynew)
piupp <- yn + tv * sqrt(varynew)
print(c(pilow, piupp))
print(c(cilow.ey, ciupp.ey))
beta1

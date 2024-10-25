data("trees")

png(filename = "TreeHeights.png")
plot(trees$Volume, trees$Height,
    pch = 16,
    ylab = "Tree Height",
    xlab = "Tree Volume",
    main = "Plot and linear fit of tree volume by height"
)

slr.fit <- lm(Height ~ Volume, data = trees)
Volume <- trees$Volume
Height <- trees$Height

abline(slr.fit, lwd = 2, col = "red")
dev.off()

anova.table <- anova(slr.fit)

confint(slr.fit, level = 0.90)
b0 <- slr.fit$coefficients[1]
b1 <- slr.fit$coefficients[2]

r <- sum((trees$Height - slr.fit$fitted.values)^2)
LSE <- r / (length(Volume) - 2)
MLE <- r / (length(Volume))

confint(slr.fit, level = 0.90)

MSE <- anova.table$"Mean Sq"[2]
Sxx <- sum((Volume - mean(Volume))^2)

SEb1 <- sqrt(MSE / Sxx)
t <- (b1 - .5) / SEb1

qt(.05, df = 29, lower.tail = TRUE)

print(paste("Beta0 Hat is:", b0, "Beta1 Hat is:", b1))
print(paste("sigma^2 Hat is:", LSE, "MLE is:", MLE))
print("Our Beta0 hat and Beta1 hat confidence interval is:")
confint(slr.fit, level = 0.90)
print(paste(
    "For our test we find that t^* =", t, "and our test stat value is",
    qt(.05, df = 29, lower.tail = TRUE),
    "Thus we reject our null hypothesis."
))

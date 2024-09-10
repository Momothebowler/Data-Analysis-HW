data <- USArrests$UrbanPop
# print(data)
# plot(data)
mu <- mean(data)
std <- sd(data)
normalized <- dnorm(data, mu, std)

png(filename = "Normalized_Data.png")
plot(data, normalized,
    col = "blue",
    xlab = "% of Peoples", ylab = "Density",
    main = "Plot of densities for height data"
)
abline(v = mu, col = "red", lty = "dashed")
dev.off()

png(filename = "CDF.png")
plot(data, pnorm(data, mean = mu, sd = std),
    ylab = "Cumulative Distribution Function",
    xlab = "% of People",
    main = "CDF for height data"
)
abline(v = mu, col = "red", lty = "dashed")
dev.off()

print(pnorm(60, mean = mu, sd = std))
print(pnorm(80, mean = mu, sd = std) - pnorm(50, mean = mu, sd = std))

print(qnorm(0.75, mean = mu, sd = std))

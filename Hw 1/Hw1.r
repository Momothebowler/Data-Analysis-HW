data <- USArrests$UrbanPop

png(filename = "Urban_Data.png")
plot(data)
dev.off()

mu <- mean(data)
std <- sd(data)
normalized <- dnorm(data, mu, std)

png(filename = "Normalized_Data.png")
plot(data, normalized,
    col = "blue",
    xlab = "% of Peoples", ylab = "Density",
    main = "Plot of densities for Urban Pop data"
)
abline(v = mu, col = "red", lty = "dashed")
dev.off()

png(filename = "CDF.png")
plot(data, pnorm(data, mean = mu, sd = std),
    ylab = "Cumulative Distribution Function",
    xlab = "% of People",
    main = "CDF for Urban Pop data"
)
abline(v = mu, col = "red", lty = "dashed")
dev.off()

print(paste0(
    "Probability that 60% or less of ",
    "people in a state are living in urban areas: ",
    pnorm(60, mean = mu, sd = std)
), quote = FALSE)
print(paste0(
    "Probability that 50% to 80% of",
    "people in a state are living in urban areas: ",
    pnorm(80, mean = mu, sd = std) - pnorm(50, mean = mu, sd = std)
), quote = FALSE)

print(paste0(
    "The 75th percentile ends at a value of: ",
    qnorm(0.75, mean = mu, sd = std)
), quote = FALSE)

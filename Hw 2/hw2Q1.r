Y <- c(
    11.3, 12.0, 10.1, 10.3, 13.5, 10.8, 13.7, 12.8, 11.9, 10.7,
    11.0, 11.6, 10.1, 11.7, 10.6, 12.0, 11.7, 13.1, 11.3
)
n <- length(Y)
m <- sum(Y) / n
print(paste("Our maximum likelihood estimator is:", m))

# first approach
Z <- qnorm(.04, lower.tail = FALSE)

lower <- (mean(Y) - sd(Y) / sqrt(n) * Z)
upper <- (mean(Y) + sd(Y) / sqrt(n) * Z)
print(paste("Our interval is (", lower, ",", upper, ")"))
print("Thus we are 92% confident that our mean is in the interval above
 or that 8% of the time is lies outside of it.")

e <- (mean(Y) - 11) / (sd(Y) / sqrt(n))
zz <- qnorm(.025, lower.tail = FALSE)

print(paste("Since our test statistic:", e, "<", zz, "the Z value with a confidence interval of 95%"))
print("We reject the null hypothesis where H0 = mu and HA != mu")

# TODO verify above and do the last part of Q1

upperchi <- qchisq(.1 / 2, n - 1, lower.tail = TRUE)
lowerchi <- qchisq(.1 / 2, n - 1, lower.tail = FALSE)

print(paste(
    "Our interval for sd^2 is (", (n - 1) * sd(Y) / lowerchi,
    ",", (n - 1) * sd(Y) / upperchi, ")"
))

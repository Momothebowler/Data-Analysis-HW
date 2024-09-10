data <- USArrests$UrbanPop
plot(data)
normalized <- dnorm(data, mean(data), sd(data))
plot(data, normalized)
cdf <- pnorm(data, mean(data), sd(data))
plot(data,cdf)
print(1-pnorm(60, mean(data), sd(data)))
print(pnorm(50, mean(data), sd(data)) - (1 - pnorm(80, mean(data), sd(data))))

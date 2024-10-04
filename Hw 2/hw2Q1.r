Y <- c(
    11.3, 12.0, 10.1, 10.3, 13.5, 10.8, 13.7, 12.8, 11.9, 10.7,
    11.0, 11.6, 10.1, 11.7, 10.6, 12.0, 11.7, 13.1, 11.3
)
n <- length(Y)
m <- sum(Y) / n
print(paste("Our maximum likelihood estimator is:", m))


alpha <- 0.08
df <- 19 - 1
sample.mean <- mean(Y)
sample.n <- length(Y)
sample.sd <- sd(Y)
sample.se <- sample.sd / sqrt(sample.n)
t.score <- qt(p = alpha / 2, df = degrees.freedom, lower.tail = F)
margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound, upper.bound))
print("Thus we are 92% confident that our mean is in the interval above
 or that 8% of the time is lies outside of it.")

t <- (sample.mean - 11) / (sample.sd / sqrt(19))
ta <- qt(1 - .05 / 2, length(class1) + length(class2) - 2)
print(t)
print(ta)
ewa <- t.test(Y, mu = 11)
print(ewa)
# By ewa we see that true mean is not equal to 11
# as t = 2.3611 which is outside our confidence interval
# our condfidence intervals almost line up (different alpha values)
chilower <- qchisq(.1 / 2, 18)
chiupper <- qchisq((1 - .1 / 2), 18)
v <- var(Y)
var.interval <- c(degrees.freedom * v / chiupper, degrees.freedom * v / chilower)
print(var.interval)

print(paste(
    "Our interval for sd^2 is (", degrees.freedom * v / chiupper,
    ",", degrees.freedom * v / chilower, ")"
))

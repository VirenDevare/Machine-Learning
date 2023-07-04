# Group No-9 
#GAssignment 2

#data collection and preparation
wbcd <- read.csv(url("car.data"), header=FALSE)
wbcd = wbcd[ , -1]
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
wbcd.X = subset(wbcd, select = -V2)
wbcd_X.normalized = as.data.frame(lapply(wbcd.X, normalize))
train.X = wbcd_X.normalized[1:350, ]
test.X = wbcd_X.normalized[351:569, ]
train.Y = wbcd[1:350, 1]
test.Y = wbcd[351:569, 1]

# For loop K++
library("class")
knn.errors <- data.frame(K = integer(), Error = double())
for (K in seq(21)) {
  set.seed(1)
  pred <- knn(train.X, test.X, train.Y, K)
  error <- mean(pred != test.Y)
  knn.errors <- rbind(knn.errors, data.frame("K" = K, "Error" = error))
}
knn.errors

# Find the best of K
minrow <- which.min(knn.errors$Error)
best.k <- knn.errors$K[minrow]
min.error <- knn.errors$Error[minrow]
print(paste("Best K is", best.k, "with a test error rate of", min.error))

# Plot 1/k
plot(
  x = 1/knn.errors$K,
  y = knn.errors$Error,
  xlab = "1/ K",
  ylab = "Test Error Rate",
  main = "Curve of test error vs. 1/K",
  log = "x",
  ylim = c(0, knn.errors$Error[which.max(knn.errors$Error)]),
  col = "blue",
  type = "b"
)


# Plot K vs test error rate
plot(
  x = knn.errors$K,
  y = knn.errors$Error,
  xlab = "K",
  ylab = "Test Error Rate",
  main = "Curve of test error vs K",
  log = "x",
  ylim = c(0, knn.errors$Error[which.max(knn.errors$Error)]),
  col = "blue",
  type = "b"
)
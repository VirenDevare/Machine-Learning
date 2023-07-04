#Final project

library(class)

car_ds<- read.csv("car.data", header = FALSE)

car_ds = car_ds[,-1]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

car_ds.X = subset(car_ds, select = -V2)
car_ds_X.normalized = as.data.frame(lapply(car_ds.X, normalize))
smplsize = nrow(car_ds)
chunk_size = round(smplsize / 5)
testerror = rep(0, numK)
numK = 21
cv.k = 5

for (k in 1:numK) {
  cv.error.k = rep(0, cv.k)
  for (n in 1:cv.k) {
    vset.start = (n - 1) * chunk_size + 1
    if (n == cv.k) {
      vset.end = smplsize
    } else{
      vset.end = n * chunk_size
    }
    vset.range = vset.start:vset.end
    
    train.X = car_ds_X.normalized[-(vset.range), ]
    test.X = car_ds_X.normalized[vset.range,]
    train.Y = factor(car_ds[-(vset.range), 1])
    test.Y = factor(car_ds[vset.range, 1])
    set.seed(1)
    car_ds.pred = knn(train.X, test.X, train.Y, k)
    cv.error.k[n] = mean(car_ds.pred != test.Y)
  }
  testerror[k] = mean(cv.error.k)
}

plot(
  1:numK,
  testerror,
  xlab = "K",
  ylab = "Test Error Rate",
  type = 'b',
  col = 'purple',
  main = "Test Error Rate K",
  ylim = c(0, max(test_errors))
)
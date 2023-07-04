#Group 9 - Assignment 4

library(class)

wbcd <- read.csv("wdbc.data", header = FALSE)
wbcd = wbcd[,-1]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd.X = subset(wbcd, select = -V2)
wbcd_X.normalized = as.data.frame(lapply(wbcd.X, normalize))
smplsize = nrow(wbcd)
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
    
    train.X = wbcd_X.normalized[-(vset.range), ]
    test.X = wbcd_X.normalized[vset.range,]
    train.Y = factor(wbcd[-(vset.range), 1])
    test.Y = factor(wbcd[vset.range, 1])
    set.seed(1)
    wbcd.pred = knn(train.X, test.X, train.Y, k)
    cv.error.k[n] = mean(wbcd.pred != test.Y)
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

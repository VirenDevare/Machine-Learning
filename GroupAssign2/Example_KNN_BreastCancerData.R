# knn method in R
# data set:breast cancer winsconsin dianosis from UCI Machine Leanring Repository
# data atttibutes: https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic)

#STEP 1: data collection 
wbcd <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"), header=FALSE)

#STEP 2: exploring and preparing data 
names(wbcd)
summary(wbcd)
str(wbcd)
#remove medical ID number:
wbcd = wbcd[ , -1] 
names(wbcd)
dim(wbcd)

#explanation of normalization:
x = c(1, 2, 3,4, 5)
x.normalized = (x-min(x))/(max(x)-min(x))
x.normalized
#define function normalize: 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
y = c(2,3,4,5,6)
y.n= normalize(y)
y.n

#continue the application on breast cancer analysis
summary(wbcd)
# get X by excluding the diagnosis V2 
wbcd.X = subset(wbcd, select = -V2)
summary(wbcd.X)
dim(wbcd.X)
#normalize X so all attributes contribute equally in calculating distance
wbcd_X.normalized = as.data.frame(lapply(wbcd.X, normalize))
summary(wbcd_X.normalized)

#STEP3: split data into traning and test sets
#training the model on the data
train.X = wbcd_X.normalized[1:350, ]
test.X = wbcd_X.normalized[351:569, ]
train.Y=wbcd[1:350, 1]
#equivalent to train.Y=wbcd[1:350, c("V2")]
test.Y=wbcd[351:569, 1] 
?knn
install.packages("class")
library("class")
?knn
dim(wbcd)


#STEP 4:Train the model 
# choose k=21:
wbcd.pred = knn(train.X, test.X, train.Y, 21)

#wbcd$V2 = factor(wbcd$V2, levels=c("B", "M"))
#contrasts(wbcd$V2)
#train.Y=wbcd[1:350, 1]
#test.Y=wbcd[351:699, 1]

#STEP 4: evaluating performance
#depending on the data, you may need to use various metrics
table(wbcd.pred, test.Y)
err = mean(wbcd.pred != test.Y)
err

#STEP 5: improving performance by 
# change k 
# scale data through standadization instead of normalization
# resampling data
library(htmlTable)
library(tidyverse)
library(ggplot2)
library(rvest)
library(naniar)
# data reading
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
clev <- read.csv(url,  header = FALSE)
head(clev)

# Initializing column name
colnames(clev)<- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","hd") 
head(clev)

str(clev)
sum(is.na(clev))
# Checking missing data
vis_miss(clev)
# checking missing data column vise
sapply(clev, function(x)sum(is.na(x)))

# Changing the numerical value of sex from 0,1 to character F, M
clev$sex = factor(clev$sex, levels = c(0,1),labels = c("F","M"))

clev$cp <- as.factor(clev$cp)
clev$fbs <- as.factor(clev$fbs)
clev$restecg <- as.factor(clev$restecg)
clev$exang <- as.factor(clev$exang)

clev$slope <- as.factor(clev$slope)

clev$ca <- as.factor(clev$ca)

clev$thal <- as.factor(clev$thal)

clev$hd <- ifelse(clev$hd == 0, yes = "Healthy", no = "Unhealthy")

clev$hd <- as.factor(clev$hd)

clev[clev == "?"] <- NA

head(clev)
sum(is.na(clev))

str(clev)
clev$ca = ifelse(is.na(clev$ca), ave(clev$ca, FUN = function(x)mean(x, na.rm = TRUE)), clev$ca)


clev$thal <- as.numeric(clev$thal)

str(clev)
clev$thal = ifelse(is.na(clev$thal), ave(clev$thal, FUN = function(x)mean(x, na.rm = TRUE)),
                 clev$thal)


list_na <- colnames(clev)[ apply(clev, 2, anyNA) ]
list_na
clev$thal <- as.factor(clev$thal)

clev$ca <- as.factor(clev$ca)

str(clev)
xtabs(~hd+sex, data = clev)
xtabs(~hd+cp,data = clev)
xtabs(~hd+fbs, data = clev)
xtabs(~hd+restecg, data = clev)

# Logestic regression
logistic <- glm(hd~sex, data = clev, family = "binomial")
summary(logistic)

# Spliting data in train data and test data with the help of caTools 
library(caTools)
split = sample.split(clev$hd, SplitRatio = 0.8)

train_data = subset(clev, split == TRUE )
test_data = subset(clev, split == FALSE )
view(train_data)

nrow(train_data)
nrow(test_data)
logistic_new <- glm(hd~.,data = train_data, family = "binomial")

summary(logistic_new)
logistic_m <- glm(hd~sex+slope+cp+ca+thal, family = "binomial", data = train_data)
summary(logistic_m)

# prediction part
p1 <- predict(logistic_m, train_data, type = "response")
head(p1)

# Comparing predicted data with train data set 
head(train_data)
pred1 <- ifelse(p1>0.5, 1,0)
table(pred=pred1,Actual = train_data$hd)
p2 <- predict(logistic_m, test_data, type = "response")
pred2 <- ifelse(p2>0.5, 1,0)

table(prediction=pred2,Actual = test_data$hd)
library(tinytex)
with(logistic_m, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F))

# The above p Value is 6.45 x 10^{-29} which is very less, hence our model is statistically very significant. 

ll.null <- logistic$null.deviance/-2

ll.proposed <- logistic_new$deviance/-2
# Calculating the pseudo R^2
(ll.null - ll.proposed)/ll.null
# To calculate a p-value for that R^2 using a Chi-Square distribution 

1-pchisq(2*(ll.proposed-ll.null), df = (length(logistic$coefficients)-1))
# Now plotting the graph, for the same we need new data.frame that contains probabilities of having heart disease along with the actual heart disease status 

predicted.data <- data.frame(probability.of.hd = logistic$fitted.values, hd = clev$hd)
predicted.data

# Now sorting the dataframe from low probabilities to high probabilities 
predicted.data <- predicted.data[order(predicted.data$probability.of.hd, decreasing = FALSE),]

# Adding new column to dataframe that has the rank of each sample, from low probability to high probability 
predicted.data$rank <- 1:nrow(predicted.data)
predicted.data

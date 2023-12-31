library("ISLR")
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower = 98), interval ="confidence")
predict(lm.fit, data.frame(horsepower = c(95)), interval ="confidence")
predict(lm.fit, data.frame(horsepower = c(95)), interval ="prediction")
attach(Auto)
plot(mpg~horsepower, main =" MPG vs Horsepower", xlab = " Horsepower", ylab ="MPG")
abline(coef = coef(lm.fit), col ="red")
detach(Auto)
par(mfrow=c(2,2))
plot(lm.fit)

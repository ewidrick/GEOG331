rm(list=ls())

#subject the virginica species
flower <- iris[iris$Species == "virginica",]

# make a scatter plot to look at sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, pch=19, 
  xlab= "Sepal Length", ylab= "Petal Length",
  main ="Iris Virginica")
  
#fit a regression line
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

# plot the residuals 
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab= "Sepal Length", ylab = "Petal length", 
    abline(h=0))

# Check normality of residuals
hist(summary(fit)$residuals, col = "red",
     main = "Residual Distribution", xlab = "Residuals")

# qqnorm of gg line can provide another visual check
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

# use Shapiro Wilks test to check normality
shapiro.test(summary(fit)$residuals)

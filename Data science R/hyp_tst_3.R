cutlets <- read.csv(file.choose())
View(cutlets)
attach(cutlets)

#normality test
library(nortest)
ad.test(cutlets$Unit.A)
ad.test(cutlets$Unit.B)

#variance test
#H0 <- both  are same var
#H1 <- both are diff var
var.test(cutlets$Unit.A,cutlets$Unit.B)

#2-sample test for equal variance
#H0 <- cutlets have same diameter of 2 units
#H1 <- cutlets have diff diameter of 2 units

t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level = 0.95)

lab <- read.csv(file.choose())
View(lab)
attach(lab)

#normality test
library(nortest)
shapiro.test(Laboratory.1)
shapiro.test(Laboratory.2)
shapiro.test(Laboratory.3)
shapiro.test(Laboratory.4)

#variance test
stacked <- stack(lab)
stacked
library(car)
leveneTest(stacked$values~stacked$ind,data=stacked)

#one way ANOVA test
anova_result <- aov(values~ind,data=stacked)
summary(anova_result)

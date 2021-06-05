
#chi-square test
a <- read.csv(file.choose())
View(a)
b <- a[,-1]
b
#H0 ->product sales ratio are same for males and females
#H1 ->product sales ratio are not same for males and females

chisq.test(b)

a=read.csv(file.choose())
View(a)
library(BSDA)
library(e1071)
library(nortest)
attach(a)
install.packages("tidyr")
library(tidyr)
a1 <- table(gather(a,nation,status,1:4))
a1
chisq.test(a1)
#H0 <- all the centers have same defective% 
#H1 <- different centers have defective%

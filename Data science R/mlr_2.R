data <- read.csv(file.choose())
library(e1071)
View(data)
corolla <- data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)
summary(corolla)
str(corolla)
attach(corolla)
library(psych)
describe(corolla)                          
pairs(corolla)
cor(corolla)
library(corpcor)
cor2pcor(cor(corolla))

#multiple linear regression
model1 <- lm(Price~.,data = corolla)
summary(model1)                             
library(car)
vif(model1) 
rmse <- mean(model1$residuals^2)^.5         
pred <- predict(model1,corolla)
cor(pred,corolla$Price)                     
influence.measures(model1)
influenceIndexPlot(model1,id=list(col="red",cex=2,n=10))
influencePlot(model1,id=list(col='red',cex=2,n=10))
qqPlot(model1)
residualPlot(model1)

modelcc <- lm(Price~cc,data = corolla)
summary(modelcc)
modeldoors <- lm(Price~Doors,data = corolla)
summary(modeldoors)
modelccdoors <- lm(Price~cc+Doors,data = corolla)
summary(modelccdoors)

#deleting the influenced observations
model2 <- lm(Price~.,data = corolla[-c(81,222),])
summary(model2)                                     
rmse2 <- mean(model2$residuals^2)^.5  
pred2 <- predict(model2,corolla)
cor(pred2,corolla$Price)            
avPlots(model2)
residualPlot(model2)

#tranformation model
l=log(corolla[,-1])
corolla2 <- data.frame(l,corolla$Price)

#log model
model3 <- lm(corolla.Price~.,data = corolla2)
summary(model3)                                 
rmse3 <- mean(model3$residuals^2)^.5            
pred3 <- predict(model3,corolla2)
cor(pred3,corolla2$corolla.Price)              
avPlots(model3)
influenceIndexPlot(model3,id=list(col='red',n=10,cex=1.5))
influencePlot(model3,id=list(col='red',n=10,cex=1.5))
qqPlot(model3)
residualPlot(model3)
vif(model3)

#log model with removing influence
model4 <- lm(corolla.Price~.,data = corolla2[-c(185,186)])
summary(model4)
rmse4 <- mean(model4$residuals^2)^.5
pred4 <- predict(model4,corolla2)
cor(pred4,corolla2$corolla.Price) 
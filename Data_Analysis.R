library(csv)
library(ggplot2)
library(ROSE)
library(smotefamily)
library(lmtest)
library(ie2misc)
library(Metrics)
library(car)

Protein_Data<-read.csv("C:\\Users\\ASUS\\Desktop\\Protein_Supply_Quantity_Data.csv")
head(Protein_Data)
tail(Protein_Data)
summary(Protein_Data)
str(Protein_Data)


Protein_Data<-subset(Protein_Data , select = -c(Country,Undernourished , Confirmed , Deaths , Recovered , Active , Unit..all.except.Population. ))
summary(Protein_Data)
Protein_Data$Obesity[is.na(Protein_Data$Obesity)]<-mean(Protein_Data$Obesity, na.rm = T)
summary(Protein_Data)
na.omit(Protein_Data)
summary(Protein_Data)


cor(Protein_Data$Obesity ,Protein_Data$Alcoholic.Beverages)
cor(Protein_Data$Obesity ,Protein_Data$Animal.Products)
cor(Protein_Data$Obesity ,Protein_Data$Animal.fats)
cor(Protein_Data$Obesity ,Protein_Data$Aquatic.Products..Other)
cor(Protein_Data$Obesity ,Protein_Data$Cereals...Excluding.Beer)
cor(Protein_Data$Obesity ,Protein_Data$Eggs)
cor(Protein_Data$Obesity ,Protein_Data$Fish..Seafood)
cor(Protein_Data$Obesity ,Protein_Data$Fruits...Excluding.Wine)
cor(Protein_Data$Obesity ,Protein_Data$Meat)
cor(Protein_Data$Obesity ,Protein_Data$Offals)
cor(Protein_Data$Obesity ,Protein_Data$Oilcrops)
cor(Protein_Data$Obesity ,Protein_Data$Pulses)
cor(Protein_Data$Obesity ,Protein_Data$Spices)
cor(Protein_Data$Obesity ,Protein_Data$Starchy.Roots)
cor(Protein_Data$Obesity ,Protein_Data$Stimulants)
cor(Protein_Data$Obesity ,Protein_Data$Sugar.Crops)
cor(Protein_Data$Obesity ,Protein_Data$Sugar...Sweeteners)
cor(Protein_Data$Obesity ,Protein_Data$Treenuts)
cor(Protein_Data$Obesity ,Protein_Data$Vegetal.Products)
cor(Protein_Data$Obesity ,Protein_Data$Vegetable.Oils)
cor(Protein_Data$Obesity ,Protein_Data$Vegetables)
cor(Protein_Data$Obesity ,Protein_Data$Miscellaneous)
cor(Protein_Data$Obesity ,Protein_Data$Milk...Excluding.Butter)




library(dplyr)
library(ggcorrplot)
library(plotly)


k = 1
correlation <- c()
l <- names(Protein_Data[,-c(1,24,25,26,27,28,29)])
for(i in Protein_Data[,-c(1,24,25,26,27,28,29)])
{
  # plot(i, data_clean$Obesity, xlab = toString(l[k]), ylab = "Obesity")
  correlation[[k]]<-as.character(paste(l[k])) 
  correlation[[k]][2]<- as.numeric(cor(Protein_Data$Obesity, i))
  k <- k+1
}
correlation <- t(bind_cols(correlation))
correlation <- data.frame(correlation)
colnames(correlation)[2] <- "Correlation Value"
colnames(correlation)[1] <- "Correlation Between"
correlation$`Correlation Value` <- as.numeric(correlation$`Correlation Value`)

View(correlation)
head(correlation)
summary(correlation)

corr.plot <- ggcorrplot(cor(Protein_Data[,-1], method=c("spearman"), use="complete.obs"), title = "Correlation Matrix for Food Supply Quantity(in Kcal) data.", lab_col = 8, tl.cex = 6)
ggplotly(corr.plot)





Protein_Data<-subset(Protein_Data , select = -c(Aquatic.Products..Other , Cereals...Excluding.Beer , Fish..Seafood , Offals , Oilcrops , Pulses , Spices , Starchy.Roots , Sugar.Crops , Sugar...Sweeteners , Vegetal.Products ))
head(Protein_Data)
summary(Protein_Data)


boxplot(Protein_Data$Alcoholic.Beverages , Protein_Data$Animal.Products , Protein_Data$Animal.fats)
boxplot(Protein_Data$Eggs , Protein_Data$Fruits...Excluding.Wine , Protein_Data$Meat )
boxplot(Protein_Data$Milk...Excluding.Butter , Protein_Data$Stimulants , Protein_Data$Treenuts )
boxplot(Protein_Data$Vegetable.Oils , Protein_Data$Vegetables , Protein_Data$Miscellaneous )
boxplot(Protein_Data$Obesity , Protein_Data$Population)


ggplot(Protein_Data,aes(x=Alcoholic.Beverages,y=Obesity)) + geom_point() + stat_smooth(color="red")
ggplot(Protein_Data,aes(x=Animal.Products,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Animal.fats,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Eggs,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Fruits...Excluding.Wine ,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Meat,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Milk...Excluding.Butter ,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Stimulants ,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Treenuts ,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Vegetable.Oils ,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Vegetables ,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Miscellaneous,y=Obesity)) + geom_point() + stat_smooth()
ggplot(Protein_Data,aes(x=Population ,y=Obesity)) + geom_point() + stat_smooth()



set.seed(1234)
ind<-sample(2,nrow(Protein_Data), replace=T , prob=c(0.7,0.3))
train<-Protein_Data[ind==1,]
test<-Protein_Data[ind==2,]
nrow(train)
nrow(test)

head(ind)
tail(ind)
#data$Admit<-c(data$Admit)

#training and testing
train<-Protein_Data[ind==1,]
test<-Protein_Data[ind==2,]

nrow(train)
nrow(test)
head(train)
tail(test)


#Protein_Data<-SMOTE(Obesity ~ . ,Protein_Data  ,K=2147483648)

#Protein_Data.rose <- ROSE(Obesity ~., data = train, seed = 4)$Protein_Data


#cor(Protein_Data$Obesity ,Protein_Data$Alcoholic.Beverages)
cor(Protein_Data$Obesity ,Protein_Data$Animal.Products)
#cor(Protein_Data$Obesity ,Protein_Data$Animal.fats)
cor(Protein_Data$Obesity ,Protein_Data$Eggs)
cor(Protein_Data$Obesity ,Protein_Data$Milk...Excluding.Butter)
cor(Protein_Data$Obesity ,Protein_Data$Meat)
#cor(Protein_Data$Obesity ,Protein_Data$Stimulants)
#cor(Protein_Data$Obesity ,Protein_Data$Treenuts)
#cor(Protein_Data$Obesity ,Protein_Data$Fruits...Excluding.Wine)
#cor(Protein_Data$Obesity ,Protein_Data$Vegetables)
#cor(Protein_Data$Obesity ,Protein_Data$Miscellaneous)


print("The indepenent variabes are Animal.Products , Eggs , Meat , Vegetable.Oils")

print("Model 1")

#Correlation (R)
cor(train$Obesity , train$Animal.Products)
Model1 = lm(Obesity~Animal.Products,data=train)
Model1
summary(Model1)
#coefficient
coeff_Model1=coefficients(Model1)
coeff_Model1
#y=b0 + B!*Animal.Products
eq_Model1 = paste("Regression Line: y=" , round(coeff_Model1[1],4),"+",round(coeff_Model1[2],4),"*","Animal.Products")
eq_Model1



print("Model 2")

#correlation (R)
cor(train$Obesity , train$Eggs)
Model2 = lm(Obesity~Eggs,data=train)
Model2
summary(Model2)
#coefficient
coeff_Model2=coefficients(Model2)
coeff_Model2
#y=b0 + B!*Eggs
eq_Model2 = paste("Regression Line: y=" , round(coeff_Model2[1],4),"+",round(coeff_Model2[2],4),"*","Eggs")
eq_Model2



print("Model 3")

#correlation (R)
cor(train$Obesity , train$Meat)
Model3 = lm(Obesity~Meat,data=train)
Model3
summary(Model3)
#coefficient
coeff_Model3=coefficients(Model3)
coeff_Model3
#y=b0 + B!*Eggs
eq_Model3 = paste("Regression Line: y=" , round(coeff_Model3[1],4),"+",round(coeff_Model3[2],4),"*","Meat")
eq_Model3



print("Model 4")

#correlation (R)
cor(train$Obesity , train$Milk...Excluding.Butter)
Model4 = lm(Obesity~Milk...Excluding.Butter,data=train)
Model4
#coefficient
coeff_Model4=coefficients(Model4)
coeff_Model4
#y=b0 + B!*Eggs
eq_Model4 = paste("Regression Line: y=" , round(coeff_Model4[1],4),"+",round(coeff_Model4[2],4),"*","Milk...Excluding.Butter")
eq_Model4


print("Model 5")

#correlation (R)
cor(train$Obesity , train$Animal.Products+train$Eggs)
Model5 = lm(Obesity~Animal.Products+Eggs,data=train)
Model5
#coefficient
coeff_Model5=coefficients(Model5)
coeff_Model5
#y=b0 + B!*Eggs
eq_Model5 = paste("Regression Line: y=" , round(coeff_Model5[1],4),"+",round(coeff_Model5[2],4),"*","Animal.Products","+",round(coeff_Model5[3],4),"*","Eggs")
eq_Model5


print("Model 6")

#Correlation (R)
cor(train$Obesity , train$Animal.Products+train$Meat)
Model6 = lm(Obesity~Animal.Products+Meat,data=train)
Model6
#coefficient
coeff_Model6=coefficients(Model6)
coeff_Model6
#y=b0 + B!*Eggs
eq_Model6 = paste("Regression Line: y=" , round(coeff_Model6[1],4),"+",round(coeff_Model6[2],4),"*","Animal.Products","+",round(coeff_Model6[3],4),"*","Meat")
eq_Model6


print("Model 7")

#Correlation (R)
cor(train$Obesity , train$Animal.Products+train$Milk...Excluding.Butter)
Model7 = lm(Obesity~Animal.Products+Milk...Excluding.Butter,data=train)
Model7
#coefficient
coeff_Model7=coefficients(Model7)
coeff_Model7
#y=b0 + B!*Eggs
eq_Model7 = paste("Regression Line: y=" , round(coeff_Model7[1],4),"+",round(coeff_Model7[2],4),"*","Animal.Products","+",round(coeff_Model7[3],4),"*","Milk...Excluding.Butter")
eq_Model7


print("Model 8")

#Correlation (R)
cor(train$Obesity , train$Eggs+train$Meat)
Model8 = lm(Obesity~Eggs+Meat,data=train)
Model8
#coefficient
coeff_Model8=coefficients(Model8)
coeff_Model8
#y=b0 + B!*Eggs
eq_Model8 = paste("Regression Line: y=" , round(coeff_Model8[1],4),"+",round(coeff_Model8[2],4),"*","Eggs","+",round(coeff_Model8[3],4),"*","Meat")
eq_Model8


print("Model 9")

#Correlation (R)
cor(train$Obesity , train$Eggs+train$Milk...Excluding.Butter)
Model9 = lm(Obesity~Eggs+Milk...Excluding.Butter,data=train)
Model9
#coefficient
coeff_Model9=coefficients(Model9)
coeff_Model9
#y=b0 + B!*Eggs
eq_Model9 = paste("Regression Line: y=" , round(coeff_Model9[1],4),"+",round(coeff_Model9[2],4),"*","Eggs","+",round(coeff_Model9[3],4),"*","Milk...Excluding.Butter")
eq_Model9


print("Model 10")

#Correlation (R)
cor(train$Obesity , train$Meat+train$Milk...Excluding.Butter)
Model10 = lm(Obesity~Meat+Milk...Excluding.Butter,data=train)
Model10
#coefficient
coeff_Model10=coefficients(Model10)
coeff_Model10
#y=b0 + B!*Eggs
eq_Model10 = paste("Regression Line: y=" , round(coeff_Model10[1],4),"+",round(coeff_Model10[2],4),"*","Meat","+",round(coeff_Model10[3],4),"*","Milk...Excluding.Butter")
eq_Model10





print("Model 11")

#Correlation (R)
cor(train$Obesity , train$Animal.Products+train$Eggs+train$Meat)
Model11 = lm(Obesity~Animal.Products+Eggs+Meat ,data=train)
Model11
#coefficient
coeff_Model11=coefficients(Model11)
coeff_Model11
#y=b0 + B!*Eggs
eq_Model11 = paste("Regression Line: y=" , round(coeff_Model11[1],4),"+",round(coeff_Model11[2],4),"*","Animal.Products","+",round(coeff_Model11[3],4),"*","Eggs","+",round(coeff_Model11[4],4),"*","Meat")
eq_Model11


print("Model 12")

#Correlation (R)
cor(train$Obesity , train$Animal.Products+train$Eggs+train$Milk...Excluding.Butter)
Model12 = lm(Obesity~Animal.Products+Eggs+Milk...Excluding.Butter ,data=train)
Model12
#coefficient
coeff_Model12=coefficients(Model12)
coeff_Model12
#y=b0 + B!*Eggs
eq_Model12 = paste("Regression Line: y=" , round(coeff_Model12[1],4),"+",round(coeff_Model12[2],4),"*","Animal.Products","+",round(coeff_Model12[3],4),"*","Eggs","+",round(coeff_Model12[4],4),"*","Milk...Excluding.Butter")
eq_Model12




print("Model 13")

#Correlation (R)
cor(train$Obesity , train$Animal.Products+train$Meat+train$Milk...Excluding.Butter)
Model13 = lm(Obesity~Animal.Products+Meat+Milk...Excluding.Butter ,data=train)
Model13
#coefficient
coeff_Model13=coefficients(Model13)
coeff_Model13
#y=b0 + B!*Eggs
eq_Model13 = paste("Regression Line: y=" , round(coeff_Model13[1],4),"+",round(coeff_Model13[2],4),"*","Animal.Products","+",round(coeff_Model13[3],4),"*","Meat","+",round(coeff_Model13[4],4),"*","Milk...Excluding.Butter")
eq_Model13




print("Model 14")

#Correlation (R)
cor(train$Obesity , train$Eggs+train$Meat+train$Milk...Excluding.Butter)
Model14 = lm(Obesity~Eggs+Meat+Milk...Excluding.Butter ,data=train)
Model14
#coefficient
coeff_Model14=coefficients(Model14)
coeff_Model14
#y=b0 + B!*Eggs
eq_Model14 = paste("Regression Line: y=" , round(coeff_Model14[1],4),"+",round(coeff_Model14[2],4),"*","Eggs","+",round(coeff_Model14[3],4),"*","Meat","+",round(coeff_Model14[4],4),"*","Milk...Excluding.Butter")
eq_Model14



print("Model 15")

#Correlation (R)
cor(train$Obesity , train$Animal.Products+train$Eggs+train$Meat+train$Milk...Excluding.Butter)
Model15 = lm(Obesity~Animal.Products+Eggs+Meat+Milk...Excluding.Butter ,data=train)
Model15
#coefficient
coeff_Model15=coefficients(Model15)
coeff_Model15
#y=b0 + B!*Eggs
eq_Model15 = paste("Regression Line: y=" , round(coeff_Model15[1],4),"+",round(coeff_Model15[2],4),"*","Animal.Products","+",round(coeff_Model15[3],4),"*","Eggs","+",round(coeff_Model15[4],4),"*","Meat","+",round(coeff_Model15[5],4),"*","Milk...Excluding.Butter")
eq_Model15



print("Model validation")
print("*****************************************")

Model1_Predict = predict(Model1,train)
Model1_Predict
Model1_Predict_test = predict(Model1,test)
Model1_Predict_test

Model2_Predict = predict(Model2,train)
Model2_Predict
Model2_Predict_test = predict(Model2,test)
Model2_Predict_test

Model3_Predict = predict(Model3,train)
Model3_Predict
Model3_Predict_test = predict(Model3,test)
Model3_Predict_test

Model4_Predict = predict(Model4,train)
Model4_Predict
Model4_Predict_test = predict(Model4,test)
Model4_Predict_test

Model5_Predict = predict(Model5,train)
Model5_Predict
Model5_Predict_test = predict(Model5,test)
Model5_Predict_test

Model6_Predict = predict(Model6,train)
Model6_Predict
Model6_Predict_test = predict(Model6,test)
Model6_Predict_test

Model7_Predict = predict(Model7,train)
Model7_Predict
Model7_Predict_test = predict(Model7,test)
Model7_Predict_test

Model8_Predict = predict(Model8,train)
Model8_Predict
Model8_Predict_test = predict(Model8,test)
Model8_Predict_test

Model9_Predict = predict(Model9,train)
Model9_Predict
Model9_Predict_test = predict(Model9,test)
Model9_Predict_test

Model10_Predict = predict(Model10,train)
Model10_Predict
Model10_Predict_test = predict(Model10,test)
Model10_Predict_test

Model11_Predict = predict(Model11,train)
Model11_Predict
Model11_Predict_test = predict(Model11,test)
Model11_Predict_test

Model12_Predict = predict(Model12,train)
Model12_Predict
Model12_Predict_test = predict(Model12,test)
Model12_Predict_test

Model13_Predict = predict(Model13,train)
Model13_Predict
Model13_Predict_test = predict(Model13,test)
Model13_Predict_test

Model14_Predict = predict(Model14,train)
Model14_Predict
Model14_Predict_test = predict(Model14,test)
Model14_Predict_test

Model15_Predict = predict(Model15,train)
Model15_Predict
Model15_Predict_test = predict(Model15,test)
Model15_Predict_test



Model1_residuals = residuals(Model1)
Model1_residuals

Model2_residuals = residuals(Model2)
Model2_residuals

Model3_residuals = residuals(Model3)
Model3_residuals

Model4_residuals = residuals(Model4)
Model4_residuals

Model4_residuals = residuals(Model4)
Model4_residuals

Model5_residuals = residuals(Model5)
Model5_residuals

Model6_residuals = residuals(Model6)
Model6_residuals

Model7_residuals = residuals(Model7)
Model7_residuals

Model8_residuals = residuals(Model8)
Model8_residuals

Model9_residuals = residuals(Model9)
Model9_residuals

Model10_residuals = residuals(Model10)
Model10_residuals

Model11_residuals = residuals(Model11)
Model11_residuals

Model12_residuals = residuals(Model12)
Model12_residuals

Model13_residuals = residuals(Model13)
Model13_residuals


Model14_residuals = residuals(Model14)
Model14_residuals

Model15_residuals = residuals(Model15)
Model15_residuals


plot(Model1_residuals~Model1_Predict , main="Residual VS Predicted Plot" , col="red")
plot(Model2_residuals~Model2_Predict , main="Residual VS Predicted Plot" , col="blue")
plot(Model3_residuals~Model3_Predict , main="Residual VS Predicted Plot" , col="green")
plot(Model4_residuals~Model4_Predict , main="Residual VS Predicted Plot" , col="orange")
plot(Model5_residuals~Model5_Predict , main="Residual VS Predicted Plot" , col="red")
plot(Model6_residuals~Model6_Predict , main="Residual VS Predicted Plot" , col="blue")
plot(Model7_residuals~Model7_Predict , main="Residual VS Predicted Plot" , col="green")
plot(Model8_residuals~Model8_Predict , main="Residual VS Predicted Plot" , col="orange")
plot(Model9_residuals~Model9_Predict , main="Residual VS Predicted Plot" , col="red")
plot(Model10_residuals~Model10_Predict , main="Residual VS Predicted Plot" , col="blue")
plot(Model11_residuals~Model11_Predict , main="Residual VS Predicted Plot" , col="green")
plot(Model12_residuals~Model12_Predict , main="Residual VS Predicted Plot" , col="orange")
plot(Model13_residuals~Model13_Predict , main="Residual VS Predicted Plot" , col="red")
plot(Model14_residuals~Model14_Predict , main="Residual VS Predicted Plot" , col="blue")
plot(Model15_residuals~Model15_Predict , main="Residual VS Predicted Plot" , col="green")




bptest(Model1)
bptest(Model2)
bptest(Model3)
bptest(Model4)
bptest(Model5)
bptest(Model6)
bptest(Model7)
bptest(Model8)
bptest(Model9)
bptest(Model10)
bptest(Model11)
bptest(Model12)
bptest(Model13)
bptest(Model14)
bptest(Model15)


mse(train$Obesity , Model1_Predict)
mse(test$Obesity , Model1_Predict_test)
rmse(train$Obesity , Model1_Predict)
rmse(test$Obesity , Model1_Predict_test)
mae(train$Obesity , Model1_Predict)
mae(test$Obesity , Model1_Predict_test)
mape(train$Obesity , Model1_Predict)
mape(test$Obesity , Model1_Predict_test)

mse(train$Obesity , Model2_Predict)
mse(test$Obesity , Model2_Predict_test)
rmse(train$Obesity , Model2_Predict)
rmse(test$Obesity , Model2_Predict_test)
mae(train$Obesity , Model2_Predict)
mae(test$Obesity , Model2_Predict_test)
mape(train$Obesity , Model2_Predict)
mape(test$Obesity , Model2_Predict_test)

mse(train$Obesity , Model3_Predict)
mse(test$Obesity , Model3_Predict_test)
rmse(train$Obesity , Model3_Predict)
rmse(test$Obesity , Model3_Predict_test)
mae(train$Obesity , Model3_Predict)
mae(test$Obesity , Model3_Predict_test)
mape(train$Obesity , Model3_Predict)
mape(test$Obesity , Model3_Predict_test)

mse(train$Obesity , Model4_Predict)
mse(test$Obesity , Model4_Predict_test)
rmse(train$Obesity , Model4_Predict)
rmse(test$Obesity , Model4_Predict_test)
mae(train$Obesity , Model4_Predict)
mae(test$Obesity , Model4_Predict_test)
mape(train$Obesity , Model4_Predict)
mape(test$Obesity , Model4_Predict_test)

mse(train$Obesity , Model5_Predict)
mse(test$Obesity , Model5_Predict_test)
rmse(train$Obesity , Model5_Predict)
rmse(test$Obesity , Model5_Predict_test)
mae(train$Obesity , Model5_Predict)
mae(test$Obesity , Model5_Predict_test)
mape(train$Obesity , Model5_Predict)
mape(test$Obesity , Model5_Predict_test)

mse(train$Obesity , Model6_Predict)
mse(test$Obesity , Model6_Predict_test)
rmse(train$Obesity , Model6_Predict)
rmse(test$Obesity , Model6_Predict_test)
mae(train$Obesity , Model6_Predict)
mae(test$Obesity , Model6_Predict_test)
mape(train$Obesity , Model6_Predict)
mape(test$Obesity , Model6_Predict_test)

mse(train$Obesity , Model7_Predict)
mse(test$Obesity , Model7_Predict_test)
rmse(train$Obesity , Model7_Predict)
rmse(test$Obesity , Model7_Predict_test)
mae(train$Obesity , Model7_Predict)
mae(test$Obesity , Model7_Predict_test)
mape(train$Obesity , Model7_Predict)
mape(test$Obesity , Model7_Predict_test)

mse(train$Obesity , Model8_Predict)
mse(test$Obesity , Model8_Predict_test)
rmse(train$Obesity , Model8_Predict)
rmse(test$Obesity , Model8_Predict_test)
mae(train$Obesity , Model8_Predict)
mae(test$Obesity , Model8_Predict_test)
mape(train$Obesity , Model8_Predict)
mape(test$Obesity , Model8_Predict_test)

mse(train$Obesity , Model9_Predict)
mse(test$Obesity , Model9_Predict_test)
rmse(train$Obesity , Model9_Predict)
rmse(test$Obesity , Model9_Predict_test)
mae(train$Obesity , Model9_Predict)
mae(test$Obesity , Model9_Predict_test)
mape(train$Obesity , Model9_Predict)
mape(test$Obesity , Model9_Predict_test)

mse(train$Obesity , Model10_Predict)
mse(test$Obesity , Model10_Predict_test)
rmse(train$Obesity , Model10_Predict)
rmse(test$Obesity , Model10_Predict_test)
mae(train$Obesity , Model10_Predict)
mae(test$Obesity , Model10_Predict_test)
mape(train$Obesity , Model10_Predict)
mape(test$Obesity , Model10_Predict_test)

mse(train$Obesity , Model11_Predict)
mse(test$Obesity , Model11_Predict_test)
rmse(train$Obesity , Model11_Predict)
rmse(test$Obesity , Model11_Predict_test)
mae(train$Obesity , Model11_Predict)
mae(test$Obesity , Model11_Predict_test)
mape(train$Obesity , Model11_Predict)
mape(test$Obesity , Model11_Predict_test)

mse(train$Obesity , Model12_Predict)
mse(test$Obesity , Model12_Predict_test)
rmse(train$Obesity , Model12_Predict)
rmse(test$Obesity , Model12_Predict_test)
mae(train$Obesity , Model12_Predict)
mae(test$Obesity , Model12_Predict_test)
mape(train$Obesity , Model12_Predict)
mape(test$Obesity , Model12_Predict_test)

mse(train$Obesity , Model13_Predict)
mse(test$Obesity , Model13_Predict_test)
rmse(train$Obesity , Model13_Predict)
rmse(test$Obesity , Model13_Predict_test)
mae(train$Obesity , Model13_Predict)
mae(test$Obesity , Model13_Predict_test)
mape(train$Obesity , Model13_Predict)
mape(test$Obesity , Model13_Predict_test)

mse(train$Obesity , Model14_Predict)
mse(test$Obesity , Model14_Predict_test)
rmse(train$Obesity , Model14_Predict)
rmse(test$Obesity , Model14_Predict_test)
mae(train$Obesity , Model14_Predict)
mae(test$Obesity , Model14_Predict_test)
mape(train$Obesity , Model14_Predict)
mape(test$Obesity , Model14_Predict_test)

mse(train$Obesity , Model15_Predict)
mse(test$Obesity , Model15_Predict_test)
rmse(train$Obesity , Model15_Predict)
rmse(test$Obesity , Model15_Predict_test)
mae(train$Obesity , Model15_Predict)
mae(test$Obesity , Model15_Predict_test)
mape(train$Obesity , Model15_Predict)
mape(test$Obesity , Model15_Predict_test)




#Akaike Info. Criteria
AIC(Model1)
AIC(Model2)
AIC(Model3)
AIC(Model4)
AIC(Model5)
AIC(Model6)
AIC(Model7)
AIC(Model8)
AIC(Model9)
AIC(Model10)
AIC(Model11)
AIC(Model12)
AIC(Model13)
AIC(Model14)
AIC(Model15)







#Bayesian Info. Criteria
BIC(Model1)
BIC(Model2)
BIC(Model3)
BIC(Model4)
BIC(Model5)
BIC(Model6)
BIC(Model7)
BIC(Model8)
BIC(Model9)
BIC(Model10)
BIC(Model11)
BIC(Model12)
BIC(Model13)
BIC(Model14)
BIC(Model15)




#library(car)
#Durbin Watson Statistics
durbinWatsonTest(Model1)
durbinWatsonTest(Model2)
durbinWatsonTest(Model3)
durbinWatsonTest(Model4)
durbinWatsonTest(Model5)
durbinWatsonTest(Model6)
durbinWatsonTest(Model7)
durbinWatsonTest(Model8)
durbinWatsonTest(Model9)
durbinWatsonTest(Model10)
durbinWatsonTest(Model11)
durbinWatsonTest(Model12)
durbinWatsonTest(Model13)
durbinWatsonTest(Model14)
durbinWatsonTest(Model15)





#Variance Inflation Factor
#VIF - Minimum two independent variable is required

vif(Model5)
vif(Model6)
vif(Model7)
vif(Model8)
vif(Model9)
vif(Model10)
vif(Model11)
vif(Model12)
vif(Model13)
vif(Model14)
vif(Model15)

summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)
summary(Model5)
summary(Model6)
summary(Model7)
summary(Model8)
summary(Model9)
summary(Model10)
summary(Model11)
summary(Model12)
summary(Model13)
summary(Model14)
summary(Model15)

names(Protein_Data)



# Kfold

library(caret)
ctrl <- trainControl(method = "cv", number = 10)

model1 <- train(Obesity~Animal.Products, data = Protein_Data, method = "lm", trControl = ctrl)
print(model1)

model2 <- train(Obesity~Eggs, data = Protein_Data, method = "lm", trControl = ctrl)
print(model2)

model3 <- train(Obesity~Meat, data = Protein_Data, method = "lm", trControl = ctrl)
print(model3)

model4 <- train(Obesity~Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model4)

model5 <- train(Obesity~Animal.Products, data = Protein_Data, method = "lm", trControl = ctrl)
print(model5)

model6 <- train(Obesity~Animal.Products+Meat, data = Protein_Data, method = "lm", trControl = ctrl)
print(model6)

model7<- train(Obesity~Animal.Products+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model7)

model8 <- train(Obesity~Eggs+Meat, data = Protein_Data, method = "lm", trControl = ctrl)
print(model8)

model9 <- train(Obesity~Eggs+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model9)

model10 <- train(Obesity~Meat+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model10)

model11 <- train(Obesity~Animal.Products+Eggs+Meat, data = Protein_Data, method = "lm", trControl = ctrl)
print(model11)

model12 <- train(Obesity~Animal.Products+Eggs+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model12)

model13 <- train(Obesity~Animal.Products+Meat+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model13)

model14 <- train(Obesity~Eggs+Meat+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model14)
      
model15 <- train(Obesity~Animal.Products+Eggs+Meat+Milk...Excluding.Butter, data = Protein_Data, method = "lm", trControl = ctrl)
print(model15)  

model15$resample
model10$resample





# SVM REGRESSION

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


model1 <- train(Obesity~Animal.Products, data = Protein_Data, method = "svmLinear", trControl = ctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model1)

model2 <- train(Obesity~Eggs, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model2)

model3 <- train(Obesity~Meat, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model3)

model4 <- train(Obesity~Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model4)

model5 <- train(Obesity~Animal.Products, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model5)

model6 <- train(Obesity~Animal.Products+Meat, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model6)

model7<- train(Obesity~Animal.Products+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model7)

model8 <- train(Obesity~Eggs+Meat, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model8)

model9 <- train(Obesity~Eggs+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model9)

model10 <- train(Obesity~Meat+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model10)

model11 <- train(Obesity~Animal.Products+Eggs+Meat, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model11)

model12 <- train(Obesity~Animal.Products+Eggs+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model12)

model13 <- train(Obesity~Animal.Products+Meat+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model13)

model14 <- train(Obesity~Eggs+Meat+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model14)

model15 <- train(Obesity~Animal.Products+Eggs+Meat+Milk...Excluding.Butter, data = Protein_Data, method = "svmLinear", trControl = trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(model15)  


library(rpart)

model1 <- train(Obesity~Animal.Products, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model1
plot(model1)

model2 <- train(Obesity~Eggs, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model2
plot(model2)

model3 <- train(Obesity~Meat, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model3
plot(model3)

model4 <- train(Obesity~Milk...Excluding.Butter, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model4
plot(model4)

model5 <- train(Obesity~Animal.Products+Eggs, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model5
plot(model5)

model6 <- train(Obesity~Animal.Products+Meat, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model6
plot(model6)

model7 <- train(Obesity~Animal.Products+Milk...Excluding.Butter, data = Protein_Data, preProcess = c("center", "scale"),method = 'rpart2')
model7
plot(model7)

model8 <- train(Obesity~Eggs+Meat, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model8
plot(model8)

model9 <- train(Obesity~Eggs+Milk...Excluding.Butter, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model9
plot(model9)

model10 <- train(Obesity~Meat+Milk...Excluding.Butter, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model10
plot(model10)

model11 <- train(Obesity~Animal.Products+Eggs+Meat, data = Protein_Data, preProcess = c("center", "scale"),method = 'rpart2')
model11
plot(model11)

model12 <- train(Obesity~Animal.Products+Eggs+Milk...Excluding.Butter, data = Protein_Data,preProcess = c("center", "scale"), method = 'rpart2')
model12
plot(model12)

model13 <- train(Obesity~Animal.Products+Meat+Milk...Excluding.Butter, data = Protein_Data, preProcess = c("center", "scale"), method = 'rpart2')
model13
plot(model13)

model14 <- train(Obesity~Eggs+Meat+Milk...Excluding.Butter, data = Protein_Data, preProcess = c("center", "scale"), method = 'rpart2')
model14
plot(model14)

model15 <- train(Obesity~Animal.Products+Eggs+Meat+Milk...Excluding.Butter, data = Protein_Data, preProcess = c("center", "scale"), method = 'rpart2')
model15
plot(model15)
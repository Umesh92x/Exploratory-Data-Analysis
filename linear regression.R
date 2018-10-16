data('mtcars')
cars<-mtcars
head(cars)
str(cars)
hist(cars$mpg)
boxplot(cars$mpg)
pairs(cars[1:6])

plot(cars$wt,cars$mpg) # scatter plot
cor(cars$mpg,cars$wt)
plot(cars$wt,cars$mpg)
#abline(cars$mpg)
#simple linear regression
carslm1<-lm(mpg~wt, data= cars)
carslm1$coefficients
carslm1$fitted.values


plot(cars$wt,carslm1$fitted.values)
abline(carslm1)

cars$mpg-carslm1$fitted.values
RSS1<-sum((cars$mpg-carslm1$fitted.values)^2) # finding RSS
mean(carslm1$residuals)# residual must follow mean equal to zeros 
# residual must follow normal distribution

shapiro.test(carslm1$residuals)

mean(cars$mpg)
TSS<-sum((cars$mpg-20)^2)
R_sq=(TSS-RSS1)/TSS
R_sq

summary(carslm1)
summary(carslm1)$r.squared

summary(carslm1)$adj.r.squared
RSE<-sqrt(RSS1/(nrow(cars)-ncol(cars)-1))
RSE
summary(carslm1)$sigma

confint(carslm1)
plot(carslm1,1)


#*************************************************
#multiple linear regression
carslm2<-lm(mpg~wt+hp,data=cars)
summary(carslm2)
RSS2<-sum((cars$mpg-carslm2$fitted.values)^2) # finding RSS
RSS2

plot(carslm2,1)

cars$cyl<-as.factor(cars$cyl)
six<-mean(cars[c(cars$cyl==6),1])
eight<-mean(cars[c(cars$cyl==8),1])
four<-mean(cars[c(cars$cyl==4),1])
six
eight
four
tapply(cars$mpg,cars$cyl,mean)

mul_lm<-lm(mpg~cyl,data=cars)
mul_lm$coefficients
contrasts(cars$cyl)
summary(mul_lm)
RSS3<-sum((cars$mpg-mul_lm$fitted.values)^2) # finding RSS
RSS3

mul_lm2<-lm(mpg~wt+hp+cyl,data=cars)
mul_lm2
RSS4<-sum((cars$mpg-mul_lm2$fitted.values)^2) # finding RSS
RSS4
summary(mul_lm2)

mul_lm3<-lm(mpg~wt+hp+cyl+wt*hp,data=cars)
mul_lm3$coefficients
RSS5<-sum((cars$mpg-mul_lm3$fitted.values)^2) # finding RSS
RSS5
summary(mul_lm3)


mul_lm4<-lm(mpg~wt+hp+wt*hp,data=cars)
mul_lm4$coefficients
RSS5<-sum((cars$mpg-mul_lm4$fitted.values)^2) # finding RSS
RSS5
summary(mul_lm4)
head(cars)
#***********************    continue    *****************
# Polinomial ........
plot(cars$hp,cars$mpg)
plot(cars$drat,cars$mpg)
plot(cars$qsec,cars$mpg)
plot(cars$disp,cars$mpg)

mul_lm5<-lm(mpg~I(hp^2)+hp,data=cars)
#OR

RSS6<-sum((cars$mpg-mul_lm4$fitted.values)^2)
RSS6
summary(mul_lm4)

mul_lm5<-lm(mpg~I(hp^2)+hp,data=cars)
RSS7<-sum((cars$mpg-mul_lm5$fitted.values)^2)
RSS7
summary(mul_lm5)

mul_lm6<-lm(mpg~poly(hp,3)+hp+I(hp^2)+I(hp^3),data=cars)
RSS8<-sum((cars$mpg-mul_lm6$fitted.values)^2)
RSS8
summary(mul_lm6)

# Adding Hp^2

mul_lm7<-lm(mpg~wt+hp+wt*hp+I(hp^2),data=cars)
mul_lm7$coefficients
RSS9<-sum((cars$mpg-mul_lm7$fitted.values)^2) # finding RSS
RSS9
summary(mul_lm7)


mul_lm8<-lm(mpg~.+wt*hp+I(hp^2),data=cars)
mul_lm8$coefficients
RSS10<-sum((cars$mpg-mul_lm7$fitted.values)^2) # finding RSS
RSS10
summary(mul_lm8)

anova(carslm1,carslm2,mul_lm4,mul_lm7,mul_lm8)
# R2 adj and RSE choose min

# Prediction
new_car_data<-data.frame(wt=c(2.5,3.5,4.5,5.5),
                          hp=c(100,200,250,350))
new_car_data

pred1<-predict(carslm1,data=new_car_data)
pred2<-predict(carslm2,data=new_car_data)
pred3<-predict(mul_lm4,data=new_car_data)
cbind(pred1,pred2,pred3)
head(cars)
# load Hitters
new_car_data<-(data.frame(wt=c(2.5,3.5,4.5,5.5),
                          hp=c(100,200,250,350)))

actual<-predict(carslm1,data=cars)
actual

RSS_pred1<-sum((cars$mpg-pred1)^2) # finding RSS
RSS_pred1


RSS_pred2<-sum((cars$mpg-pred2)^2) # finding RSS
RSS_pred2

RSS_pred3<-sum((cars$mpg-pred3)^2) # finding RSS
RSS_pred3

# 1 part is EDA.
# 2nd part is Model making
# 3rd is prediction
# *************************************   EDA   **************************************
#loading data from R documentation
data('mtcars')
cars<-mtcars
# Just look first 6 row
head(cars)
# lets see structre of data
str(cars)
# making histogram, box plot and matrix plot
hist(cars$mpg)
boxplot(cars$mpg)
pairs(cars[1:6])

plot(cars$wt,cars$mpg) # scatter plot,bi variate analysis
cor(cars$mpg,cars$wt)  # correlation coeff
#simple linear regression
carslm1<-lm(mpg~wt, data= cars)   # Making model linear regression
carslm1$coefficients              # printing intercept and slop
carslm1$fitted.values             # print predict value Y cap

plot(cars$wt,carslm1$fitted.values) # plot between x= wt and y= predict value y cap
abline(carslm1)                     # best fit line

RSS1<-sum((cars$mpg-carslm1$fitted.values)^2) # finding RSS, Residual sum of square
mean(carslm1$residuals)
#  ***********************Assumption
# residual must follow mean equal to zeros and 
# residual must follow normal distribution. So lets test it. using shapiro test

shapiro.test(carslm1$residuals)    
mean(cars$mpg)
TSS<-sum((cars$mpg-20)^2) # Max error Tss calculated     
R_sq=(TSS-RSS1)/TSS       # Rsquared values
R_sq

summary(carslm1)         # let print individual, group p value, R and R sq value, 
# RSE and F statistic value and explore these for EDA 
summary(carslm1)$r.squared    # individal R sq value

summary(carslm1)$adj.r.squared   # individal R adj value
RSE<-sqrt(RSS1/(nrow(cars)-ncol(cars)-1))   # calculate RSE using FORMULA
RSE
summary(carslm1)$sigma         # print RES directly 

confint(carslm1)# finding confidence interval 
plot(carslm1,1)    # this plot based of resudual value, where is positive and where is negative,this graph look bent because both side in graph of linear model it is postive residula and in middle it is negative residual.


#************************          MULTIPLE LINEAR REGRESSION
#multiple linear regression
carslm2<-lm(mpg~wt+hp,data=cars)  # using weight and hourse power.
summary(carslm2)                  # print complete set of analysis.
RSS2<-sum((cars$mpg-carslm2$fitted.values)^2) # finding RSS
RSS2

plot(carslm2,1) # Plot again residual.

cars$cyl<-as.factor(cars$cyl)  # cyl look like categorical in data set but in number so we convert it into categorical. 
tapply(cars$mpg,cars$cyl,mean) # according to cyl finding mean of mpg

mul_lm<-lm(mpg~cyl,data=cars)  # lets take mileage and cylinder.
mul_lm$coefficients     # print intecet and slop of each variable
contrasts(cars$cyl)   # find dummy variable.
summary(mul_lm) # print all analysis
RSS3<-sum((cars$mpg-mul_lm$fitted.values)^2) # finding RSS
RSS3

mul_lm2<-lm(mpg~wt+hp+cyl,data=cars) # lets take three variable and again do analysis
mul_lm2
RSS4<-sum((cars$mpg-mul_lm2$fitted.values)^2) # finding RSS
RSS4
summary(mul_lm2)

mul_lm3<-lm(mpg~wt+hp+cyl+wt*hp,data=cars) # Again take all three and interaction effect wt*hp 
mul_lm3$coefficients
RSS5<-sum((cars$mpg-mul_lm3$fitted.values)^2) # finding RSS
RSS5
summary(mul_lm3)

mul_lm4<-lm(mpg~wt+hp+wt*hp,data=cars)  # we removed cyl, because it gives high P value, High RSS and HIGh F statistic 
mul_lm4$coefficients
RSS5<-sum((cars$mpg-mul_lm4$fitted.values)^2) # finding RSS
RSS5
summary(mul_lm4)
head(cars)
# *************************************   Polinomial 
plot(cars$hp,cars$mpg)   #lets visualise hp
mul_lm5<-lm(mpg~I(hp^2)+hp,data=cars) #fitting mpg and hp with power of 2.
RSS7<-sum((cars$mpg-mul_lm5$fitted.values)^2)  
RSS7
summary(mul_lm5)

mul_lm6<-lm(mpg~poly(hp,3)+hp+I(hp^2)+I(hp^3),data=cars)  # this time power of 3 of hp
RSS8<-sum((cars$mpg-mul_lm6$fitted.values)^2)
RSS8
summary(mul_lm6)

# ________________________   After finalise we choose hp with power of 2, it give high f statistic, low RSS, High R adj.
# Adding Hp^2 to final model

mul_lm7<-lm(mpg~wt+hp+wt*hp+I(hp^2),data=cars) # final model fit.
mul_lm7$coefficients  # intercetp and slops
RSS9<-sum((cars$mpg-mul_lm7$fitted.values)^2) # finding RSS
RSS9
summary(mul_lm7)


mul_lm8<-lm(mpg~.+wt*hp+I(hp^2),data=cars) # this time chosse every variable, and hp of power of 2.
mul_lm8$coefficients
RSS10<-sum((cars$mpg-mul_lm7$fitted.values)^2) # finding RSS

summary(mul_lm8)

anova(carslm1,carslm2,mul_lm4,mul_lm7,mul_lm8)
# NOTE : # R2 adj and RSE choose min

# Prediction
new_car_data<-data.frame(wt=c(2.5,3.5,4.5,5.5),
                          hp=c(100,200,250,350))  # lets predict on our data set.


pred1<-predict(carslm1,data=new_car_data) # wt and mpg model
pred2<-predict(carslm2,data=new_car_data) # wt mpg and hp
pred3<-predict(mul_lm4,data=new_car_data) # wt mpg and wt*hp
cbind(pred1,pred2,pred3) # let analysis them to see which is better predict.

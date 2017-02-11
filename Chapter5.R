sample()
library(ISLR)
attach(Auto)
set.seed(3)
train=sample(392,196)
head(train)
train
lm.fit=lm(mpg~horsepower,data=Auto,subset = train)
summary(lm.fit)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,3),data=Auto,subset = train)
summary(lm.fit2)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
####
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
summary(lm.fit3)

#####
glm.fit=glm(mpg~horsepower,data = Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.err$call
cv.err$K
cv.err$seed
cv.err$delta

cv.error=rep(0,5)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
  }
cv.error

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1] #note K not k
}
cv.error.10

#####
set.seed(2)
train=sample(392,196)
attach(Portfolio)
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/var(x)+var(Y)-2*cov(X,Y))
}
Portfolio$X
alpha.fn(Portfolio,1:100)

set.seed(2)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio, alpha.fn, R=1000)

#####
library(ISLR)
set.seed(1)
train=sample(392,196)
head(train)
train
lm.fit=lm(mpg~horsepower,data=Auto,subset = train)
summary(lm.fit)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset = train)
summary(lm.fit2)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
####
set.seed(3)
library(ISLR)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train,data=Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
summary(lm.fit3)

library(boot)
fit.glm6=glm(mpg~poly(horsepower,6),data=Auto,subset = train)
summary(fit.glm6)
cv.glm(Auto,fit.glm6)[6]

####
set.seed(2)

boot.fn=function(data,index) 
  + coefficients(lm(formula = mpg∼horsepower+I(horsepower^2),data=Auto,subset=index))
set.seed(2)
boot(Auto,boot.fn,1000)

library(boot)
library(MASS)
library(ISLR)

data(Default)

set.seed(12345)
glmB = glm(default ~ income + balance, Default, family="binomial")

train1 = Default[1:6000, ]
test1 = Default[6001:10000, ]
train2 = Default[4001:10000, ]
test2 = Default[1:4000, ]

glm1 = glm(default ~ income + balance, train1, family = "binomial")
glm1.prob = predict(glm1, test1, type="response")
glm1.pred = rep("No", nrow(test1))
glm1.pred[glm1.prob > 0.5] = "Yes"
table(glm1.pred, test1$default)
1 - (3852+44)/(3852+44+90+14)

#######

glm2 = glm(default ~ income + balance, train2, family = "binomial")
glm1.prob2 = predict(glm2, test2, type="response")
glm1.pred2 = rep("No", nrow(test2))
glm1.pred2[glm1.prob2 > 0.5] = "Yes"
table(glm1.pred2, test2$default)
1 - (3848+41)/(3848+41+97+14)

glm = glm(default ~ income + balance + student, train1, family = "binomial")
glm.prob = predict(glm, test1, type="response")
glm.pred = rep("No", nrow(test1))
glm.pred[glm.prob > 0.5] = "Yes"
table(glm.pred, test1$default)
1 - (3848+42)/(3848+42+92+18)

summary(glmB)

boot.fn=function(Default, index){
  glm10=glm(default~income+balance, Default, subset=index, family = "binomial")
  glm10$coefficients[2:3]
}
data(Weekly)

library("car")
library("lmtest")
library("sandwich")
library("mlbench")

## linear regression
data("BostonHousing")
lm.boston <-  lm(medv ~ ., data = BostonHousing)
dat.reg <-  data.frame(coef = coef(lm.boston),
                     hmsk = diag(vcov(lm.boston))^(0.5),
                     
                     hccm0 = diag(hccm(lm.boston, type = "hc0"))^(0.5),
                     sandwich0 = diag(sandwich(lm.boston, adjust = FALSE))^(0.5),
                     vcovHC0 = diag(vcovHC(lm.boston, type = "HC0"))^(0.5),
                     
                     hccm1 = diag(hccm(lm.boston, type = "hc1"))^(0.5),
                     sandwich1 = diag(sandwich(lm.boston, adjust = TRUE))^(0.5),
                     vcovHC1 = diag(vcovHC(lm.boston, type = "HC1"))^(0.5),
                     
                     hccm3 = diag(hccm(lm.boston, type = "hc3"))^(0.5),
                     vcovHC3 = diag(vcovHC(lm.boston, type = "HC3"))^(0.5))
round(dat.reg[-1, ], 2) 


## logistic regression
setwd("/Users/Zhichao/Dropbox/courses/SYSU/linear\ model/lectures/code/")
flu <-  read.table("flu240.txt", header = TRUE) 
flu <-  within(flu, rm(receive))
assign.logit <-  glm(outcome ~ ., 
                   family  = binomial(link = logit), 
                   data    = flu)
summary(assign.logit) 
coeftest(assign.logit, vcov = sandwich)


## freedman's logit
n <-  100
x <-  runif(n, 0, 10)
prob.x <-  1/(1 + exp(3*x - 0.5*x^2))
y <-  rbinom(n, 1, prob.x)
freedman.logit <-  glm(y ~ x, family = binomial(link = logit))
summary(freedman.logit)
coeftest(freedman.logit, vcov = sandwich)


## poisson regression
n <-  1000
x <-  rnorm(n)
lambda.x <-  exp(x/5)
y <-  rpois(n, lambda.x)
pois.pois <-  glm(y ~ x, family = poisson(link = log))
summary(pois.pois)
coeftest(pois.pois, vcov = sandwich)

library(MASS)
theta <-  0.2
y <-  rnegbin(n, mu = lambda.x, theta = theta)
nb.pois <-  glm(y ~ x, family = poisson(link = log))
summary(nb.pois)
coeftest(nb.pois, vcov = sandwich)

nb.nb <-  glm.nb(y ~ x)
summary(nb.nb)
coeftest(nb.nb, vcov = sandwich)

## misspecified mean function 
lambda.x <-  x^2
y <-  rpois(n, lambda.x)
wr.pois <-  glm(y ~ x, family = poisson(link = log))
summary(wr.pois)
coeftest(wr.pois, vcov = sandwich)

wr.nb <-  glm.nb(y ~ x)
summary(wr.nb)
coeftest(wr.nb, vcov = sandwich)




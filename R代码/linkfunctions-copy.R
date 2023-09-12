n <- 100
x <- rnorm(n,0,3)
prob <- 1/(1+exp(-1+x))
y <- rbinom(n,1,prob)               #伯努利分布

### 
lpmfit <- lm(y~x)
probitfit <- glm(y~x,family = binomial(link="probit"))
logitfit <- glm(y~x,family = binomial(link="logit"))
cloglogfit <- glm(y~x,family = binomial(link="cloglog"))
cauchitfit <- glm(y~x,family = binomial(link="cauchit"))

### coefficients
betacoef <- c(lpmfit$coef[2],
              probitfit$coef[2],
              logitfit$coef[2],
              cloglogfit$coef[2],
              cauchitfit$coef[2])
names(betacoef) <- c("lpm","probit","logit","cloglog","cauchit")
round(betacoef,2)

## prediction
table(y,lpmfit$fitted.values>0.5)
table(y,probitfit$fitted.values>0.5)
table(y,logitfit$fitted.values>0.5)
table(y,cloglogfit$fitted.values>0.5)
table(y,cauchitfit$fitted.values>0.5)


### logit and probit
d.logit.probit <- function(b){
  x <- seq(-20,20,0.00001)
  max(abs(plogis(b*x)-pnorm(x)))     #b*x是因为logit函数的参数是beta*x
}
#plogis 表示logit的分布函数
#d表示密度函数 dlogis  
#q表示分位数函数
#r表示对应分布生成随机数
optimize(d.logit.probit,c(-10,10))    #区间（-10，10）内求最小值

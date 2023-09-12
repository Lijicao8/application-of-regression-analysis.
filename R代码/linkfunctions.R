n <- 100
x <- rnorm(n,0,3)
prob <- 1/(1+exp(-1+x))
y <- rbinom(n,1,prob)

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
  max(abs(plogis(b*x)-pnorm(x)))
}

optimize(d.logit.probit,c(-10,10))
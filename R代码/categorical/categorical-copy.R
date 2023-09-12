setwd("/Users/lijicao/Desktop/数学寄！/R语言/应用回归分析/categorical")
karolinska <- read.table("karolinska.txt", header=TRUE)      #header表示变量名是否作为第一行
karolinska <- karolinska[,c(2,3,4,5,7)]
names(karolinska) <- c("hightreat","age","rural","male","survival")


### binary logistic regression for the treatment
treatglm <- glm(hightreat~age+rural+male,data=karolinska,family=binomial(link="logit"))
# family表示分布族，这里表示分布族用的是伯努利分布 链接函数用的是logit
summary(treatglm)


### binary logistic regression for the outcome
karolinska$loneyear <- (karolinska$survival != "1")
loneyearglm <- glm(loneyear~hightreat+age+rural+male,data=karolinska,family=binomial(link="logit"))
summary(loneyearglm)


#### multinomial logistic regression for the outcome
library(nnet)
yearmultinom <- multinom(survival~hightreat+age+rural+male,data=karolinska)
summary(yearmultinom)

predict(yearmultinom,type="probs")[1:5,]

#### proportional odds logist for the outcome
library(MASS)
yearpo<-polr(as.factor(survival)~hightreat+age+rural+male,data =karolinska,Hess=TRUE)
summary(yearpo)

predict(yearpo,type="probs")[1:5,]

##### mlogit
library(mlogit)
data(Fishing)
head(Fishing)


Fish.logit1 <- mlogit(mode~0+price+catch, data =Fish)
summary(Fish.logit1)

Fish.logit2 <- mlogit(mode~price+catch, data =Fish)
summary(Fish.logit2)

Fish.logit3 <- mlogit(mode~0|income, data =Fish)
summary(Fish.logit3)

Fish.logit4 <- mlogit(mode~price+catch|income, data =Fish)
summary(Fish.logit4)





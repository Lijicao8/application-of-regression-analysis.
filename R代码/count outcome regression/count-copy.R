library(foreign)
library(MASS)
gym1 <- read.dta("/Users/lijicao/Desktop/数学寄！/R语言/应用回归分析/count outcome regression/gym_treatment_exp_weekly.dta")
f.reg <- weekly_visit ~ incentive_commit+ incentive+target+member_gym_pre

weekids <- sort(unique(gym1$incentive_week))  #sort是排序函数  unique 删去相同变量
lweekids <- length(weekids)           
coefincentivecommit <- 1:lweekids
coefincentive <- 1:lweekids
seincentivecommit <- 1:lweekids
seincentive <- 1:lweekids
AIClm <- 1:lweekids


### linear probability model
for(i in 1:lweekids){
  gymweek <- gym1[gym1$incentive_week== weekids[i],]
  regweek <- lm(f.reg,data=gymweek)
  regweekcoef <- summary(regweek)$coef
  
  ##求系数
  coefincentivecommit[i] <- regweekcoef[2,1]
  coefincentive[i] <- regweekcoef[3,1]
  ##求标准误
  seincentivecommit[i] <- regweekcoef[2,2]
  seincentive[i] <- regweekcoef[3,2]
  
  AIClm[i] <- AIC(regweek)
}

### for Poisson and NB regressions change the line with lm by
# regweek = glm(f.reg, family = poisson(link = "log"), data = gymweek)
#  regweek = glm.nb(f.reg, data = gymweek)

### zero-inflated regression
library(pscl)

coefincentivecommit0 <- coefincentivecommit
coefincentive0 <- coefincentive
seincentivecommit0 <- seincentivecommit
seincentive0 <- seincentive
AIC0poisson <- AIClm


### zero-inflated poisson
for(i in 1:lweekids){
  gymweek <- gym1[gym1$incentive_week== weekids[i],]
  regweek <- zeroinfl(f.reg, dist="poisson",data=gymweek)##进行两个回归，一个是泊松回归，一个是零膨胀泊松回归
  regweekcoef <- summary(regweek)$coef
  
  coefincentivecommit[i] <- regweekcoef$count[2,1]
  coefincentive[i] <- regweekcoef$count[3,1]
  seincentivecommit[i] <- regweekcoef$count[2,2]
  seincentive[i] <- regweekcoef$count[3,2]
  
  coefincentivecommit0[i] <- regweekcoef$zero[2,1]
  coefincentive0[i] <- regweekcoef$zero[3,1]
  seincentivecommit0[i] <- regweekcoef$zero[2,2]
  seincentive0[i] <- regweekcoef$zero[3,2]
  
  AIC0poisson[i] <- AIC(regweek)
}

### for zero-inflated NB, replace the line with zerolinfl by
# zeroinfl(f.reg, dist="negbin",data=gymweek)
     #进行NB回归时会返回log（theta）的估计值

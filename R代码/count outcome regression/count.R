library(foreign)
library(MASS)
gym1 <- read.dta("/Users/Zhichao/Dropbox/courses/SYSU/linear\ model/lectures/code/gym_treatment_exp_weekly.dta")
f.reg <- weekly_visit ~ incentive_commit+ incentive+target+member_gym_pre

weekids <- sort(unique(gym1$incentive_week))
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
  
  coefincentivecommit[i] <- regweekcoef[2,1]
  coefincentive[i] <- regweekcoef[3,1]
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
  regweek <- zeroinfl(f.reg, dist="poisson",data=gymweek)
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



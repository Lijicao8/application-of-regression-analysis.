library(gee)
setwd("/Users/Zhichao/Dropbox/courses/SYSU/linear\ model/lectures/code/")

### neuroscience experiment
Pten <- read.csv("PtenAnalysisData.csv")[,-(7:9)]

Pten.gee <- gee(somasize~factor(fa)*pten,id=mouseid,
                family=gaussian,
                corstr="independence",
                data=Pten)

summary(Pten.gee)$coef

Pten.gee <- gee(somasize~factor(fa)*pten,id=mouseid,
                family=gaussian,
                corstr="exchangeable",
                data=Pten)

summary(Pten.gee)$coef


#### public health intervention
hygaccess <- read.csv("hygaccess.csv")
hygaccess <- hygaccess[,c("r4_hyg_access","treat_cat_1",
                          "bl_c_hyg_access","vid","eligible")]
hygaccess <- hygaccess[hygaccess$eligible=="Eligible" & hygaccess$r4_hyg_access!="Missing",]
hygaccess$y <- ifelse(hygaccess$r4_hyg_access=="Yes",1,0)
hygaccess$z <- hygaccess$treat_cat_1
hygaccess$x <- hygaccess$bl_c_hyg_access

hygaccess.gee <- gee(y~z,id=vid,
                     family=binomial(link=logit),
                     corstr="independence",
                     data=hygaccess)
summary(hygaccess.gee)$coef


hygaccess.gee <- gee(y~z,id=vid,
                     family=binomial(link=logit),
                     corstr="exchangeable",
                     data=hygaccess)
summary(hygaccess.gee)$coef

### longitudinal data: gym data
library("foreign")
gym1 <- read.dta("gym_treatment_exp_weekly.dta")
f.reg <- weekly_visit ~ incentive_commit+ incentive+target+member_gym_pre


## normal gee
normal.gee <- gee(f.reg,id=id,
                  family=gaussian,
                  corstr="independence",
                  data=gym1)
summary(normal.gee)$coef

normal1.gee <- gee(f.reg,id=id,
                    subset=(incentive_week<=0),
                   family=gaussian,
                   corstr="independence",
                   data=gym1)
summary(normal1.gee)$coef

normal2.gee <- gee(f.reg,id=id,
                   subset=(incentive_week>0&incentive_week<15),
                   family=gaussian,
                   corstr="independence",
                   data=gym1)
summary(normal2.gee)$coef

## poisson gee

poisson.gee1 = gee(weekly_visit ~ incentive_commit+ incentive+target, 
                   id = id, 
                   subset = (incentive_week<0),
                   family = poisson(link = log), 
                   corstr = "independence", 
                   data = gym1)
poisson.gee1 = summary(poisson.gee1)$coef
poisson.gee1


poisson.gee2 = gee(f.reg, id = id, 
                   subset = (incentive_week>0&incentive_week<15),
                   family = poisson(link = log), 
                   corstr = "independence", 
                   data = gym1)
poisson.gee2 = summary(poisson.gee2)$coef
poisson.gee2

poisson.gee3 = gee(f.reg, id = id, 
                   subset = (incentive_week>=15),
                   family = poisson(link = log), 
                   corstr = "independence", 
                   data = gym1)
poisson.gee3 = summary(poisson.gee3)$coef 
poisson.gee3


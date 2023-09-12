library("mlbench")
library("glmnet")
library("MASS")
data(BostonHousing)


### training and testing data
set.seed(2022)  
   #set.seed 表示设置一个种子 后面程序生成的随机数是依赖这个种子生成的
  #这样就能保证后面每次norm（）生成的随机数都是随机但是相同的
nsample <- dim(BostonHousing)[1]                  
trainindex <- sample(1:nsample, floor(nsample*0.9))     #floor向上取整函数
#sample 表示随机抽样函数  这里指的是从1～n中随机抽 0.9n个指标  训练集
#eg： n=10时   从1～10里随机抽9个  8，5，4，3，2，7，6，1，10
#目的：将数据分为两组 一组作为测试集，一组作为训练集
xmatrix <- model.matrix(medv~.,data=BostonHousing)[,-1]     #创立设计矩阵
yvector <- BostonHousing$medv
dat <- data.frame(yvector,xmatrix)

## linear regression
bostonlm <- lm(yvector~.,data=dat[trainindex,])
predicterror <- dat$yvector[-trainindex]-predict(bostonlm,dat[-trainindex,])#测试集检验
mse.ols <- sum(predicterror^2)/length(predicterror)     

## ridge regression
lambdas <- seq(0,5,0.01)
lm0 <- lm.ridge(yvector~.,data=dat[trainindex,],lambda=lambdas)     
coefridge <- coef(lm0)[which.min(lm0$GCV),]  #根据GCV选择lambdas
predicterrorridge <- dat$yvector[-trainindex]-cbind(1,xmatrix[-trainindex,])%*%coefridge
mse.ridge <- sum(predicterrorridge^2)/length(predicterrorridge)

## lasso
cvboston <- cv.glmnet(x=xmatrix[trainindex,],y=yvector[trainindex])
#lasso回归  实际上函数默认cv.glmnet(x=xmatrix[trainindex,],y=yvector[trainindex]，alpha=1)
# alpha指弹性网络中的alpha ridge回归时 alpha=0
#进行k—fold交叉检验 默认k=10 
coeflasso <- coef(cvboston,s="lambda.min")
#lambda.min 指的是k-fold交叉检验中 mse最小的lambda
predicterrorlasso <- dat$yvector[-trainindex]-cbind(1,xmatrix[-trainindex,])%*%coeflasso
mse.lasso <- sum(predicterrorlasso^2)/length(predicterrorlasso)

c(mse.ols,mse.ridge,mse.lasso)

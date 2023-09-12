library("mlbench")
library("glmnet")
library("MASS")
data(BostonHousing)


### training and testing data
set.seed(2022)  
nsample <- dim(BostonHousing)[1]                
trainindex <- sample(1:nsample, floor(nsample*0.8))
data.noise<-matrix(rnorm(nsample*nsample),nsample,nsample)
datas<-cbind(BostonHousing,data.noise)
xmatrix <- model.matrix(medv~.,data=datas)[,-1]     #创立设计矩阵
yvector <- BostonHousing$medv
dat <- data.frame(yvector,xmatrix)
## linear regression
bostonlm <- lm(yvector~.,data=dat[trainindex,])
coefols<-bostonlm$coefficients
predicterror <- dat$yvector[-trainindex]-predict(bostonlm,dat[-trainindex,])#测试集检验
mse.ols <- sum(predicterror^2)/length(predicterror)   
## ridge regression
lambdas <- seq(0,5,0.01)
lm0 <- lm.ridge(yvector~.,data=dat[trainindex,],lambda=lambdas)     
coefridge <- coef(lm0)[which.min(lm0$GCV),] 
predicterrorridge <- dat$yvector[-trainindex]-cbind(1,xmatrix[-trainindex,])%*%coefridge
mse.ridge <- sum(predicterrorridge^2)/length(predicterrorridge)

## lasso
cvboston <- cv.glmnet(x=xmatrix[trainindex,],y=yvector[trainindex])
coeflasso <- coef(cvboston,s="lambda.min")
predicterrorlasso <- dat$yvector[-trainindex]-cbind(1,xmatrix[-trainindex,])%*%coeflasso
mse.lasso <- sum(predicterrorlasso^2)/length(predicterrorlasso)

c(mse.ols,mse.ridge,mse.lasso)
cbind(coefols,coefridge,coeflasso)
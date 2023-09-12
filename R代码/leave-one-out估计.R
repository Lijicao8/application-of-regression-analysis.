library(mlbench)
data(BostonHousing)
attach(BostonHousing)
n <- dim(BostonHousing)[1]          #样本数
p <- dim(BostonHousing)[2]          #协变量个数
ymin <- min(medv)
ymax <- max(medv)
grid.y <- seq(ymin-30,ymax+30,0.1)#生成一组等差数列
BostonHousing <- BostonHousing[order(medv),]   #按照medv从小到大排
                                               #order用于获取顺序
detach(BostonHousing)

ols.fit.full <- lm(medv~.,data=BostonHousing,x=TRUE,y=TRUE,qr=TRUE)  
beta <- ols.fit.full$coef
e.sigma <- summary(ols.fit.full)$sigma
rss<-deviance(ols.fit.full)
X <- ols.fit.full$x
Y <- ols.fit.full$y
X.QR <- ols.fit.full$qr                      #qr分解
X.Q <- qr.Q(X.QR)                             
X.R <- qr.R(X.QR)
Gram.inv <- solve(t(X.R)%*%X.R)            #t（A）表示A的转置   %*%表示矩阵乘法 
              #solve（a，b）表示ax=b方程的解  若无b 则solve（a）表示a的逆矩阵
hatmat <- X.Q%*%t(X.Q)              
resmat <- diag(n)-hatmat
leverage <- diag(hatmat)
Resvec <- ols.fit.full$residuals                  

cvt <- qt(0.975,df=n-p-1)      #表示自由度为n-p-1的t分布0.975所取的值
cvr <- ceiling(0.95*(n+1))      #ceiling表示向上取整函数

loo.pred <- matrix(0,n,5)       
loo.cov <- matrix(0,n,2)

for(i in 1:n){
  beta.i <- beta-Gram.inv%*%X[i,]*Resvec[i]/(1-leverage[i])   #去掉i以后的beta[i]
  e.sigma.i <- sqrt( rss-(Resvec[i])^2/(1-leverage[i])  )/sqrt(n-p-1)
                   #sigma^2基于leave-one-out模型下的无偏估计
  pred.i <- sum(X[i,]*beta.i)           #yihat（基于leave-i-out）
  lower.i <- pred.i-cvt*e.sigma.i/sqrt(1-leverage[i])        #置信区间左端点
  upper.i <- pred.i+cvt*e.sigma.i/sqrt(1-leverage[i])
  loo.pred[i,1:3] <- c(pred.i,lower.i,upper.i)                
  loo.cov[i,1] <- findInterval(Y[i],c(lower.i,upper.i))
  xhi<-c(lower.i,upper.i)
  yhi<-c(0,0)
 
  plot(xhi,yhi,type = "b")
  
  grid.r <- sapply(grid.y,
                  FUN = function(y){
                    Res <-  Resvec+resmat[,i]*(y-Y[i])
                    rank(abs(Res))[i]
                  }
  )
  Cinterval <- range(grid.y[grid.r<=cvr])
  loo.pred[i,4:5] <- Cinterval
  loo.cov[i,2] <- findInterval(Y[i],Cinterval)
}

colnames(loo.pred) <- c("point","G.l","G.u","c.l","c.u")
head(loo.pred)
apply(loo.cov==1,2,mean)


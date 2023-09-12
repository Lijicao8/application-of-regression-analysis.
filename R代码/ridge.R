library(MASS)
n <- 200
p <- 100
beta <- rep(1/sqrt(p),p)  #rep 表示复制p次1/sqrt（p）
sig <- 1/2               
### uncorrelated covariates
X <- matrix(rnorm(n*p),n,p)
X <- scale(X)          #标准化X


### scale use n-1 but ridge uses n
X <- X*sqrt(n/(n-1))                        #标准化时除的是n 但是r中默认是除n-1
Y <- as.vector(X%*%beta+rnorm(n,0,sig))     #将矩阵转换为行向量（）按列进行
#MSE
eigenxx <- eigen(t(X)%*%X)         #求X‘X的特征值和特征向量 为了获得SVD中的奇异值
xis <- eigenxx$values                              
gammas <- t(eigenxx$vectors)%*%beta               

lambda.seq <- seq(0,70,0.01)                #ridge回归中的参数lambda
bias2.seq <- lambda.seq
var.seq <- lambda.seq
mse.seq <- lambda.seq
for (i in 1:length(lambda.seq)){
  ll <- lambda.seq[i]                 
  bias2.seq[i] <- ll^2*sum(gammas^2/(xis+ll)^2)
  var.seq[i] <- sig^2*sum(xis/(xis+ll)^2)
  mse.seq[i] <- bias2.seq[i]+var.seq[i]
}


### MSE in the testing dataset

ridge.fit <- lm.ridge(Y~X,lambda=lambda.seq)

#测试集构造
X.new <- matrix(rnorm(n*p),n,p)
X.new <- scale(X.new)
X.new <- X.new*sqrt(n/(n-1))
Y.new <- as.vector(X.new%*%beta+rnorm(n,0,sig))

predict.error <- Y.new-X.new%*%ridge.fit$coef   
#ridge recession 的预测SSR 一个列向量（因为有n个样本）
predict.mse <- apply(predict.error^2,2,mean)     #SSR的均值  因为有n个样本 
#apply表示对行或者列进行某种操作  
#对列进行操作求均值


### correlated covariates

X <- matrix(rnorm(n*p),n,p)+rnorm(n,0,0.5)           #去除x的单位正交性
X <- scale(X)
X <- X*sqrt(n/(n-1))
Y <- as.vector(X%*%beta+rnorm(n,0,sig))

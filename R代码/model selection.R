library("Matching")
library("leaps")
###求AIC BIC CVG
library("ICglm")
###求PRESS
library("MPV")
data(lalonde)
models<- regsubsets(re78~., data = lalonde, nvmax =11)#选择时默认包含了截距项
res.sum<-summary(models)
par(mfrow=c(2,2))
###AIC作图
n<-length(lalonde$re78)
p<-apply(res.sum$which, 1, sum)        
aic<-res.sum$bic-log(n)*p+2*p
plot(aic, xlab="Parameter", ylab="AIC")
###BIC作图
plot(res.sum$bic, xlab="Parameter", ylab="BIC")
###GCV作图
gcv<-res.sum$rss*(1-p/n)^-2
plot(gcv, xlab="Parameter", ylab="GCV")
mtext("lalonde data", side=3,outer=TRUE, line=-3)
###构造提取模型函数
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:11
cv.errors <-  map(model.ids, get_model_formula, models, "re78") %>%
  map(get_cv_error, data = lalonde) %>%
  unlist()
press=cv.errors^2*n
plot(press, xlab="Parameter", ylab="PRESS")
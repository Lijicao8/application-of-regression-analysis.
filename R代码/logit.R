setwd("/Users/Zhichao/Dropbox/courses/SYSU/linear\ model/lectures/code/")
flu <-  read.table("flu240.txt", header = TRUE) 
flu <-  within(flu, rm(treatment.received))
assign.logit <- glm(outcome~.,family = binomial(link=logit),data=flu)
summary(assign.logit)

### Likelihood ratio test
pchisq(assign.logit$null.deviance -assign.logit$deviance,
        df = assign.logit$df.null - assign.logit$df.residual,
       lower.tail = FALSE)

### prediction
emp.mean <- apply(flu,2,mean)
data.ave <- rbind(emp.mean,emp.mean)
data.ave[1,1] <- 1
data.ave[2,1] <- 0
data.ave <- data.frame(data.ave)
predict(assign.logit,newdata=data.ave,type="response",se.fit=TRUE)


#### average marginal effects
library(margins)
ape<- margins(assign.logit)
summary(ape)


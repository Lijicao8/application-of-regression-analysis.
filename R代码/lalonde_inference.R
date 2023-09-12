library(Matching)
data(lalonde)
lalonde.lm <- lm(re78~.,data=lalonde)
summary(lalonde.lm)


library(car)
linearHypothesis(lalonde.lm, c("age=0", "educ=0", "black=0",
                                     "hisp=0", "married=0", "nodegr=0", 
                                     "re74=0", "re75=0", "u74=0", "u75=0"))
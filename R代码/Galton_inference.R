library(HistData)
galton.lm <- lm(childHeight ~ midparentHeight, data = GaltonFamilies)
round(summary(galton.lm)$coef,3)

mph.new <- seq(60,90,by=0.5)
data.new <- data.frame(midparentHeight = mph.new)
ci.new <- predict(galton.lm, data.new, interval = "confidence")
pi.new <- predict(galton.lm, data.new, interval = "prediction")

round(head(cbind(ci.new,pi.new)),3)

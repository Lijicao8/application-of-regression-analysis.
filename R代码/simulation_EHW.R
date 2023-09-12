library(car)
n <- 200
x <- runif(n,-2,2)
beta <- 1
xbeta <- x*beta
sim1 <-   replicate(5000,  {y <- xbeta+rnorm(n)
            ols.fit <- lm(y~x)
            c(summary(ols.fit)$coef[2,1:2],  sqrt(hccm(ols.fit)[2,2]) )
            })


sim2 <-   replicate(5000,  {y <- xbeta+rexp(n)
                            ols.fit <- lm(y~x)
                            c(summary(ols.fit)$coef[2,1:2],  sqrt(hccm(ols.fit)[2,2]) )
})

sim3 <-   replicate(5000,  {y <- xbeta+rnorm(n,0,abs(x))
                                ols.fit <- lm(y~x)
                            c(summary(ols.fit)$coef[2,1:2],  sqrt(hccm(ols.fit)[2,2]) )
})

sim4 <-   replicate(5000,  {y <- xbeta+runif(n,-x^2,x^2)
                                  ols.fit <- lm(y~x)
                              c(summary(ols.fit)$coef[2,1:2],  sqrt(hccm(ols.fit)[2,2]) )
})




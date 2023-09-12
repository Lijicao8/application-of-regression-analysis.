library("KernSmooth")
n <- 500
x <-  seq(0, 1, length.out = n)
fx <-  sin(8*x)
y <-  fx + rnorm(n, 0, 0.5)
plot(y ~ x, pch = 19, cex = 0.2, col = "grey", bty = "n",
        main = "local linear fit", font.main = 1)
lines(fx ~ x, lwd = 2, col = "grey")
h <-  dpill(x, y)
locp.fit <-  locpoly(x, y, bandwidth = h)
lines(locp.fit, lty = 2)
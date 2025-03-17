


#beta distribution

x1 = seq(0,1414,1)
y1 = dbeta(seq(0,1,by=1/1414), shape1=2, shape2=3, ncp = 0, log = FALSE)

plot(x1,y1,
      ylab = "probability density",
      xlab = "distance from nest (m)",
      lty=1,lwd=2,type="l")
curve(dbeta(x, shape1=2, shape2=3, ncp = 0, log = FALSE),
      from = 0, to=1,
      ylab = "probability density",
      xlab = "distance from nest \n normalized so that 1 is the farthest distance",
      lty=1,lwd=2,type="l")



#exponential 
x2 = 1:10
y2 = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.05, 0.05, 0.15, 0.15, 0.55)
plot(x2,y2,
     ylab = "probability",
     xlab = "bin",
     lty=2,pch=16)


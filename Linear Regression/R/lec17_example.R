x<-seq(3,10, 0.01)
y<-1+.1*x+0.07*x^3-0.02*x^4+rnorm(length(x), 0, 4)
plot(x,y,ylim=c(-150, 40))
abline(v=x[100])
abline(v=x[600])
x2<-(x-mean(x))^2
x3<-(x-mean(x))^3
x4<-(x-mean(x))^4
x5<-(x-mean(x))^5
x6<-(x-mean(x))^6

lines(x, cbind(rep(1,length(x)), x, x2, x3, x4, x5, x6)%*%lsfit(cbind(x,x2,x3,x4,x5,x6)[100:600,], y[100:600])$coef, col=2, lwd=3)
lines(x, cbind(rep(1,length(x)), x, x2, x3, x4, x5)%*%lsfit(cbind(x,x2,x3,x4,x5)[100:600,], y[100:600])$coef, col=3, lwd=3)
lines(x, cbind(rep(1,length(x)), x, x2, x3)%*%lsfit(cbind(x,x2,x3)[100:600,], y[100:600])$coef, col=4, lwd=3)
lines(x, cbind(rep(1,length(x)), x, x2)%*%lsfit(cbind(x, x2)[100:600,], y[100:600])$coef, col=5, lwd=3)
lines(x, cbind(rep(1,length(x)), x)%*%lsfit(cbind(x)[100:600,], y[100:600])$coef, col=6, lwd=3)

legend(5, -30, c("6th order", "5th order", "3rd order", "2nd order", "1st order"), lty=1, lwd=2, col=2:6)

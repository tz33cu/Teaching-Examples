#library(mvtnorm) ##get multivariate normal functions  
rr.seq<-c(0, 0.1, 0.3, 0.5, 0.7, 0.9)
par(mfrow=c(2,3))
for(rr in rr.seq){
    x <- seq(-2,2,.01)  
    X <- cbind(rep(x,each=length(x)), rep(x, length(x)))  
    z <- matrix(dmvnorm(X, sigma=matrix(c(1,rr,rr,1), 2, 2)), nrow=length(x))  
    contour(x=x,y=x,z=z, nlevels=8) ##take in vector x and y with matrix z of values   
    abline(0,1, col=2, lwd=1.5)
    abline(0, rr, col=1, lty=2, lwd=2)
}

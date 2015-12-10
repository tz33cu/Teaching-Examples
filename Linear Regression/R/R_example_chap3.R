# toluca is a dataset about the lot size and work hours in Toluca company
# 3.5 Correlation test for normality
names(toluca) <- c("lotsize","workhr")
lm.toluca <- lm(workhr~lotsize,data=toluca)
res <- lm.toluca$residual
# obtain a sequence of ordered residual
res.order <- sort(res)
# compute the expected value of ordered residual
anova(lm.toluca)
mse <- summary(lm.toluca)$sigma^2
n <- nrow(toluca)
res.exp <- sqrt(mse)*qnorm((1:n-0.375)/(n+0.25)) 
## compute the correlation between ordered residual and their expectations
cor(res.order,res.exp)

#3.6 Test for constant error variance
# Brown-Forsythe test 
library(lawstat)
group <- (toluca$lotsize<=70)
# set location="median" to implement Brown-Forsythe version of Levene test
levene.test(res,group,location="median")

# Breusch-Pagan test
library(lmtest)
# If set studentize=TRUE,Koenker's studentized 
# version of the test statistic will be used.
# note: this test gives the upper-tail p-value, 
# while the book gives the lower-tail p-value. 
bptest(lm.toluca,studentize=FALSE) 

#3.7 F test for lack of fit
lm.toluca.2 <- lm(workhr~factor(lotsize),data=toluca)
anova(lm.toluca,lm.toluca.2)

# 3.9 Transformations
library(MASS)
# plot the log-likelihood of the parameter in Box-cox transformation
boxcox(lm.toluca)

# 3.10 Exploration of regression function shape
plot(toluca)
#add lowess line
lines(lowess(toluca))
# plot confidence band
xx <- seq(20,120,1)
yy <- predict (lm.toluca, newdata=data.frame(lotsize=xx),se.fit=T)$fit
s.yy <- predict (lm.toluca, newdata=data.frame(lotsize=xx),se.fit=T)$se.fit
W <- sqrt(2*qf(0.95,2,n-2))
lower.bound <- yy-W*s.yy
upper.bound <- yy+W*s.yy
lines(xx,lower.bound,lty=2,col=2)
lines(xx,upper.bound,lty=2,col=2)

# state.x77 is a data set about the 50 states of united states
statedata=as.data.frame(state.x77)

# change the column names for easier use. 
#colnames can be used to retrieve the colume names or assign new names. 
colnames(statedata)=c("popu", "inc", "illit", "life.exp", "murder", "hs.grad", "frost", "area")

lm(life.exp~inc, data=statedata)
model1=lm(life.exp~inc, data=statedata)
# results from lm() can be saved as R objects. 

confint(model1, level=0.9)
confint(model1)
# the confidence interval function for parameters. Default level is 95%.

predict(model1, data.frame(inc=c(3500, 4500, 5500)), level=0.9, interval="confidence")
predict(model1, data.frame(inc=c(3500, 4500, 5500)), level=0.99, interval="confidence")
predict(model1, data.frame(inc=c(3500, 4500, 5500)), level=0.99, interval="prediction")

summary(model1)
# basic outputs of regression models

plot(statedata$inc, statedata$life.exp)
abline(model1$coef)
abline(v=mean(statedata$inc), col="gray", lty=3)
# a vertical line at a X value.
abline(h=mean(statedata$life.exp), col="gray", lty=3)
# a horizontal line at a Y value

## Make a confidence band. 
# first calculate the critical value W.
ww=sqrt(2*qf(0.95, 2, nrow(statedata)-2))
# generate plotting X values. 
plot.x<-data.frame(inc=seq(3000, 7000, 1))
# to use with the predict() function, input needs to be a data.frame
# containing a vector with the same name as the original X variable. 
# se.fit=T is an option to save the standard error of the fitted values. 
plot.fit<-predict(model1, plot.x, level=0.95, interval="confidence", se.fit=T)

# lines is a function to add connected lines to an existing plot.
lines(plot.x$inc, plot.fit$fit[,1]+ww*plot.fit$se.fit, col=2, lty=2)
lines(plot.x$inc, plot.fit$fit[,1]-ww*plot.fit$se.fit, col=2, lty=2)

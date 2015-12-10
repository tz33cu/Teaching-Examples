# state.x77 is a data set about the 50 states of united states
statedata=as.data.frame(state.x77)

# change the column names for easier use. 
#colnames can be used to retrieve the colume names or assign new names. 
colnames(statedata)=c("popu", "inc", "illit", "life.exp", "murder", "hs.grad", "frost", "area")

# attach is a command to add a data set to the search path of R so that the variables
# of this data set can be directly called.
attach(statedata)

# see? since statedata is attached, we don't need to specify it
# in the lm() command. 
model1=lm(life.exp~inc)

# The anova table with p values
summary(aov(model1))
# The coefficient table with p values
summary(model1)
# correlation function
cor(life.exp, inc)
# Plot the relation
plot(inc, life.exp)
abline(model1$coef)

## make residual plots. 
## par() specifies display parameters. mfrow and mfcol specifies multi graphs 
## on a page by specifying the number of rows and the number of columns. 
## both mfrow=c(2, 3) and mfcol=c(2,3) will make a page with two rows and three
## columns of figures. The difference is that mfrow will fill the page by rows
##    1    2    3
##    4    5    6
## while mfcol will fill the page by columns
##    1    3    5
##    2    4    6

par(mfrow=c(2,3))
## residual sequence plot
plot(model1$residuals)
## against X
plot(inc, model1$residuals)
abline(h=0) # add a horizontal line at y=0
## absolution residuals against X
plot(inc, abs(model1$residuals))
## histogram of residuals
hist(model1$residuals)
## normal quantiles plot of residuals
qqnorm(model1$residuals)
qqline(model1$residuals) # add a line
## residuals against other possible influential variables.
plot(hs.grad, model1$resid)
abline(h=0)

model2=lm(inc~hs.grad)
par(mfrow=c(2,3))
## regression model
plot(inc~hs.grad)
abline(model2$coef)
## against X
plot(hs.grad, model2$residuals)
abline(h=0) # add a horizontal line at y=0
## absolution residuals against X
plot(hs.grad, abs(model2$residuals))
## histogram of residuals
hist(model2$residuals)
## normal quantiles plot of residuals
qqnorm(model2$residuals)
qqline(model2$residuals) # add a line
## residuals against other possible influential variables.
plot(life.exp, model2$resid)
abline(h=0)

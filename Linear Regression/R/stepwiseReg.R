zagat$region=as.factor(zagat$region)
zagat.use=zagat[,-2]
null=lm(cost~1, data=zagat.use)
full=lm(cost~., data=zagat.use)
step(null, scope=list(lower=null, upper=full), direction="forward")
step(null, scope=list(lower=null, upper=full), direction="both")
step(full, scope=list(lower=null, upper=full), direction="backward")

# All Subsets Regression function
leaps.lm<-function(formula.lm, data){
    library(leaps)
    library(nlme)
    library(DAAG)
    model.lm = lm(formula.lm, data=data, x=TRUE, y=TRUE)
    xx = model.lm$x[,-1]
    yy = model.lm$y
    
    var.names = colnames(xx)
    
    leaps.lm.temp = summary(regsubsets(x=xx, y=yy, nbest=2^ncol(xx), nvmax=2^ncol(xx),
                                method="exhaustive", all.best=TRUE, really.big=T))    
    
    aic.list = rep(0, nrow(leaps.lm.temp$which))
    bic.list = rep(0, nrow(leaps.lm.temp$which))
    press.list = rep(0, nrow(leaps.lm.temp$which))
    model.name = rep(0, nrow(leaps.lm.temp$which))
    models.try = leaps.lm.temp$which[,-1]
    model.size = rowSums(as.matrix(models.try))
    
    for(i in 1:length(aic.list)){
        matrix.temp = as.data.frame(cbind(yy, xx[, (1:ncol(xx))[models.try[i,]]]))
        colnames(matrix.temp)[1]<-"y"
        cur.model = lm(y~., data=matrix.temp)
        aic.list[i] = extractAIC(cur.model)[2]
        bic.list[i] = aic.list[i]-2*model.size[i]+log(nrow(xx))*model.size[i]
        press.list[i] = press(cur.model)
        model.name[i] = paste(var.names[models.try[i,]], collapse=" ")
    }

    
    results.leaps=data.frame(model.name, model.size , leaps.lm.temp$rss, leaps.lm.temp$rsq, leaps.lm.temp$adjr2, leaps.lm.temp$cp, aic.list, bic.list, press.list)
    colnames(results.leaps)=c("model", "size", "SSE", "r2", "adjr2", "Cp", "aic", "bic", "press")
    return(results.leaps)
}

zagat.result = leaps.lm(cost~service+decor+food+ind_f, data=zagat)

par(mfrow=c(2,2))
plot(zagat.result$size, zagat.result$r2, type="n", xlim=c(0.5, 5))
text(zagat.result$size, zagat.result$r2, zagat.result$model)

plot(zagat.result$size, zagat.result$adjr2, type="n", xlim=c(0.5, 5))
text(zagat.result$size, zagat.result$adjr2, zagat.result$model)

plot(zagat.result$size, zagat.result$Cp, type="n", xlim=c(0.5, 5))
text(zagat.result$size, zagat.result$Cp, zagat.result$model)

plot(zagat.result$size, zagat.result$press, type="n", xlim=c(0.5, 5))
text(zagat.result$size, zagat.result$press, zagat.result$model)

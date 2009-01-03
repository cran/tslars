`tslars` <-
function(formula,h=1,p.max=5,max.x=10,nr.rank=NA){

    call <- match.call()
    mf <- model.frame(formula)
    y <- mf[,1]
    x <- as.matrix(model.matrix(attr(mf,"terms"),data=mf))[,-1]
    fixedp <- FALSE
    
    m <- dim(x)[2]
    n <- length(y)
    a <- p.max:1
    tslars.fix.p <- list()
    
    form <- list()
    for(i in 1:p.max){
        form[[i]] <- y[a[i]:n] ~ x[a[i]:n,]
        tslars.fix.p[[i]] <- tslars.p(form[[i]], p=i, h=h, max.x=max.x,nr.rank=nr.rank)
    }

    bic.matrix <- rep(0, length(tslars.fix.p[[1]]$bic))
    for(i in 1:p.max){
       bic.matrix <- cbind(bic.matrix, tslars.fix.p[[i]]$bic)
    }
    bic.matrix <- bic.matrix[,-1]
    
    p.opt <- minindexmatrix(bic.matrix[1:min(c(max.x,nr.rank,m),na.rm=TRUE),])[2]
    nrvar.opt <- minindexmatrix(bic.matrix[1:min(c(max.x,nr.rank,m),na.rm=TRUE),])[1] 

out <- list(active=tslars.fix.p[[p.opt]]$ordered.row-1, ordered.row = tslars.fix.p[[p.opt]]$ordered.row, laglength.opt = p.opt, nrvar.opt = nrvar.opt , bic = tslars.fix.p[[p.opt]]$bic, call=call, response=y, predictors=x, h=h, fixedp=fixedp)
class(out) <- "tslars"
return(out)
}


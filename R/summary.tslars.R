`summary.tslars` <-
function(object, ...){

    y <- object$response
    x <- cbind(y,object$predictors)[,object$ordered.row[1:object$nrvar.opt]]
    p <- object$laglength.opt
    h <- object$h
    fixedp <- object$fixedp
    n <- length(y)
    if(object$nrvar.opt==1 & object$ordered.row[1]==1){ary=TRUE}else{ary=FALSE}

    # make appropriate matrixes
    x <- design(x=x,y=y,p=p,ary=ary,h=h)

    response <- y[(p+h):n]
    reg <- lm(response ~ x)
    xx <- list(object,reg,bic=object$bic,fixedp=fixedp)
    class(xx) <- "summary.tslars"
    return(xx)
}


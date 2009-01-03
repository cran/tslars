`predict.tslars` <-
function(object, ...){

    y <- object$response
    x <- cbind(y,object$predictors[,object$ordered.row[1:object$nrvar.opt][-match(1,object$ordered.row)]-1])
    p <- object$laglength.opt
    h <- object$h
    n <- length(y)
    if(object$nrvar.opt==1 & object$ordered.row[1]==1){ary=TRUE}else{ary=FALSE}

    # make appropriate matrixes
    design.matrix <- design(x=x,y=y,p=p,ary=ary,h=h)

    y.left <- y[(p+h):n]
    coefs <- lm(y.left ~ design.matrix)$coef
    # make h-step-ahead forecast
    y.hat <- c(1, design.matrix[n-p-h+1,]) %*% coefs
    print(drop(y.hat))
    
out <- drop(y.hat)
}


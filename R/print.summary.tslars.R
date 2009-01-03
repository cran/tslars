`print.summary.tslars` <-
function(x, ...){
    cat("\nCall:\n")
    dput(x[[1]]$call)
    cat("\n")
    cat("Predictor ranking ('0' denotes lagged values of the response) \n")
    ordered.row <- x[[1]]$ordered.row-1
    print(ordered.row[1:length(x$bic)])
    cat("\n")
    cat(c("Optimal number of predictors: ", x[[1]]$nrvar.opt, "\n"))    
    fixedp <- x$fixedp
    if(fixedp){
        cat(c("Fixed lag length: ", x[[1]]$laglength.opt, "\n"))
    }else{
        cat(c("Optimal lag length: ", x[[1]]$laglength.opt, "\n"))
    }
    cat("\n")
    cat("Regression coefficients of the selected model: \n")
    print(coef(x[[2]]))
}


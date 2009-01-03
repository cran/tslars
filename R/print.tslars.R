`print.tslars` <-
function(x, ...){
    cat("\n Call: \n")
    dput(x$call)
    cat("\n")
    m <- length(x$ordered.row)
    ordered.row <- x$ordered.row-1
    print.out <- data.frame(Var=ordered.row[1:length(x$bic)], bic=x$bic)
    print(print.out)
}


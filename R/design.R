`design` <-
function(x=NULL,y,p,h,ary=FALSE){
# Make design matrix
# INPUT
#   x = matrix with predictor variables
#   y = dependent variable
#   p = lag lenght
# OUTPUT = matrix with lagged values of y and x up to lag p
    n <- length(y)
    if(is.vector(x)){x <- matrix(x, ncol=1)}
    if(ary==TRUE){
    out.matrix <- matrix(NA, ncol=p, nrow=n-p-h+1)
    for(i in 1:p){
        out.matrix[,i] <- y[(p-i+1):(n-i-h+1)]
    }
    }else{
    m <- dim(x)[2]
    out.matrix <- matrix(NA, ncol=(m*p), nrow=n-p-h+1)
    for(i in 1:p){
        out.matrix[,seq(i, (m*p)+i-1, by=p)] <- x[(p-i+1):(n-i-h+1),]
    }
    }
out.matrix
}


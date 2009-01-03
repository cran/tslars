`minindexmatrix` <-
function(x){
# look for the index where the matrix is minimal
    d <- dim(x)
    A <- cbind(rep(1:d[1],d[2]),sort(rep(1:d[2],d[1])))
    out <- A[which.min(x),]
return(out)
}


`tslars.p` <-
function(formula,p=2,h=1,max.x=NA,nr.rank=NA){

    call <- match.call()
    mf <- model.frame(formula)
    y <- mf[,1]
    x <- as.matrix(model.matrix(attr(mf,"terms"),data=mf))[,-1]
    fixedp <- TRUE

    # STEP 1: Initialize
    n <- length(y)
    m <- dim(x)[2]
    if(is.na(max.x)){max.x <- m}
    if(is.na(nr.rank)){nr.rank <- m+1}
    # make blocks of lagged values
    x.hist.or <- array(NA, dim=c((n-h-p+1),(m+1),p))
    for(ip in 1:p){
        x.hist.or[,,ip] <- cbind(y,x)[(p-ip+1):(n-ip-h+1),]
    }
    x.hist <- apply(x.hist.or, c(2,3), 'scale')
    # standardized response
    z <- c(scale(y[(p+h):n]))
    nn <- length(z)
    # Empty active set
    Ac <- 1:(m+1)

    # STEP 2: Find first ranked predictor
    Rsq <- c()
    for(irsq in 1:(m+1)){
        Rsq[irsq] <- Rsquared(x.hist[,irsq,], z)
    }
    A <- which.max(Rsq)
    Ac <- Ac[Ac != A]

    z <- as.matrix(z,nrow=1)
    x.tilde <- matrix(NA, nrow=nn, ncol=m)
    a <- c()
    r <- c()
    gamma.vec <- c()
    # STEP 3: Update active set
    for(k in 1:min((m-1),nr.rank)){
        x.tilde[,k] <- c(scale(lm(z[,k]~x.hist[,A[k],])$fitted))
        # compute the equiangular vector
        RsubA <- var(x.tilde[,1:k])
	invRsubA <- solve(RsubA)
        ac <- (drop(t(rep(1,k)) %*% invRsubA %*% rep(1,k)))^(-1/2)
        wsubA <- ac * invRsubA %*% rep(1,k)
        eq.vec <- x.tilde[,1:k] %*% wsubA
        if(k==1){
            W <- wsubA
        }else{
            W <- cbind(W,rep(0,(k-1)))
            W <- rbind(W,drop(wsubA))
        }
        a[k] <- cor(eq.vec, x.tilde[,1])
        r[k] <- cor(z[,k],x.tilde[,1])
        
        gamma.j <- c()
        for(j in Ac){
            lm1 <- lm(z[,k]~x.hist[,j,]-1)$fitted
            lm2 <- lm(eq.vec~x.hist[,j,]-1)$fitted
            r.kj <- sd(lm1)
            tau.kj <- sd(lm2)
            a.kj <- cor(eq.vec,lm1)
            comp <- c()
            comp[1] <- r[k]^2-r.kj^2
            comp[2] <- 2*(a.kj*r.kj-a[k]*r[k])
            comp[3] <- a[k]^2-tau.kj^2
            gamma.inter <- Re(polyroot(comp))
            gamma.j <- c(gamma.j,min(gamma.inter[gamma.inter>0]))
        }
        gamma.vec[k] <- min(gamma.j)
        A <- c(A,Ac[which.min(gamma.j)])
        Ac <- Ac[Ac!=A[k+1]]
        z <- cbind(z,c(scale(z[,k] - gamma.vec[k]*eq.vec)))
    }
    A <- c(A,Ac)
    
    bic <- c()
    response <- y[(p+h):n]
    predictors <- rep(1,nn)
    for(ibic in 1:min(m+1,nr.rank)){
        predictors <- cbind(predictors,x.hist.or[,A[ibic],])
        bic[ibic] <- AIC(lm(response~predictors-1), k=log(length(response)))
    }

    nrvar.opt <- which.min(bic[1:max.x])
out <- list(active=A-1, ordered.row=A, laglength.opt=p, nrvar.opt=nrvar.opt, bic=bic, call=call, response=y, predictors=x, h=h, fixedp=fixedp)
class(out) <- "tslars"
return(out)
}


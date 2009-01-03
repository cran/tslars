`Rsquared` <-
function(x,y){
# x = matrix of predictors (n x p)
# y = response vector (n x 1)
# Rsq(x,y) gives the R squared measure after regression y on x (no intercept)
    Rsq <- summary(lm(y~x-1))[[8]]
    return(Rsq)
}


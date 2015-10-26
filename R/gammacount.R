## Generate Gamma-Count random variables.

n=1L; shape=1; rate=1
n=1L; shape=1:4; rate=11:14
n=4L; shape=1:4; rate=11:12
size <- c(n=n, sh=length(shape), rt=length(rate))

rgc <- function(n=1L, shape=1, rate=1){
    rGammaCount <- function(shape=shape, rate=rate){
        ## To save the sum of Gamma ramdom variables.
        s <- 0
        ## To keep each Gamma random number.
        x <- numeric(0)
        repeat {
            ## Generate a Gamma.
            u <- rgamma(n=1L, shape=shape, rate=rate)
            ## Update the sum.
            s <- s+u
            if (s>=1) break
            ## Append the new value.
            x <- append(x, values=u)
        }
        return(length(x))
    }
    sizes <- c(n, length(shape), length(rate))
    if (max(sizes[-1])>1L){
        message(paste(
            "`n` was ignored because some",
            "parameter has length > 1."))
        shapeRate <- cbind(shape=shape, rate=rate)
        y <- mapply(FUN=rGammaCount,
                    shape=shapeRate[, "shape"],
                    rate=shapeRate[, "rate"])
    } else {
        y <- replicate(n, rGammaCount(shape=shape, rate=rate))
    }
    return(y)
}

L <- list(
    y1=rgc(1000, shape=1, rate=1),
    y2=rgc(1000, shape=5, rate=5),
    y3=rgc(1000, shape=0.2, rate=0.2))
lapply(L, FUN=function(x) c(mean(x), var(x)))

##----------------------------------------------------------------------

rgc <- function(n=1L, shape=1, rate=1){
    ## To save the sum of Gamma ramdom variables.
    s <- 0
    ## To keep each Gamma random number.
    x <- numeric(0)
    repeat {
        ## Generate a Gamma.
        u <- rgamma(n=1L, shape=shape, rate=rate)
        ## Update the sum.
        s <- s+u
        if (s>=n) break
        ## Append the new value.
        x <- append(x, values=u)
    }
    ## Tabulate (count) occurrences in each interval.
    y <- tabulate(ceiling(cumsum(x)))
    ## This is a Gamma Count random variable.
    return(y)
}

L <- list(
    y1=rgc(500, shape=1, rate=1),
    y2=rgc(500, shape=5, rate=5),
    y3=rgc(500, shape=0.2, rate=0.2))

lapply(L, FUN=function(x) c(mean(x), var(x)))

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

##======================================================================
## Fonte:
## http://www.johndcook.com/blog/cpp_random_number_generation/

## The Poisson process.
## http://www.math.uchicago.edu/~may/VIGRE/VIGRE2010/REUPapers/Mcquighan.pdf

## Gerador de Poisson.
x <- replicate(10000, {
    lambda <- 5
    p <- 1
    L <- exp(-lambda) ## Pr(X=0) = exp(-lambda)
    k <- 0;
    while (p>=L) {
        k <- k+1
        p <- p*runif(1)
    }
    k-1
})

plot(ecdf(x))
curve(ppois(x, lambda=5), type="s", add=TRUE, col=2)

##======================================================================
## Density of a Gamma Count random variable.

dgc <- function(x, alpha, beta){
  pgamma(q=1, shape=alpha*x, rate=beta)-
      pgamma(q=1, shape=alpha*(x+1), rate=beta)
}

dgc_test <- function(x, alpha, beta){
    fx <- function(u){
        (beta^(alpha*x))*
            exp(-beta*u)*
            (u^(alpha*x-1))*
            ((1/gamma(alpha*x))-
             (beta^alpha)*(u^alpha)/gamma(alpha*(x+1)))
    }
    integrate(fx, 0, 1)$value
}

dgc_test(x=5, alpha=1, beta=2)
dgc(x=5, alpha=1, beta=2)

##----------------------------------------------------------------------

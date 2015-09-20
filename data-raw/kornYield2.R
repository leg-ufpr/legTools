##----------------------------------------------------------------------
## Data generation.

## kornYield2 <- read.table("clipboard", header=TRUE, sep="\t")
kornYield2 <- data.frame(
    N=c(0L, 0L, 45L, 45L, 45L, 45L, 45L, 45L, 90L),
    P=c(0L, 45L, 0L, 45L, 45L, 45L, 45L, 90L, 45L),
    K=c(0L, 30L, 30L, 0L, 30L, 30L, 60L, 30L, 30L),
    limestone=c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
    yield=c(4.192, 4.427, 4.146, 5.029, 5.29, 5.325, 5.275, 5.465,
            5.39))
str(kornYield2)

save(kornYield2, file="../data/kornYield2.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

data(kornYield2)
str(kornYield2)

## Axial triple factorial with 2 controls.
ftable(xtabs(~N+P+K, data=kornYield2))

xyplot(yield~N+P+K,
       groups=as.integer(limestone==1 | (N+P+K)==0),
       data=kornYield2, type=c("p", "a"),
       auto.key=TRUE,
       ylab=expression(Yield~(ton~ha^{-1})),
       xlab="Nutrient content")

rm(list=ls())
load("../data/kornYield2.RData")
ls()
str(kornYield2)

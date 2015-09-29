##----------------------------------------------------------------------
## Data generation.

cottonFert <- expand.grid(
    ## K=c(-1, 1),
    ## N=c(-1, 1),
    trt=c("N1K1", "N1K2", "N2K1", "N2K2", "CTRL"),
    rept=1:4,
    loc=gl(5, 1),
    KEEP.OUT.ATTRS=FALSE)

## x <- scan()
## dput(x/10)

cottonFert$y <- c(4.2, 3.6, 3.2, 3.6, 2.4, 2.4, 2.2, 2.6, 2.8, 1.2, 2.8,
                  1.8, 3, 3, 3, 3.2, 3.2, 2, 2.4, 2.8, 11, 10, 12, 10.5,
                  8.5, 10.5, 9.5, 9, 11.5, 8, 9, 9, 9.5, 10.5, 10, 9, 8,
                  9.5, 10, 7, 7, 8.5, 9, 9, 7, 7.5, 7, 6, 9.5, 7, 6.5,
                  8, 6, 8, 4.5, 5.5, 5.5, 5.5, 7, 6, 8, 8, 7, 9, 3.5,
                  6.9, 5.5, 4.7, 6.5, 3, 6, 7.5, 5.5, 5.5, 3.9, 6.5, 8,
                  9, 7, 7, 1.72, 2.38, 2.52, 2.78, 1.48, 1.81, 2.56,
                  2.88, 3.01, 1.62, 1.73, 2.48, 2.76, 2.83, 1.58, 1.62,
                  2.43, 2.54, 2.79, 1.56)

## Check using the totals.
aggregate(y~loc, data=cottonFert, FUN=sum)

str(cottonFert)

save(cottonFert, file="../data/cottonFert.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(cottonFert)
str(cottonFert)

xyplot(y~trt|loc,
       data=cottonFert, type=c("p", "a"),
       ylab="y", xlab="Treatment")

xyplot(log(y)~trt|loc,
       data=cottonFert, type=c("p", "a"),
       ylab="y", xlab="Treatment")

m0 <- by(data=cottonFert, INDICES=cottonFert$loc,
         FUN=lm, formula=y~trt)
lapply(m0, anova)

m1 <- lm(y~loc*trt, data=cottonFert)

par(mfrow=c(2,2)); plot(m1); layout(1)
MASS::boxcox(m1)

m2 <- lm(log(y)~loc*trt, data=cottonFert)
par(mfrow=c(2,2)); plot(m2); layout(1)
anova(m2)

##----------------------------------------------------------------------
## Data generation.

coffeeFert <- expand.grid(N=c(-1,1), P=c(-1,1), K=c(-1,1),
                          block=gl(6, 1), KEEP.OUT.ATTRS=FALSE)

coffeeFert$branches <- c(104, 29, 73, 78, 71, 5, 44, 19, 57, 58, 155,
                         45, 13, 10, 39, 4, 58, 22, 53, 14, 25, 1, 15,
                         3, 111, 11, 70, 9, 21, 1, 11, 7, 64, 30, 64,
                         89, 64, 15, 84, 7, 21, 18, 37, 15, 23, 5, 10,
                         0)

## coffeeFert$sqrtBranches <- sqrt(coffeeFert$branches)

str(coffeeFert)

save(coffeeFert, file="../data/coffeeFert.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

data(coffeeFert)
str(coffeeFert)

xyplot(branches~N|P, groups=K,
       data=coffeeFert, type=c("p", "a"),
       ylab=expression(Branches~(plant^{-1})),
       xlab="Nutrient level")

range(coffeeFert$branches)

## Sum a positive number to avoid zeros and allow BoxCox
## transformation.
m0 <- lm(branches+1~block+(N+P+K)^3, data=coffeeFert)

## Departures from homecedasticity and normality.
par(mfrow=c(2,2)); plot(m0); layout(1)

MASS::boxcox(m0)
abline(v=0, col=2)

m1 <- update(m0, log(.)~.)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m1)

m2 <- update(m1, .~block+N*K)
par(mfrow=c(2,2)); plot(m2); layout(1)

anova(m1, m2)
anova(m2)

summary(m2)

pred <- expand.grid(block="1",
                    N=seq(-1, 1, by=0.1),
                    K=seq(-1, 1, by=0.1))
pred$mu <- predict(m2, newdata=pred)

wireframe(mu~N+K, data=pred,
          scales=list(arrows=FALSE),
          zlab=list(expression(log(Branches+1)), rot=90),
          drape=TRUE, cuts=20,
          col.regions=colorRampPalette(
              color=brewer.pal(n=11, name="Spectral"))(21))

levelplot(mu~N+K, data=pred, aspect=1,
          main=expression(log(Branches+1)),
          col.regions=colorRampPalette(
              color=brewer.pal(n=11, name="Spectral")))

rm(list=ls())
load("../data/coffeeFert.RData")
ls()
str(coffeeFert)

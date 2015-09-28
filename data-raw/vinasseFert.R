##----------------------------------------------------------------------
## Data generation.

vinasseFert <- expand.grid(block=gl(4, 1), mineral=c(-1, 1),
                           vinasse=c(-1, 1), KEEP.OUT.ATTRS=FALSE)

vinasseFert$y <- c(0.020, 0.630, 0.110, 0.115, 0.020, 2.005, 0.700,
                   1.120, 3.040, 4.760, 5.860, 5.520, 5.150, 4.770,
                   3.960, 5.230)

str(vinasseFert)

save(vinasseFert, file="../data/vinasseFert.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

data(vinasseFert)
str(vinasseFert)

xyplot(y~vinasse, groups=mineral,
       auto.key=list(title="Mineral", columns=2),
       data=vinasseFert, type=c("p", "a"),
       ylab="y",
       xlab="Vinasse level")

m0 <- lm(y~block+(vinasse+mineral)^2, data=vinasseFert)
par(mfrow=c(2,2)); plot(m0); layout(1)
anova(m0)

m1 <- update(m0, .~block+vinasse)
par(mfrow=c(2,2)); plot(m1); layout(1)

anova(m0, m1)
anova(m1)

summary(m1)

rm(list=ls())
load("../data/vinasseFert.RData")
ls()
str(vinasseFert)

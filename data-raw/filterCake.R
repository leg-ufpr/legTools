##----------------------------------------------------------------------
## Data generation.

filterCake <- expand.grid(block=gl(4, 1), mineral=c(-1, 1),
                          cake=c(-1, 1), KEEP.OUT.ATTRS=FALSE)

filterCake$y <- c(18.0, 8.6, 9.4, 11.4, 20.6, 21.0, 18.6, 20.6, 19.6,
                  15.0, 14.6, 15.8, 19.2, 19.6, 18.4, 20.2)

str(filterCake)

save(filterCake, file="../data/filterCake.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

data(filterCake)
str(filterCake)

xyplot(y~cake, groups=mineral,
       auto.key=list(title="Mineral", columns=2),
       data=filterCake, type=c("p", "a"),
       ylab="y",
       xlab="Filter cake level")

m0 <- lm(y~block+(cake+mineral)^2, data=filterCake)
par(mfrow=c(2,2)); plot(m0); layout(1)
anova(m0)

summary(m0)

filterCake$Mineral <- factor(filterCake$mineral,
                             labels=c("absent", "present"))

m1 <- aov(y~block+Mineral/cake, data=filterCake)
anova(m1)

## Split SS to see effect of cake in each level of mineral.
summary(m1, split=list("Mineral:cake"=list("absent"=1, "present"=2)))

summary.lm(m1)

rm(list=ls())
load("../data/filterCake.RData")
ls()
str(filterCake)

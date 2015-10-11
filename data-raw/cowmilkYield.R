##----------------------------------------------------------------------
## Data generation. Pimentel page 269.

cowmilkYield <- expand.grid(casein=c(0, 10, 15, 20),
                            block=gl(3, 1))
cowmilkYield$yield <- c(431.4, 687.5, 679.2, 569.7, 485.2, 560.4, 563.3,
                        502.5, NA, 443, 430.5, 462.4)


save(cowmilkYield, file="../data/cowmilkYield.RData")

##----------------------------------------------------------------------

m0 <- lm(yield~block+factor(casein), data=cowmilkYield)
anova(m0)

## The imputed value is the estimated value by the missing model.
predict(m0, newdata=cowmilkYield)

cowmilkYield$yield[9] <- 309.8
m0 <- lm(yield~block+factor(casein), data=cowmilkYield)
anova(m0)

aggregate(yield~casein, data=cowmilkYield, FUN=mean)

addmargins(with(cowmilkYield,
                tapply(yield, list(casein, block), FUN=sum)))

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(cowmilkYield)
str(cowmilkYield)

xyplot(yield~casein, groups=block,
       data=cowmilkYield, type="o",
       ylab=expression(Milk~yield~(kg)),
       xlab=expression(Casein~(g~day^{-1})))

rm(list=ls())
load("../data/cowmilkYield.RData")
ls()
str(cowmilkYield)

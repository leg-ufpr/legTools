##----------------------------------------------------------------------
## Data generation.

cassavaYield <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_mandioca.txt",
                           header=TRUE, sep="\t")
names(cassavaYield) <- c("block", "variety", "yield")
str(cassavaYield)

cassavaYield <- cassavaYield[with(cassavaYield, order(block, variety)),]

save(cassavaYield, file="../data/cassavaYield.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(yield~variety, data=cassavaYield,
       groups=block, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/cassavaYield.RData")
ls()
str(cassavaYield)

m0 <- lm(yield~block+variety, data=cassavaYield)

## Accessing the model assumptions.
par(mfrow=c(2,2)); plot(m0); layout(1)

## ANOVA table.
anova(m0)

library(doBy)
ls <- LSmatrix(m0, effect="variety")
dput(ls)

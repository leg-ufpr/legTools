##----------------------------------------------------------------------
## Data generation.

potatoYield <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_batatinha.txt",
                          header=TRUE, sep="\t")
names(potatoYield) <- c("block", "variety", "yield")
str(potatoYield)

potatoYield <- potatoYield[with(potatoYield, order(block, variety)),]

save(potatoYield, file="../data/potatoYield.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(yield~variety, data=potatoYield,
       groups=block, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/potatoYield.RData")
ls()
str(potatoYield)

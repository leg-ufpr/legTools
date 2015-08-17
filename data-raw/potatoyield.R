##----------------------------------------------------------------------
## Data generation.

potatoyield <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_batatinha.txt",
                          header=TRUE, sep="\t")
names(potatoyield) <- c("block", "variety", "yield")
str(potatoyield)

potatoyield <- potatoyield[with(potatoyield, order(block, variety)),]

save(potatoyield, file="../data/potatoyield.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(yield~variety, data=potatoyield,
       groups=block, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/potatoyield.RData")
ls()
str(potatoyield)

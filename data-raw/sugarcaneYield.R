##----------------------------------------------------------------------
## Data generation.

sugarcaneYield <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_canadeacucar1.txt",
                             header=TRUE, sep="\t")
names(sugarcaneYield) <- c("block", "variety", "yield")
str(sugarcaneYield)

sugarcaneYield <- sugarcaneYield[with(sugarcaneYield, order(block, variety)),]

save(sugarcaneYield, file="../data/sugarcaneYield.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(yield~variety, data=sugarcaneYield,
       groups=block, type="o",
       ylab=expression(Yield~(kg~plot^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/sugarcaneYield.RData")
ls()
str(sugarcaneYield)

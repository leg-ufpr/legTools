##----------------------------------------------------------------------
## Data generation.

plowing <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_aradura.txt",
                      header=TRUE, sep="\t")
names(plowing) <- c("plow", "block", "yield")
levels(plowing$plow) <- c("normal", "deep")
str(plowing)

plowing <- plowing[with(plowing, order(block, plow)),]

save(plowing, file="../data/plowing.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(yield~plow|block, data=plowing, type=c("p", "a"),
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Plowing level")

rm(list=ls())
load("../data/plowing.RData")
ls()
str(plowing)

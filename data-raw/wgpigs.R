##----------------------------------------------------------------------
## Data generation.

wgpigs <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_racoes.txt",
                     header=TRUE, sep="\t")
names(wgpigs) <- c("ft","wg")
str(wgpigs)

save(wgpigs, file="../data/wgpigs.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(wg~ft, data=wgpigs,
       ylab="Weight gain (kg)",
       xlab="Feeding type")

rm(list=ls())
load("../data/wgpigs.RData")
ls()
str(wgpigs)

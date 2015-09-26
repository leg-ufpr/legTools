##----------------------------------------------------------------------
## Data generation.

wgPigs <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_racoes.txt",
                     header=TRUE, sep="\t")
names(wgPigs) <- c("ft","wg")
str(wgPigs)

save(wgPigs, file="../data/wgPigs.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(wg~ft, data=wgPigs,
       ylab="Weight gain (kg)",
       xlab="Feeding type")

rm(list=ls())
load("../data/wgPigs.RData")
ls()
str(wgPigs)

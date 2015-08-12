##----------------------------------------------------------------------
## Data generation.

da <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_racoes.txt",
                 header=TRUE, sep="\t")
names(da) <- c("ft","wg")
str(da)

save(da, file="../data/wgpigs.rda")
wgpigs <- da

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(wg~ft, data=wgpigs,
       ylab="Weight gain (kg)",
       xlab="Feeding type")

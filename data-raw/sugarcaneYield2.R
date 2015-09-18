##----------------------------------------------------------------------
## Data generation.

sugarcaneYield2 <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_canadeacucar2.txt",
                              header=TRUE, sep="\t")
names(sugarcaneYield2) <- c("row", "col", "variety", "yield")
sugarcaneYield2 <- transform(sugarcaneYield2, row=factor(row), col=factor(col))
str(sugarcaneYield2)

sugarcaneYield2 <- sugarcaneYield2[with(sugarcaneYield2, order(row, col)),]

save(sugarcaneYield2, file="../data/sugarcaneYield2.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

xyplot(yield~variety|col,  groups=row, data=sugarcaneYield2,
       ylab=expression(Yield~(kg~plot^{-1})),
       xlab="Variety")

## display.brewer.all()

levelplot(yield~row+col,
          data=sugarcaneYield2, aspect="iso",
          xlab="Row", ylab="Column",
          main=expression(Yield~(kg~plot^{-1})),
          col.regions=colorRampPalette(
              colors=brewer.pal(n=11, name="Spectral")))+
    layer(with(sugarcaneYield2,
               panel.text(x=row, y=col,
                          label=paste(variety, yield))))

aggregate(yield~row, data=sugarcaneYield2, FUN=mean)
aggregate(yield~col, data=sugarcaneYield2, FUN=mean)
aggregate(yield~variety, data=sugarcaneYield2, FUN=mean)

rm(list=ls())
load("../data/sugarcaneYield2.RData")
ls()
str(sugarcaneYield2)

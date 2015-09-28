##----------------------------------------------------------------------
## Data generation.

sugarcaneYield3 <- read.table("http://www.leg.ufpr.br/~walmes/data/pimentel_crotalaria.txt",
                              header=TRUE, sep="\t")
names(sugarcaneYield3) <- c("row", "col", "fertil", "yield")
sugarcaneYield3 <- transform(sugarcaneYield3, row=factor(row),
                             col=factor(col))

aggregate(yield~fertil, data=sugarcaneYield3, FUN=mean)
levels(sugarcaneYield3$fertil) <- c("ABC", "ABc", "Ab", "aBC", "aBc", "ab")
str(sugarcaneYield3)

sugarcaneYield3 <- sugarcaneYield3[with(sugarcaneYield3, order(row, col)),]

save(sugarcaneYield3, file="../data/sugarcaneYield3.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

xyplot(yield~fertil|col,  groups=row, data=sugarcaneYield3,
       ylab=expression(Yield~(kg~plot^{-1})),
       xlab="Fertilization", scales=list(x=list(rot=90)))

## display.brewer.all()

levelplot(yield~row+col,
          data=sugarcaneYield3, aspect="iso",
          xlab="Row", ylab="Column",
          main=expression(Yield~(kg~plot^{-1})),
          col.regions=colorRampPalette(
              colors=brewer.pal(n=11, name="Spectral")))+
    layer(with(sugarcaneYield3,
               panel.text(x=row, y=col,
                          label=sprintf("%s\n%0.2f", fertil, yield))))

aggregate(yield~row, data=sugarcaneYield3, FUN=mean)
aggregate(yield~col, data=sugarcaneYield3, FUN=mean)
aggregate(yield~fertil, data=sugarcaneYield3, FUN=mean)

## The incomplete factorial structure.
X <- mapply(FUN=grepl, c("A", "B", "C"),
            MoreArgs=list(x=sugarcaneYield3$fertil))*1
sugarcaneYield3 <- cbind(sugarcaneYield3, as.data.frame(X))

ftable(with(sugarcaneYield3, tapply(yield, list(B, A, C), FUN=mean)))
aggregate(yield~A+B+C, data=sugarcaneYield3, FUN=mean)

rm(list=ls())
load("../data/sugarcaneYield3.RData")
ls()
str(sugarcaneYield3)

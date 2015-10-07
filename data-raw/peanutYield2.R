##----------------------------------------------------------------------
## Data generation. Pimentel page 156.

peanutYield2 <- expand.grid(
    variety=c("40-Roxo", "54-Roxo", "49-Cateto", "53-Tatu"),
    loc=c("Pindorama 49/50", "Ribeirão Preto 49/50",
          "Campinas 48/49", "Campinas 42/43"),
    KEEP.OUT.ATTRS=FALSE)

peanutYield2$meanYield <-
    c(2100, 2160, 1570, 870, 2650, 2740, 1890, 1570, 2100, 1830, 1890,
      1370, 2710, 2610, 2590, 1590)

addmargins(with(peanutYield2,
                tapply(meanYield, list(variety, loc), FUN=sum)))

peanutYield2 <- peanutYield2[with(peanutYield2,
                                  order(loc, variety)),]

str(peanutYield2)

## Put MSE as an attibute to the data.frame.
mse <-  c(52900, 84700, 3970, 106900)
names(mse) <- levels(peanutYield2$loc)
attr(peanutYield2, which="MSE") <- mse
str(peanutYield2)

save(peanutYield2, file="../data/peanutYield2.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

data(peanutYield2)
str(peanutYield2)

xyplot(meanYield~variety, data=peanutYield2,
       groups=loc, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/peanutYield2.RData")
ls()
str(peanutYield2)

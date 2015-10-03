##----------------------------------------------------------------------
## Data generation.

peanutYield <- expand.grid(
    variety=c("40-Roxo", "54-Roxo", "49-Cateto", "53-Tatu"),
    loc=c("Campinas", "Ribeirão Preto", "Pindorama"),
    year=c("1941-42", "1942-43", "1949-50"),
    KEEP.OUT.ATTRS=FALSE)

peanutYield$meanYield <-
    c(1780, 1450, 1430, 790, 690, 470, 520, 280, 4400, 4330, 3440, 3710,
      2610, 2590, 2710, 1590, 1570, 1330, 1500, 1170, 1850, 2010, 2240,
      1790, 2570, 2320, 2130, 2220, 2650, 2740, 1890, 1570, 2100, 2160,
      1570, 870)

addmargins(with(peanutYield,
                tapply(meanYield, list(variety, loc), FUN=sum)))

peanutYield <- peanutYield[with(peanutYield,
                                order(year, loc, variety)),]

str(peanutYield)

## save(peanutYield, file="../data/peanutYield.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(meanYield~variety|year, data=peanutYield,
       groups=loc, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/peanutYield.RData")
ls()
str(peanutYield)

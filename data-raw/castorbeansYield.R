##----------------------------------------------------------------------
## Data generation.

castorbeansYield <- expand.grid(
    variety=c("V 38", "L 41", "L 168", "L 176", "L 178", "L 881",
              "L 882", "L 883", "L 1.000"),
    loc=c("Ribeirão Preto", "Pindorama", "Mococa", "Tietê",
          "Santa Rita"),
    KEEP.OUT.ATTRS=FALSE)

castorbeansYield$meanYield <-
    c(1186, 1219, 1005, 1264, 1272, 1151, 1246, 1223, 1168, 1460, 1598,
      1825, 1394, 1407, 1436, 1291, 1622, 1521, 1832, 1595, 1851, 1613,
      1747, 2297, 2233, 2391, 1992, 1644, 1422, 1458, 1567, 1532, 1532,
      1683, 1699, 1467, 2192, 2294, 1920, 1856, 2178, 2026, 2458, 2040,
      1963)

addmargins(with(castorbeansYield,
                tapply(meanYield, list(variety, loc), FUN=sum)))

castorbeansYield <- castorbeansYield[with(castorbeansYield,
                                          order(loc, variety)),]

## Put MSE as an attibute to the data.frame.
mse <-  c(29930, 69170, 88210, 35720, 64520)
names(mse) <- levels(castorbeansYield$loc)
attr(castorbeansYield, which="MSE") <- mse
str(castorbeansYield)

save(castorbeansYield, file="../data/castorbeansYield.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

data(castorbeansYield)
str(castorbeansYield)

xyplot(meanYield~variety, data=castorbeansYield,
       groups=loc, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/castorbeansYield.RData")
ls()
str(castorbeansYield)

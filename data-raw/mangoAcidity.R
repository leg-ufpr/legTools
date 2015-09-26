##----------------------------------------------------------------------
## Data generation.

mangoAcidity <- expand.grid(variety=c("Bourbon", "Brasil", "Extrema",
                                      "Maçã", "Non Plus Ultra",
                                      "Oliveira"),
                            year=c(1957:1959),
                            month=c("N", "D", "J"),
                            KEEP.OUT.ATTRS=FALSE)
mangoAcidity$acid <- c(28.2, 38.3, 37.6, 47.2, 36.4, 40.0, 24.7, 32.0,
                       39.0, 47.7, 35.3, 30.1, 16.4, 38.4, 50.3, 50.2,
                       40.1, 30.0, 6.0, 5.0, 6.0, 6.2, 7.9, 6.0, 6.2,
                       3.6, 6.6, 6.9, 10.2, 7.0, 6.9, 4.0, 6.9, 7.9,
                       9.8, 8.4, 4.6, 4.6, 5.4, 5.2, 4.9, 6.1, 4.4,
                       4.2, 5.4, 4.2, 6.8, 3.5, 5.0, 3.8, 5.0, 5.0,
                       7.0, 4.8)
str(mangoAcidity)

save(mangoAcidity, file="../data/mangoAcidity.RData")

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

data(mangoAcidity)
str(mangoAcidity)

## reshape::cast() can also be used.
with(mangoAcidity,
     ftable(tapply(acid,
                   list(variety, year, month),
                   FUN=identity)))

xyplot(acid~month|variety, groups=year,
       data=mangoAcidity, type=c("p", "a"),
       auto.key=TRUE,
       ylab="Acidity",
       xlab="Month")

rm(list=ls())
load("../data/mangoAcidity.RData")
ls()
str(mangoAcidity)

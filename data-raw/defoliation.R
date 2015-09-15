##----------------------------------------------------------------------
## Data generation.

phenol <- c("vegetative", "flower bud", "blossom", "boll", "boll open")
defoliation <- expand.grid(rept=1:5,
                           defol=seq(0, 1, length.out=5),
                           phenol=factor(phenol, levels=phenol),
                           KEEP.OUT.ATTRS=FALSE)

defoliation$bolls <- c(10, 9, 8, 8, 10, 11, 9, 10, 10, 10, 8, 8, 10, 8,
                       9, 9, 7, 7, 8, 9, 8, 6, 6, 5, 6, 7, 8, 8, 9, 10,
                       9, 12, 7, 10, 9, 8, 9, 9, 10, 8, 11, 10, 7, 8, 8,
                       7, 7, 7, 7, 8, 10, 9, 8, 12, 8, 7, 5, 5, 7, 5, 6,
                       5, 7, 4, 7, 8, 5, 7, 6, 4, 5, 5, 4, 4, 5, 8, 10,
                       7, 8, 10, 9, 6, 6, 8, 6, 9, 7, 11, 8, 9,6, 6, 6,
                       6, 7, 3, 3, 2, 4, 3, 11, 7, 9, 12 , 11, 9, 13, 8,
                       10, 10, 9, 7, 7, 9, 9, 8, 8, 10, 8, 10, 9, 8, 10,
                       8, 10)

defoliation <- defoliation[,c(3,2,1,4)]
str(defoliation)
save(defoliation, file="../data/defoliation.RData")
rm(list=ls())
load(file="../data/defoliation.RData")
ls()

##----------------------------------------------------------------------
## Examples.

library(lattice)
library(latticeExtra)

x11(width=7, height=2.8)
xyplot(bolls~defol|phenol, data=defoliation,
       layout=c(NA, 1), type=c("p", "smooth"),
       xlab="Artificial defoliation level",
       ylab="Number of bolls produced",
       xlim=extendrange(c(0:1), f=0.15), jitter.x=TRUE)

## Sample mean and variance in each treatment cell.
mv <- aggregate(bolls~phenol+defol, data=defoliation,
                FUN=function(x) c(mean=mean(x), var=var(x)))
str(mv)

xlim <- ylim <- extendrange(c(mv$bolls), f=0.05)

## Evidence in favor of the underdispersion.
xyplot(bolls[,"var"]~bolls[,"mean"], data=mv,
       aspect="iso", xlim=xlim, ylim=ylim,
       ylab="Sample variance", xlab="Sample mean")+
    layer(panel.abline(a=0, b=1, lty=2))

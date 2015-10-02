##----------------------------------------------------------------------
## Data generation.

potatoYield2 <- expand.grid(
    variety=c("Kennebec", "B 25-50 E", "B 1-52", "Huinkul",
              "B 116-51", "B 72-53 A", "S. Rafaela", "Buena Vista"),
    loc=gl(7, 1),
    KEEP.OUT.ATTRS=FALSE)

potatoYield2$sumYield <- c(470, 483, 646, 822, 611, 694, 685, 477,
                           318, 650, 1201, 1205, 1223, 1112, 1176, 426,
                           428, 660, 891, 1002, 900, 912, 1018, 497,
                           584, 780, 928, 970, 954, 865, 703, 682,
                           364, 356, 386, 558, 546, 450, 558, 356,
                           482, 358, 439, 624, 523, 519, 488, 496,
                           492, 583, 940, 929, 928, 797, 929, 532)/10

addmargins(with(potatoYield2,
                tapply(SumYield, list(variety, loc), FUN=sum)))

potatoYield2 <- potatoYield2[with(potatoYield2, order(loc, variety)),]

## Put MSE as an attibute to the data.frame.
mse <-  c(315, 263, 855, 209, 325, 199, 535)/100
names(mse) <- paste0("loc:", 1:length(mse))
attr(potatoYield2, which="MSE") <- mse
str(potatoYield2)

## save(potatoYield2, file="../data/potatoYield2.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(sumYield/4~variety, data=potatoYield2,
       groups=loc, type="o",
       ylab=expression(Yield~(t~ha^{-1})),
       xlab="Variety")

rm(list=ls())
load("../data/potatoYield2.RData")
ls()
str(potatoYield2)

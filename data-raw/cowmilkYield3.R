##----------------------------------------------------------------------
## Data generation. Pimentel page 272.

## cowmilkYield3 <- read.table("clipboard", header=FALSE, sep="\t")
## cowmilkYield3[, 5] <- as.numeric(sub(x=cowmilkYield3[, 5], ",", "."))
## names(cowmilkYield3) <- c("period", "block", "cow", "treat", "yield")
## dput(cowmilkYield3)

cowmilkYield3 <- data.frame(
    period = factor(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
                      2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                      3, 3)),
    block = factor(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 1, 1,
                     1, 2, 2, 2, 3, 3, 3, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3,
                     3, 3)),
    cow = factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5,
                   6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                   10, 11, 12)),
    treat = factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 2, 3, 1, 3, 1,
                     2, 3, 1, 2, 2, 3, 1, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1,
                     2, 3)),
    yield = c(34.6, 22.8, 32.9, 48.9, 21.8, 25.4, 30.4, 35.2, 30.8,
              38.7, 25.7, 21.4, 32.3, 21, 33.1, 46.9, 23.9, 26, 29.5,
              33.5, 29.3, 37.4, 26.1, 22, 28.5, 18.6, 27.5, 42, 21.7,
              23.9, 26.7, 28.4, 26.4, 34.4, 23.4, 19.4))

str(cowmilkYield3)

save(cowmilkYield3, file="../data/cowmilkYield3.RData")

##----------------------------------------------------------------------

m0 <- aov(yield~period+block+cow+treat,
          data=cowmilkYield3)
anova(m0)

with(cowmilkYield3, tapply(yield, list(block, period), FUN=sum))
unstack(x=cowmilkYield3, form=yield~cow)

db <- reshape(data=cowmilkYield3, idvar="cow", timevar="period",
              v.names="yield", direction="wide")

db$y <- apply(db[, 4:6], MARGIN=1,
              FUN=function(x) sum(x*c(1,-2,1)))

m0 <- aov(y~block+treat, data=db)
anova(m0)

## This results differs from the book. I don't know why.

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(cowmilkYield3)
str(cowmilkYield3)

xyplot(yield~period|cow, groups=treat,
       data=cowmilkYield3, type="o",
       ylab="Milk yield",
       xlab="Period")

rm(list=ls())
load("../data/cowmilkYield3.RData")
ls()
str(cowmilkYield3)

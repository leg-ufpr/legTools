##----------------------------------------------------------------------
## Data generation. Pimentel page 272.

cowmilkYield2 <- expand.grid(period=gl(3, 1),
                             cow=gl(3, 1),
                             group=gl(4, 1))
cowmilkYield2$feed <- factor(c(1, 3, 2, 2, 1, 3, 3, 2, 1, 1, 2, 3, 2, 3,
                               1, 3, 1, 2, 1, 3, 2, 2, 1, 3, 3, 2, 1, 1,
                               2, 3, 2, 3, 1, 3, 1, 2),
                             labels=c("A", "B", "C"))
cowmilkYield2$yield <- c(527L, 883L, 785L, 696L, 635L, 901L, 989L, 899L,
                         657L, 608L, 715L, 844L, 885L, 1087L, 711L,
                         940L, 766L, 832L, 472L, 819L, 778L, 734L, 644L,
                         953L, 897L, 766L, 706L, 586L, 723L, 892L, 635L,
                         799L, 595L, 805L, 542L, 681L)

names(cowmilkYield2)
cowmilkYield2 <- cowmilkYield2[, c(3,1,2,4,5)]
cowmilkYield2 <- cowmilkYield2[
    with(cowmilkYield2, order(group, period, cow)), ]
str(cowmilkYield2)

save(cowmilkYield2, file="../data/cowmilkYield2.RData")

##----------------------------------------------------------------------

m0 <- aov(terms(yield~group/(cow+period)+feed+group:feed,
                keep.order=TRUE),
          data=cowmilkYield2)
anova(m0)

m0 <- aov(terms(yield~group/(cow+period)+feed,
                keep.order=TRUE),
          data=cowmilkYield2)
anova(m0)

aggregate(yield~feed, data=cowmilkYield2, FUN=mean)

addmargins(with(cowmilkYield2,
                tapply(yield, list(feed, group), FUN=sum)))

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(cowmilkYield2)
str(cowmilkYield2)

xyplot(yield~feed, groups=group,
       data=cowmilkYield2, type=c("p", "a"),
       ylab=expression(Milk~yield~(kg)),
       xlab="Feed")

rm(list=ls())
load("../data/cowmilkYield2.RData")
ls()
str(cowmilkYield2)

##----------------------------------------------------------------------
## Data generation. Pimentel page 215.

cornYield3 <- read.table("clipboard", header=FALSE, sep="\t")
cornYield3[, 4] <- as.numeric(sub(x=cornYield3[, 4], ",", "."))
names(cornYield3) <- c("rept", "block", "hybrid", "yield")
cornYield3 <- transform(cornYield3, rept=factor(rept),
                        block=factor(block), hybrid=factor(hybrid))
cornYield3 <- cornYield3[
    with(cornYield3, order(rept, block, hybrid)), ]
dput(cornYield3)

cornYield3 <- structure(list(
    rept = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                       1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                       2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L,
                       3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                     .Label = c("1", "2", "3"),
                     class = "factor"),
    block = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L,
                        4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
                        3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L,
                        2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L),
                      .Label = c("1", "2", "3", "4"),
                      class = "factor"),
    hybrid = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                         12L, 13L, 14L, 15L, 16L, 1L, 5L, 9L, 13L, 2L,
                         6L, 10L, 14L, 3L, 7L, 11L, 15L, 4L, 8L, 12L,
                         16L, 1L, 6L, 11L, 16L, 2L, 5L, 12L, 15L, 3L,
                         8L, 9L, 14L, 4L, 7L, 10L, 13L),
                       .Label = c("1", "2", "3", "4", "5", "6", "7",
                                  "8", "9", "10", "11", "12", "13",
                                  "14", "15", "16"),
                       class = "factor"),
    yield = c(2, 2.9, 2.2, 3.9, 2.3, 2.5, 1.4, 1.7, 1.6, 3, 1.5, 2.1,
              2.3, 3.4, 2, 2.8, 2.2, 2.3, 2.7, 1.4, 3.1, 2.8, 2.6, 2.8,
              3.1, 2.9, 2.5, 2.4, 4, 2.8, 2.7, 1.5, 3, 2.9, 2.6, 3.1,
              1.8, 1.9, 2.9, 2.5, 1.7, 2, 1.4, 2.3, 4.4, 3.7, 3.3,
              2.2)),
              row.names=1:48,
              class = "data.frame")

str(cornYield3)

save(cornYield3, file="../data/cornYield3.RData")

##----------------------------------------------------------------------

aggregate(yield~block+rept, data=cornYield3, FUN=sum)
aggregate(yield~hybrid, data=cornYield3, FUN=sum)

m0 <- aov(terms(yield~rept/block+hybrid, keep.order=TRUE),
          data=cornYield3)
anova(m0)

library(doBy)
L <- LSmeans(m0, effect="hybrid")

str(L)
L$K

##----------------------------------------------------------------------
## Examples.

require(lattice)

data(cornYield3)
str(cornYield3)

xyplot(yield~hybrid|rept, groups=block,
       data=cornYield3, type="b",
       ylab=expression(Yield~(kg~plot^{-1})),
       xlab="Hybrid")

xyplot(yield~hybrid, data=cornYield3,
       jitter.x=TRUE, type=c("p", "a"),
       ylab=expression(Yield~(kg~plot^{-1})),
       xlab="Hybrid")

g <- nlevels(cornYield3$hybrid)
a <- seq(0, by=(2*pi)/(g), length.out=g)
y <- sin(a)
x <- cos(a)
plot(y~x, asp=1, xlim=c(-1,1), ylim=c(-1,1))

for (b in levels(cornYield3$block)){
    cbn <- combn(x=as.integer(cornYield3$hybrid[cornYield3$block==b]),
                 m=2)
    segments(
        x0=x[cbn[1,]], y0=y[cbn[1,]],
        x1=x[cbn[2,]], y1=y[cbn[2,]], col=b)
}


rm(list=ls())
load("../data/cornYield3.RData")
ls()
str(cornYield3)

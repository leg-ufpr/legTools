##----------------------------------------------------------------------
## Data generation. Pimentel page 198.

## bib3asin <- read.table("clipboard", header=TRUE, sep="\t")
## bib3asin[, 3] <- as.numeric(sub(x=bib3asin[, 3], ",", "."))
## names(bib3asin) <- c("block", "treat", "z")
## bib3asin <- transform(bib3asin,
##                       block=factor(block), treat=factor(treat))
## dput(bib3asin)

bib3asin <- structure(list(
    block = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,
                        12L, 13L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,
                        10L, 11L, 12L, 13L, 1L, 2L, 3L, 4L, 5L, 6L, 7L,
                        8L, 9L, 10L, 11L, 12L, 13L, 1L, 2L, 3L, 4L, 5L,
                        6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L),
                      .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13"),
                      class = "factor"),
    treat = structure(c(1L, 2L, 1L, 3L, 4L, 1L, 2L, 1L, 6L, 3L, 5L, 4L,
                        2L, 2L, 8L, 7L, 4L, 8L, 3L, 3L, 5L, 10L, 7L, 9L,
                        5L, 6L, 4L, 12L, 11L, 6L, 9L, 9L, 5L, 6L, 11L,
                        8L, 10L, 7L, 7L, 10L, 13L, 12L, 12L, 11L, 13L,
                        11L, 8L, 13L, 10L, 12L, 13L, 9L),
                      .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13"),
                      class = "factor"),
    z = c(52.7, 71.6, 58.9, 75, 71.6, 56.8, 75, 48.8, 63.4, 71.6, 75,
          63.4, 71.6, 79.5, 56.8, 75, 65.9, 75, 65.9, 65.9, 68.6, 90,
          63.4, 68.6, 61.1, 63.4, 63.4, 71.6, 75, 79.5, 71.6, 71.6,
          65.9, 63.4, 90, 61.1, 75, 50.8, 61.1, 65.9, 65.9, 71.6, 79.5,
          79.5, 65.9, 90, 61.1, 71.6, 58.9, 75, 79.5, 75)),
          .Names = c("block", "treat", "z"),
          row.names = c(NA, -52L),
          class = "data.frame")

bib <- bib3asin[with(bib3asin, order(block, treat)), ]

save(bib3asin, file="../data/bib3asin.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

data(bib3asin)
str(bib3asin)

xyplot(z~treat|block, data=bib3asin,
       ylab="Arc sin of heathy plants fraction",
       xlab="Treatment")

## Why not consider a beta distribution for p?
bib3asin$p <- sin(bib3asin$z*pi/180)^2

xyplot(p~treat|block, data=bib3asin,
       ylab="Fraction of healthy plants",
       xlab="Treatment")

g <- nlevels(bib3asin$treat)
a <- seq(0, by=(2*pi)/(g), length.out=g)
y <- sin(a)
x <- cos(a)
plot(y~x, asp=1, xlim=c(-1,1), ylim=c(-1,1))

for (b in levels(bib3asin$block)){
    cbn <- combn(x=as.integer(bib3asin$treat[bib3asin$block==b]),
                 m=2)
    segments(
        x0=x[cbn[1,]], y0=y[cbn[1,]],
        x1=x[cbn[2,]], y1=y[cbn[2,]], col=b)
}

rm(list=ls())
load("../data/bib3asin.RData")
ls()
str(bib3asin)

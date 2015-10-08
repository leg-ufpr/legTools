##----------------------------------------------------------------------
## Data generation. Pimentel page 185.

## bib3 <- read.table("clipboard", header=TRUE, sep="\t")
## names(bib3) <- c("block", "treat", "y")
## bib3 <- transform(bib3,
##                   block=factor(block), treat=factor(treat))
## dput(bib3)

bib3 <- structure(list(
    block = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L,
                        5L, 5L, 5L, 6L, 6L, 6L, 7L, 7L, 7L, 8L, 8L, 8L,
                        9L, 9L, 9L, 10L, 10L, 10L),
                      .Label = c("1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10"),
                      class = "factor"),
    treat = structure(c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 2L, 5L, 1L, 3L, 4L,
                        1L, 3L, 5L, 1L, 4L, 5L, 2L, 3L, 4L, 2L, 3L, 5L,
                        2L, 4L, 5L, 3L, 4L, 5L),
                      .Label = c("1", "2", "3", "4", "5"),
                      class = "factor"),
    y = c(35L, 28L, 27L, 30L, 20L, 22L, 28L, 16L, 18L, 36L, 29L, 30L,
          29L, 19L, 22L, 25L, 16L, 19L, 26L, 30L, 28L, 27L, 29L, 27L,
          29L, 29L, 27L, 27L, 26L, 29L)),
          .Names = c("block", "treat", "y"),
          row.names = c(NA, -30L),
          class = "data.frame")

bib <- bib3[with(bib3, order(block, treat)), ]

save(bib3, file="../data/bib3.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

data(bib3)
str(bib3)

xyplot(y~treat|block, data=bib3,
       ylab="Y",
       xlab="Treatment")

g <- nlevels(bib3$treat)
a <- seq(0, by=(2*pi)/(g), length.out=g)
y <- sin(a)
x <- cos(a)
plot(y~x, asp=1, xlim=c(-1,1), ylim=c(-1,1))

for (b in levels(bib3$block)){
    cbn <- combn(x=as.integer(bib3$treat[bib3$block==b]),
                 m=2)
    segments(
        x0=x[cbn[1,]], y0=y[cbn[1,]],
        x1=x[cbn[2,]], y1=y[cbn[2,]], col=b)
}

rm(list=ls())
load("../data/bib3.RData")
ls()
str(bib3)

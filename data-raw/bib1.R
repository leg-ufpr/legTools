##----------------------------------------------------------------------
## Data generation. Pimentel page 190.

## bib1 <- read.table("clipboard", header=TRUE, sep="\t")
## names(bib1) <- c("rept", "treat", "block", "y")
## bib1 <- transform(bib1, rept=factor(rept),
##                   block=factor(block), treat=factor(treat))

bib1 <- 
structure(list(rept = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 
7L), .Label = c("1", "2", "3", "4", "5", "6", "7"), class = "factor"), 
    treat = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 3L, 
    2L, 8L, 4L, 5L, 6L, 7L, 1L, 4L, 2L, 7L, 3L, 6L, 5L, 8L, 1L, 
    5L, 2L, 3L, 4L, 7L, 6L, 8L, 1L, 6L, 2L, 4L, 3L, 8L, 5L, 7L, 
    1L, 7L, 2L, 6L, 3L, 5L, 4L, 8L, 1L, 8L, 2L, 5L, 3L, 7L, 4L, 
    6L), .Label = c("1", "2", "3", "4", "5", "6", "7", "8"), class = "factor"), 
    block = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 
    2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 
    1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 
    1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 
    4L), .Label = c("1", "2", "3", "4"), class = "factor"), y = c(20L, 
    18L, 15L, 16L, 14L, 15L, 16L, 18L, 24L, 18L, 25L, 19L, 13L, 
    16L, 12L, 16L, 23L, 17L, 26L, 18L, 15L, 17L, 13L, 16L, 21L, 
    13L, 23L, 16L, 10L, 12L, 13L, 11L, 28L, 14L, 27L, 18L, 18L, 
    15L, 16L, 17L, 22L, 17L, 24L, 16L, 18L, 14L, 15L, 17L, 23L, 
    15L, 21L, 13L, 15L, 12L, 13L, 16L)), .Names = c("rept", "treat", 
"block", "y"), row.names = c(NA, -56L), class = "data.frame")

str(bib1)

bib <- bib1[with(bib1, order(rept, block, treat)), ]

save(bib1, file="../data/bib1.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(y~treat|rept, groups=block, data=bib1, type="b",
       ylab="Y", xlab="Treatment")

xyplot(y~treat, data=bib1, jitter.x=TRUE,
       ylab="Y", xlab="Treatment")

rm(list=ls())
load("../data/bib1.RData")
ls()
str(bib1)

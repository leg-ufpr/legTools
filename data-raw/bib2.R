##----------------------------------------------------------------------
## Data generation. Pimentel page 192.

## bib2 <- read.table("clipboard", header=TRUE, sep="\t")
## bib2$y <- as.numeric(sub(x=bib2$y, ",", "."))
## names(bib2) <- c("rept", "block", "treat", "y")
## bib2 <- transform(bib2, rept=factor(rept),
##                   block=factor(block), treat=factor(treat))
## dput(bib2)

bib2 <- 
structure(list(rept = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L), .Label = c("1", "2", "3"), class = "factor"), block = structure(c(1L, 
1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 1L, 1L, 2L, 
2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 1L, 1L, 2L, 2L, 3L, 
3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L), .Label = c("1", "2", "3", 
"4", "5", "6", "7"), class = "factor"), treat = structure(c(1L, 
2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 1L, 1L, 3L, 3L, 
5L, 5L, 7L, 7L, 2L, 2L, 4L, 4L, 6L, 6L, 1L, 1L, 4L, 4L, 7L, 7L, 
3L, 3L, 6L, 6L, 2L, 2L, 5L, 5L, 1L), .Label = c("1", "2", "3", 
"4", "5", "6", "7"), class = "factor"), y = c(3.5, 2.8, 3.2, 
3.7, 3.5, 2.5, 2.8, 2.7, 3, 3.2, 2.4, 2.6, 3.1, 2.7, 3.8, 4, 
3.6, 2.7, 2.3, 3, 2.8, 2.5, 2.6, 2.8, 2.3, 2.4, 2.8, 3.3, 3, 
2.2, 2.7, 3.4, 3.2, 3.9, 3.3, 2.4, 2.8, 3.4, 2.9, 2.6, 2.3, 3.3
)), .Names = c("rept", "block", "treat", "y"), row.names = c(NA, 
-42L), class = "data.frame")
str(bib2)

bib <- bib2[with(bib2, order(rept, block, treat)), ]

save(bib2, file="../data/bib2.RData")

##----------------------------------------------------------------------
## Examples.

require(lattice)

xyplot(y~treat|rept, groups=block, data=bib2, type="b",
       ylab="Y", xlab="Treatment")

xyplot(y~treat, data=bib2, jitter.x=TRUE,
       ylab="Y", xlab="Treatment")

rm(list=ls())
load("../data/bib2.RData")
ls()
str(bib2)

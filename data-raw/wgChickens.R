##----------------------------------------------------------------------
## Data generation. Pimentel page 267.

## wgChickens <- read.table("clipboard", header=FALSE, sep="\t")
## wgChickens <- wgChickens[with(wgChickens, order(V1, V2)), ]
## names(wgChickens) <- c("gender", "conc", "n", "twg")
## dput(wgChickens)

wgChickens <- structure(list(
    gender = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
                         2L, 2L, 2L, 2L),
                       .Label = c("F", "M"),
                       class = "factor"), 
    conc = c(0L, 0L, 10L, 10L, 20L, 20L, 30L, 30L, 0L, 0L, 10L, 10L,
             20L, 20L, 30L, 30L),
    n = c(12L, 12L, 13L, 12L, 13L, 12L, 13L, 12L, 13L, 13L, 13L, 13L,
          13L, 13L, 13L, 13L),
    twg = c(399L, 388L, 503L, 508L, 475L, 437L, 398L, 448L, 548L, 512L,
            689L, 646L, 543L, 611L, 514L, 537L)),
            .Names = c("gender", "conc", "n", "twg"),
            row.names = c(9L, 13L, 10L, 14L, 11L, 15L, 12L, 16L, 1L, 5L,
                          2L, 6L, 3L, 7L, 4L, 8L),
            class = "data.frame")

save(wgChickens, file="../data/wgChickens.RData")

##----------------------------------------------------------------------

m0 <- lm(twg~factor(conc)*gender, data=wgChickens)
anova(m0)

## The number of animals is not considered in the book analysis.
m0 <- lm((twg/n)~factor(conc)*gender, data=wgChickens, weights=n)
anova(m0)

##----------------------------------------------------------------------
## Examples.

library(lattice)

data(wgChickens)
str(wgChickens)

xyplot(twg/n~conc, groups=gender,
       data=wgChickens, type=c("p", "a"),
       auto.key=list(columns=2, corner=c(0.95, 0.95),
                     title="Gender"),
       ylab="Mean weight gain (kg)",
       xlab="Sorghum concentration in the feed (%)")

rm(list=ls())
load("../data/wgChickens.RData")
ls()
str(wgChickens)

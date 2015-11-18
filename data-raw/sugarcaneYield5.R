##----------------------------------------------------------------------
## Data generation.

x <- read.table(file = 'clipboard', sep = "\t", header=TRUE)
dput(x)

sugarcaneYield5 <- structure(list(linha = c(1L, 1L, 1L, 1L, 1L, 
                                            1L, 1L, 1L, 1L, 
1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 
5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L), coluna = c(1L, 1L, 2L, 2L, 
3L, 3L, 4L, 4L, 5L, 5L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 
1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 1L, 1L, 2L, 2L, 3L, 3L, 
4L, 4L, 5L, 5L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L), 
variedade = c(1L, 
1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 3L, 3L, 5L, 5L, 1L, 1L, 2L, 
2L, 4L, 4L, 4L, 4L, 3L, 3L, 5L, 5L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 
1L, 4L, 4L, 5L, 5L, 3L, 3L, 5L, 5L, 4L, 4L, 2L, 2L, 3L, 3L, 1L, 
1L), adubacao = structure(c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 
1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 
1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 
1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c("no", "yes"
), class = "factor"), producao = structure(c(33L, 30L, 7L, 5L, 
8L, 14L, 31L, 28L, 25L, 9L, 11L, 3L, 24L, 14L, 25L, 23L, 4L, 
2L, 25L, 27L, 34L, 27L, 1L, 37L, 12L, 1L, 35L, 26L, 3L, 6L, 15L, 
21L, 34L, 20L, 32L, 16L, 17L, 22L, 17L, 22L, 36L, 13L, 29L, 23L, 
10L, 8L, 13L, 18L, 36L, 19L), .Label = c("105,6", "117,6", "120", 
"122,4", "124,8", "127,2", "134,4", "144", "146,4", "151,2", 
"153,6", "158,4", "165,6", "168", "170,4", "172,8", "175,2", 
"177,6", "184,8", "192", "196,8", "199,2", "206,4", "218,4", 
"223,2", "228", "230,4", "244,8", "259,2", "261,6", "264", "278,4", 
"290,4", "292,8", "309,6", "312", "98,4"), class = "factor")), .Names = 
  c("row", "column", "variety", "fertilization", "yield"), 
class = "data.frame", row.names = c(NA, -50L))

sugarcaneYield5$yield <- as.numeric(sugarcaneYield5$yield)
sugarcaneYield5$row <- factor(sugarcaneYield5$row)
sugarcaneYield5$column <- factor(sugarcaneYield5$column)
sugarcaneYield5$variety <- factor(sugarcaneYield5$variety)
str(sugarcaneYield5)
save(sugarcaneYield5, file="../data/sugarcaneYield5.RData")

##----------------------------------------------------------------------
## Examples.
library(lattice)
library(latticeExtra)

data(sugarcaneYield5)
str(sugarcaneYield5)
load("../data/sugarcaneYield5.RData")

levelplot(yield~row+column|fertilization, 
         data=sugarcaneYield5, aspect="iso")

levelplot(yield~row+column|variety,
         data=sugarcaneYield5, aspect="iso")

xyplot(yield~variety, data=sugarcaneYield5, type=c("p","a"), 
        col="tomato", xlab="Variety", 
        ylab="Total Yield", main="Latin Square experiment with five 
        types of variety")


m <-lm(yield~row+column+variety+fertilization, data=sugarcaneYield5)
## Departures from homecedasticity and normality.
par(mfrow=c(2,2)); plot(m, which=1:3); layout(1)


rm(list=ls())
load("../data/sugarcaneYield.RData")
ls()
str(sugarcaneYield5)
##----------------------------------------------------------------------
## Data generation.

x <- read.table(file = 'clipboard', sep = "\t", header=TRUE)
dput(x)
mangoBrix <- structure(list(direcao = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 3L), .Label = c("east", "north", "west", 
"south"), class = "factor"), variedade = structure(c(2L, 2L, 2L, 
3L, 3L, 3L, 5L, 5L, 5L, 1L, 1L, 1L, 4L, 4L, 4L, 2L, 2L, 2L, 3L, 
3L, 3L, 5L, 5L, 5L, 1L, 1L, 1L, 4L, 4L, 4L, 2L, 2L, 2L, 3L, 3L, 
3L, 5L, 5L, 5L, 1L, 1L, 1L, 4L, 4L, 4L, 2L, 2L, 2L, 3L, 3L, 3L, 
5L, 5L, 5L, 1L, 1L, 1L, 4L, 4L, 4L), .Label = c("Bourbon", "Carlota", 
"Extrema", "Imperial", "Oliveira"), class = "factor"), 
brix = structure(c(30L, 
26L, 28L, 16L, 19L, 7L, 13L, 39L, 16L, 19L, 12L, 26L, 38L, 35L, 
40L, 23L, 37L, 21L, 12L, 5L, 3L, 15L, 6L, 17L, 8L, 1L, 11L, 36L, 
2L, 17L, 27L, 31L, 27L, 18L, 16L, 12L, 29L, 7L, 13L, 4L, 30L, 
20L, 9L, 32L, 33L, 27L, 24L, 18L, 33L, 26L, 8L, 14L, 9L, 17L, 
10L, 25L, 34L, 22L, 33L, 19L), .Label = c("13,2", "13,7", "14", 
"14,2", "14,3", "14,9", "15", "15,2", "15,3", "15,5", "15,8", 
"15,9", "16", "16,1", "16,2", "16,3", "16,4", "16,5", "16,6", 
"16,7", "16,9", "17", "17,1", "17,2", "17,3", "17,5", "17,6", 
"17,8", "17,9", "18", "18,1", "18,2", "18,3", "18,4", "18,5", 
"18,6", "18,8", "18,9", "19,5", "21,5"), class = "factor")), 
.Names = c("direction", 
"variety", "brix"), class = "data.frame", row.names = c(NA, 
-60L))

mangoBrix$brix <- as.numeric(mangoBrix$brix)
str(mangoBrix)
save(sugarcaneYield5, file="../data/mangoBrix.RData")

##----------------------------------------------------------------------
## Examples.
library(lattice)
library(latticeExtra)

data(mangoBrix)
str(mangoBrix)
load("../data/mangoBrix.RData")

xyplot(brix~variety|direction, data=mangoBrix, jitter.x=TRUE, 
       xlab="Variety", main="Mango Brix", 
       col="tomato", type=c("p","a"))

m <- lm(brix~variety+direction, data=mangoBrix)
## Departures from homecedasticity and normality.
par(mfrow=c(2,2)); plot(m); layout(1)

rm(list=ls())
load("../data/mangoBrix.RData")
ls()
str(mangoBrix)

